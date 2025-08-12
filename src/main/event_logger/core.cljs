(ns event-logger.core
  (:require [helix.core :refer [defnc $]]
            [helix.hooks :as hooks]
            [taoensso.telemere :as tel]
            [event-logger.localstorage :as ls]
            [cognitect.transit :as transit]
            [helix.dom :as d]
            [tick.core :as t]
            [cljs.pprint :as pp]
            [clojure.string :as str]
            [cljs-http.client :as http]
            [clojure.edn :as edn]
            [cljs.core.async :refer [<! go]]))

;; storage utilities
(defn json->clj [x]
  (transit/read (transit/reader :json) x))

(defn clean-storage
  [v]
  (when (seqable? v) v))

(defn storage->edn
  [k]
  (-> k ls/get-item edn/read-string clean-storage))

(defn read-local-storage
  []
  (let [categories (-> :categories storage->edn vec)
        config (storage->edn :config)
        old (->
              "[\"~#'\",\"~:event-logger\"]"
              ls/get-item
              json->clj
              clean-storage)]
    {:categories (if (seq categories) categories (:categories old))
     :config config
     :new-config config}))

(defn write-local-storage!
  [version config categories]
  (ls/set-item! :version version)
  (ls/set-item! :config config)
  (ls/set-item! :categories categories))

;; date utilities
(defn format-date-time [dt]
  (t/format :iso-local-date-time dt))

(defn now-str
  "produce a string for the datetime now"
  []
  (-> (t/date-time) (t/truncate :seconds) format-date-time))

(defn normalize-date-str
  "parse, truncate, and reformat a date string"
  [event-str]
  (->
   event-str
   t/date-time
   (t/truncate :seconds)
   format-date-time))

(defn describe-diff [diff]
  (let [days (t/days diff)
        hours (t/hours diff)
        minutes (t/minutes diff)
        seconds (t/seconds diff)]
    (cond
      (> 1 seconds) "0 seconds ago"
      (> 2 seconds) "1 second ago"
      (> 1 minutes) (str seconds " seconds ago")
      (> 2 minutes) "1 minute ago"
      (> 1 hours) (str minutes " minutes ago")
      (> 2 hours) "1 hour ago"
      (> 1 days) (str hours " hours ago")
      (> 2 days) "1 day ago"
      :else (str days " days ago"))))

;; http actions

(defn upload!
  [config categories set-state]
  (tel/log! :info "uploading")
  (go
    (let [{:keys [resource user password]} config
          response
          (-> resource
              (http/post
               {:with-credentials? false
                :basic-auth {:username user
                             :password password}
                :content-type :text/plain
                :body (str {:date (now-str) :categories categories})})
              <!)
          edn-response (-> response
                           :body
                           edn/read-string)]
      (set-state
       assoc
       :network-response
       response)
      (when
       (and (:success response) (not (:categories edn-response)))
        (set-state
         (fn [m]
           (assoc-in
            (assoc-in m
                      [:network-response :success] false)
            [:network-response :error-text]
            "Failed to upload! Check resource config.")))))))

(defn download!
  [config set-state]
  (tel/log! :info "downloading")
  (go
    (let [{:keys [resource user password]} config
          response
          (-> resource
              (http/get
               {:with-credentials? false
                :basic-auth {:username user
                             :password password}
                :content-type :text/plain})
              <!)
          edn-response (-> response
                           :body
                           edn/read-string)]
      (set-state assoc :network-response response)
      (when (:success response)
        (if (:categories edn-response)
          (set-state
           assoc
           :categories (:categories edn-response))
          (set-state
           (fn [m]
             (assoc-in
              (assoc-in m
                        [:network-response :success] false)
              [:network-response :error-text]
              "Failed to download! Check resource config."))))))))

;; confirmations utils

(defn clear-confirms!
  "clear all confirmations as you nav through the app"
  [set-state]
  (set-state dissoc :confirm))

(defn set-confirm!
  "set a confirmation state"
  [set-state name data]
  (set-state assoc-in [:confirm name] data))

(defn get-confirm
  "get a confirmation state"
  [state name]
  (get-in state [:confirm name]))

;; event utils

(defn is-new-event?
  "is this event new, and not already in the list?"
  [existing-events event]
  (empty? (filter (partial = event) existing-events)))

(defn adding-event?
  "is there an event being added?"
  [state]
  (-> state :adding-event nil? not))

;; category actions

(defn open-category! [state set-state item-id]
  (clear-confirms! set-state)
  (set-state dissoc :adding-event)
  (if (= item-id (:display-category state))
    (set-state dissoc :display-category)
    (set-state assoc :display-category item-id)))

(defn add-category! [state set-state]
  (let [new-cat-name (str/replace
                      (->> state :new-category str/trim)
                      #" +" " ")
        new-cat-id (str/replace
                    (str/lower-case new-cat-name)
                    #" " "-")
        existing-categories (set (map :id (:categories state)))]
    (when (not (str/blank? new-cat-name))
      (if (existing-categories new-cat-id)
        (open-category! state set-state new-cat-id)
        (set-state
         update :categories
         conj {:id new-cat-id :name new-cat-name})))))

(defn delete-category! [state set-state item-id]
  (set-state update :categories
             (comp vec (partial remove (comp #{item-id} :id))))
  (open-category! state set-state nil))

;; event actions

(defn add-event! [state set-state id]
  (if (adding-event? state)
    (let [time (:adding-event state)
          idx (.indexOf (map :id (:categories state)) id)
          existing-events (get-in state [:categories idx :events])]
      (when (is-new-event? existing-events time)
        (set-state update-in [:categories idx :events] conj time)
        (set-state dissoc :adding-event)))
    (do
      (when (not= (:display-category state) id)
        (open-category! state set-state id))
      (set-state assoc :adding-event (now-str)))))

(defn open-delete-event! [set-state id event]
  (set-confirm! set-state :delete-event {:id id :event event}))

(defn delete-event! [state set-state event]
  (set-state
   update
   :categories
   (fn [cats]
     (mapv
      (fn [cat]
        (if (not= (:id (get-confirm state :delete-event)) (:id cat))
          cat
          (update
           cat
           :events
           (fn [events] (remove #{event} events)))))
      cats)))
  (clear-confirms! set-state))

(defn save-config!
  [state set-state]
  (tel/log! :info "saving config")
  (set-state
   assoc
   :config
   (select-keys
    (:new-config state)
    [:resource :user :password])))

;; define components using the `defnc` macro

(defnc since-component [{:keys [category]}]
  (let [last-event (-> category :events sort last)
        [now set-now] (hooks/use-state (t/date-time))]
    (js/setTimeout (partial set-now (t/date-time)) 1000)
    (when last-event
      (d/div
       {:class "time-since"}
       (->
        last-event
        t/date-time
        (t/between now)
        describe-diff)))))

(defnc add-button [{:keys [state set-state item display]}]
  (d/button
   {:on-click (partial add-event! state set-state (:id item))}
   display))

(defnc category-controls [{:keys [set-state state item-id]}]
  (d/div
   {:class "controls"}
   (if (= (get-confirm state :delete-category) item-id)
     (d/button
      {:class "delete"
       :on-click (partial delete-category! state set-state item-id)}
      "Really?")
     (d/button
      {:class "delete"
       :on-click (partial set-confirm! set-state :delete-category item-id)}
      "X"))))

(defnc category-details [{:keys [set-state state item]}]
  (d/div {:class "details" :id (str "details-" (:id item))}
         ($ since-component {:category item})
         (when (:adding-event state)
           (d/div
            (d/input
             {:class "new-event"
              :type "datetime-local"
              :enterKeyHint "done"
              :value (:adding-event state)
              :name :new-event
              :on-change #(set-state
                           assoc
                           :adding-event
                           (.. % -target -value))})
            ($ add-button
               {:state state
                :set-state set-state
                :item item
                :display "Save"})))
         (d/ul
          {:class "events"}
          (doall
           (for [event (->> item
                            :events
                            (map normalize-date-str)
                            sort
                            reverse)]
             (d/li
              {:key (str (:id item) "-" event)}
              (d/span
               {:class "event"
                :on-click (partial
                           open-delete-event!
                           set-state
                           (:id item)
                           event)}
               event)
              (when
               (=
                {:id (:id item) :event event}
                (get-confirm state :delete-event))
                (d/button
                 {:class "delete"
                  :on-click (partial delete-event! state set-state event)}
                 "X")))))
          ($ category-controls
             {:state state :set-state set-state :item-id (:id item)}))))

(defnc categories [{:keys [state set-state]}]
  (let [categories (:categories state)]
    (d/ul
     (doall
      (for [item categories]
        (d/li
         {:key (:id item)}
         ($ add-button
            {:state state
             :set-state set-state
             :item item
             :display "+"})
         (d/span
          {:class "category"
           :on-click (partial open-category! state set-state (:id item))}
          (:name item))
         (when (= (:id item) (:display-category state))
           ($ category-details
              {:set-state set-state :state state :item item}))))))))

(defnc network-response-display [{:keys [state]}]
  (when (or (get-in state [:network-response :success])
            (get-in state [:network-response :error-text]))
    (d/div
     {:class "row"}
     (if (get-in state [:network-response :success])
       (d/div {:class "response success"} "Uploaded!")
       (d/div {:class "response error"}
         (str "Failed Upload: "(get-in state [:network-response :error-text])))))))

(defnc debugger [{:keys [state set-state]}]
  (let [[show? set-show] (hooks/use-state false)]
    (d/div
     {:id "debug"}
     (d/button
      {:class "debug"
       :on-click (fn [] (set-show not))}
      "Debug")
     (when show?
       (d/div
        {:class "wrapper"}
        (d/div
         {:class "row"}
         (d/button
          {:class "reload"
           :on-click (fn [] (.reload js/location))}
          "Reload"))
        (d/div
         {:class "row"}
         (d/button
          {:class "upload"
           :on-click (fn []
                       (upload! (:new-config state) (:categories state) set-state))}
          "Upload"))
        (d/div
         {:class "row"}
         (d/button
          {:class "download"
           :on-click (fn []
                       (download! (:new-config state) set-state))}
          "Download"))
        (d/pre
         (with-out-str (pp/pprint state))))))))

(defnc config [{:keys [state set-state]}]
  (let [[show? set-show] (hooks/use-state false)]
    (d/div {:id "config"}
           (d/button
            {:class "config"
             :on-click (fn [] (set-show not))}
            "Config")
           (when show?
             (d/div {:class "config wrapper"}
                    (d/div {:class "row"}
                           (d/label {:for :resource} "Resource")
                           (d/input
                            {:name :resource
                             :id :resource
                             :on-change (fn [e]
                                          (set-state
                                           assoc-in
                                           [:new-config :resource] (.. e -target -value)))
                             :value (get-in state [:new-config :resource])}))
                    (d/div {:class "row"}
                           (d/label {:for :user} "User")
                           (d/input
                            {:name :user
                             :id :user
                             :on-change (fn [e]
                                          (set-state
                                           assoc-in
                                           [:new-config :user] (.. e -target -value)))
                             :value (get-in state [:new-config :user])}))
                    (d/div {:class "row"}
                           (d/label {:for :password} "Password")
                           (d/input
                            {:name :password
                             :id :password
                             :type "password"
                             :on-change (fn [e]
                                          (set-state
                                           assoc-in
                                           [:new-config :password] (.. e -target -value)))
                             :value (get-in state [:new-config :password])}))
                    (d/div {:class "row"}
                           (d/button
                            {:class "save"
                             :on-click (fn []
                                         (save-config! state set-state))}
                            "Save Config")))))))

(defnc add-category-form [{:keys [state set-state]}]
  (d/div
   {:class "add"}
   (d/input
    {:class "new-category"
     :key :new-category
     :type "text"
     :name :new-category
     :enterKeyHint "done"
     :placeholder "New Category"
     :value (:new-category state)
     :on-change (fn [e]
                  (set-state
                   assoc
                   :new-category (.. e -target -value)))
     :on-key-down (fn [e]
                    (when (== 13 (.-which e))
                      (add-category! state set-state)
                      (set-state assoc :new-category "")))})
   (d/button
    {:class "add"
     :on-click (fn []
                 (add-category! state set-state)
                 (set-state assoc :new-category ""))}
    "Add")))

(defnc app []
  (let [local-data (read-local-storage)
        [state set-state] (hooks/use-state
                           (assoc
                            local-data
                            :new-category ""))
        [last-upload set-last-upload] (hooks/use-state
                                       (select-keys local-data [:categories]))]
    ;; update local storage
    (hooks/use-effect
     [state]
     (write-local-storage! "1" (:config state) (:categories state)))

    ;; upload changes!
    (hooks/use-effect
     [state]
     (when-not
      (= (:categories state) (:categories last-upload))
       (upload! (:config state) (:categories state) set-state)
       (set-last-upload assoc :categories (:categories state))))

    (d/div
     {:class "wrapper"}
     ($ network-response-display {:state state})
     ($ categories {:state state :set-state set-state})
     ($ add-category-form {:state state :set-state set-state})
     ($ config {:state state :set-state set-state})
     ($ debugger {:state state :set-state set-state}))))
