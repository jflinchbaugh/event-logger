(ns event-logger.core
  (:require [helix.core :refer [defnc $ <>]]
            [helix.hooks :as hooks]
            [taoensso.telemere :as tel]
            [event-logger.localstorage :as ls]
            [helix.dom :as d]
            [tick.core :as t]
            [cljs.pprint :as pp]
            [clojure.string :as str]
            [cljs-http.client :as http]
            [clojure.edn :as edn]
            [cljs.core.async :refer [<! go]]
            [goog.string :refer [format]])
  (:require-macros [event-logger.build-info :as build-info]))

(def response-display-ms 3000)

(def build-date (build-info/build-date))

;; date utilities
(defn- format-date-time [dt]
  (t/format :iso-local-date-time dt))

(defn now-str
  "produce a string for the datetime now to the second"
  []
  (-> (t/date-time) (t/truncate :seconds) format-date-time))

(defn now-str-ms
  "produce a string for the datetime now to the ms"
  []
  (-> (t/date-time) format-date-time))

(defn normalize-date-str
  "parse, truncate, and reformat a date string"
  [event-str]
  (->
   event-str
   t/date-time
   (t/truncate :seconds)
   format-date-time))

(defn get-event-time
  "read date-time of events in old format (date string only)
   or new format (as a map with a :date-time)"
  [event]
  (if (map? event) (:date-time event) event))

(defn normalize-event
  [event]
  (let [t (get-event-time event)
        l (when (map? event) (:note event))]
    {:date-time (normalize-date-str t)
     :note l}))

(defn describe-diff [diff]
  (let [days (t/days diff)
        hours (t/hours diff)
        minutes (t/minutes diff)
        seconds (t/seconds diff)]
    (cond
      (> 1 seconds) "0 seconds"
      (> 2 seconds) "1 second"
      (>= 120 seconds) (str seconds " seconds")
      (>= 120 minutes) (str minutes " minutes")
      (>= 48 hours) (str hours " hours")
      :else (str days " days"))))

(defn average-duration [events]
  (if (> 2 (count events))
    nil
    (->
     (t/between
      (t/date-time
       (get-event-time (first events)))
      (t/date-time
       (get-event-time (last events))))
     t/seconds
     (/ (dec (count events)))
     int
     (t/new-duration :seconds)
     describe-diff)))

;; storage utilities

(defn storage->edn
  [k]
  (let [v (-> k ls/get-item edn/read-string)]
    (if (= "null" v) nil v)))

(defn read-local-storage
  []
  (let [categories-log (-> :categories-log storage->edn vec)
        categories (-> :categories storage->edn vec)
        migrated-categories (mapv
                             (fn [cat]
                               (update
                                cat
                                :events
                                (fn [events]
                                  (mapv
                                   #(if (map? %)
                                      (merge {:note ""} (select-keys % [:date-time :note]))
                                      {:date-time % :note ""})
                                   events))))
                             categories)
        config (storage->edn :config)]
    {:categories-log categories-log
     :categories migrated-categories
     :config config
     :new-config config}))

(defn write-local-storage!
  [version config categories categories-log]
  (tel/log!
   :info
   {:write-local-storage
    {:version version
     :config config
     :categories categories
     :categories-log categories-log}})

  (ls/set-item! :version version)
  (ls/set-item! :config config)
  (ls/set-item! :categories categories)
  (ls/set-item! :categories-log categories-log))

(defn log-category-change! [set-state change-type category-data]
  (set-state update :categories-log
             conj {:timestamp (now-str-ms)
                   :type change-type
                   :data category-data}))

(defn configured?
  [resource user password]
  (and
   (not (str/blank? resource))
   (not (str/blank? user))
   (not (str/blank? password))))

(defn obfuscate [config-key state]
  (if (get-in state [config-key :password])
    (-> state
        (update-in [config-key :password]
                   (constantly "****")))
    state))

;; http actions

(defn upload!
  [force state set-state]
  (when-not (:network-response state)
    (let [config (:config state)
          {:keys [resource user password]} config]
      (when (or force (configured? resource user password))
        (tel/log! {:level :info :msg "uploading" :data config})
        (set-state assoc :network-action "Upload")
        (go
          (let [response (->
                          resource
                          (http/post
                           {:with-credentials? false
                            :basic-auth {:username user
                                         :password password}
                            :content-type :text/plain
                            :body (str
                                   {:date (now-str)
                                    :categories (:categories state)})})
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
                  (assoc-in
                   m
                   [:network-response :success] false)
                  [:network-response :error-text]
                  "Failed to upload! Check resource config."))))))))))

(defn download!
  [config set-state]
  (tel/log! :info "downloading")
  (set-state assoc :network-action "Download")
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
  (let [time (get-event-time event)]
    (empty? (filter #(= (get-event-time %) time) existing-events))))

(defn adding-event?
  "is there an event being added?"
  [state]
  (-> state :adding-event nil? not))

(defn event-expanded? [state item event]
  (=
   {:id (:id item) :event (:date-time event)}
   (get-confirm state :delete-event)))

(defn add-durations [events]
  (->>
   events
   (partition 2 1)
   (mapv
    (fn [[a b]]
      (let [t-a (get-event-time a)
            t-b (get-event-time b)
            base-map (if (map? b) b {:date-time b})]
        (assoc base-map
               :duration (describe-diff
                          (t/between
                           (t/date-time t-a)
                           (t/date-time t-b)))))))
   (cons (let [f (first events)]
           (if (map? f)
             (assoc f :duration nil)
             {:date-time f :duration nil})))))

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
        (do
          (log-category-change!
           set-state
           :add-category
           {:id new-cat-id :name new-cat-name})
          (set-state update :categories
                     conj {:id new-cat-id :name new-cat-name}))))))

(defn delete-category! [state set-state item-id]
  (log-category-change!
   set-state
   :delete-category
   {:id item-id})
  (set-state update :categories
             (comp vec (partial remove (comp #{item-id} :id))))
  (open-category! state set-state nil))

(defn remove-at-index [lst index]
  (concat (take index lst) (drop (inc index) lst)))

(defn insert-at-index [lst index element]
  (concat (take index lst) (list element) (drop index lst)))

(defn move [lst from to]
  (let [item (nth lst from)]
    (->
     lst
     (remove-at-index from)
     (insert-at-index to item)
     vec)))

(defn move-category [state from to]
  (update-in state [:categories] move from to))

;; event actions

(defn add-event! [state set-state id]
  (clear-confirms! set-state)
  (if (adding-event? state)
    (let [time (:adding-event state)
          note (:adding-note state)
          idx (.indexOf (map :id (:categories state)) id)
          event {:date-time time :note note}
          existing-events (get-in state [:categories idx :events])]
      (when (is-new-event? existing-events time)
        (log-category-change!
         set-state
         :add-event
         {:category-id id :event event})
        (set-state
         (fn [s]
           (-> s
               (update-in [:categories idx :events] conj event)
               (dissoc :adding-event :adding-note))))))
    (do
      (when (not= (:display-category state) id)
        (open-category! state set-state id))
      (set-state assoc :adding-event (now-str) :adding-note ""))))

(defn open-delete-event! [state set-state id event]
  (let [new-confirmation {:id id :event (:date-time event)}]
    (set-confirm!
     set-state
     :delete-event
     (when
      (not= new-confirmation (get-confirm state :delete-event))
       new-confirmation))))

(defn delete-event! [state set-state event]
  (log-category-change!
   set-state
   :delete-event
   {:category-id (:id (get-confirm state :delete-event))
    :event event})
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
           (fn [events]
             (remove
              #(= (get-event-time %) (:date-time event))
              events)))))
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

(defnc average-component [{:keys [category]}]
  (->>
    category
    :events
    (sort-by :date-time)
    average-duration
    (str "Avg: ")
    (d/div {:class "average-duration"})))

(defnc since-component [{:keys [category]}]
  (let [sorted-events (->> category :events (sort-by :date-time))
        last-event (last sorted-events)
        _ (tel/log! :info {:last-event last-event}) #_(tel/log! :info {:sorted-events sorted-events})
        [now set-now] (hooks/use-state (t/date-time))]
    (js/setTimeout (partial set-now (t/date-time)) 1000)
    (when last-event
      (d/div
       {:class "time-since"}
       (->
        last-event
        get-event-time
        t/date-time
        (t/between now)
        describe-diff
        (->> (format "(%s ago)")))))))

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

(defnc event-details
  [{:keys [event expanded-fn? expand-action delete-action]}]
  (d/li
   {:class "event"
    :on-click (partial expand-action event)}
   (d/span {:class "date-time"} (:date-time event))
   (when (not (str/blank? (:note event)))
     (d/span {:class "note"} (:note event)))
   (when
    (expanded-fn? event)
     (d/span
      {:class "actions"}
      " "
      (d/button
       {:class "delete"
        :on-click (partial delete-action event)}
       "X")))
   (when (:duration event)
     (d/div {:class "duration"} (str "(" (:duration event) ")")))))

(defnc category-details [{:keys [set-state state item]}]
  (d/div
   {:class "details" :id (str "details-" (:id item))}
   (d/div {:class "event-header"}
          ($ since-component {:category item})
          ($ average-component {:category item}))
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
      (d/input
       {:class "new-event-note"
        :type "text"
        :placeholder "Note"
        :value (:adding-note state)
        :on-change #(set-state
                     assoc
                     :adding-note
                     (.. % -target -value))})
      ($ add-button
         {:state state
          :set-state set-state
          :item item
          :display "Save"})))
   (d/ul
    {:class "events"}
    (let [events (->> item
                      :events
                      (map normalize-event)
                      (sort-by :date-time)
                      add-durations)]
      (doall
       (for [event (reverse events)]
         ($ event-details
            {:key (str (:id item) "-" (:date-time event))
             :event event
             :expanded-fn? (partial
                            event-expanded?
                            state
                            item)
             :expand-action (partial
                             open-delete-event!
                             state
                             set-state
                             (:id item))
             :delete-action (partial
                             delete-event!
                             state
                             set-state
                             event)}))))
    ($ category-controls
       {:state state :set-state set-state :item-id (:id item)}))))

(defnc categories [{:keys [state set-state]}]
  (let [categories (:categories state)
        drag-item (hooks/use-ref nil)
        drag-over-item (hooks/use-ref nil)
        handle-drag-start (fn [e position]
                            (reset! drag-item position)
                            (set!
                             (.. e -dataTransfer -effectAllowed)
                             "move")
                            (.. e -dataTransfer
                                (setData "text/html" (.. e -target)))
                            (.. e -target -classList (add "dragging")))
        handle-drag-enter (fn [_ position]
                            (reset! drag-over-item position))

        handle-drag-end (fn [e]
                          (when (not= @drag-item @drag-over-item)
                            (log-category-change!
                             set-state
                             :move-category
                             {:from (get-in state [:categories @drag-item :id])
                              :to (get-in state [:categories @drag-over-item :id])})
                            (.. e -target -classList (remove "dragging"))
                            (set-state move-category
                                       @drag-item
                                       @drag-over-item))
                          (reset! drag-item nil)
                          (reset! drag-over-item nil))]

    (d/ul
     (doall
      (for [[index item] (map-indexed (fn [i v] [i v]) categories)]
        (d/li
         {:key (:id item)
          :draggable true
          :on-drag-start (fn [e] (handle-drag-start e index))
          :on-drag-enter (fn [e] (handle-drag-enter e index))
          :on-drag-end handle-drag-end
          :on-drag-over (fn [e] (.. e preventDefault))}
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

(defnc network-response-display [{:keys [state set-state]}]
  (let [network-response (get state :network-response)
        network-action (get state :network-action)]
    (hooks/use-effect
     [network-response network-action]
     (when (and network-response network-action)
       (js/setTimeout
        (fn []
          (set-state assoc :network-response nil)
          (set-state assoc :network-action nil))
        response-display-ms)))
    (when (and network-response network-action)
      (d/div
       {:class "toast-container"}
       (d/div
        {:class (str "toast " (if (get-in state [:network-response :success]) "success" "error"))
         :on-click #(set-state assoc :network-response nil)}
        (if (get-in state [:network-response :success])
          (str (:network-action state) " succeeded!")
          (str
           (:network-action state)
           " failed: "
           (get-in state [:network-response :error-text]))))))))

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
           :on-click (partial
                      upload!
                      true
                      state
                      set-state)}
          "Upload"))
        (d/div
         {:class "row"}
         (d/button
          {:class "download"
           :on-click (fn []
                       (download! (:config state) set-state))}
          "Download"))
        (d/div
         {:class "row"}
         (d/button
          {:class "clear-log"
           :on-click (fn [] (set-state assoc :categories-log []))}
          "Clear Log"))
        (d/pre
         (->>
          state
          (obfuscate :config)
          (obfuscate :new-config)
          pp/pprint
          with-out-str))
        (d/div
         {:class "row"}
         (d/div "Build Date: " build-date)))))))

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
                                           [:new-config :resource]
                                           (.. e -target -value)))
                             :value (get-in state [:new-config :resource])}))
                    (d/div {:class "row"}
                           (d/label {:for :user} "User")
                           (d/input
                            {:name :user
                             :id :user
                             :on-change (fn [e]
                                          (set-state
                                           assoc-in
                                           [:new-config :user]
                                           (.. e -target -value)))
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
                                           [:new-config :password]
                                           (.. e -target -value)))
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

(defnc title-bar []
  (d/div {:class "title-bar"}
         (d/h1 "Event Logger")))

(defnc app []
  (let [local-data (read-local-storage)
        [state set-state] (hooks/use-state
                           (assoc
                            local-data
                            :new-category ""))
        [last-upload set-last-upload] (hooks/use-state
                                       (select-keys
                                        local-data
                                        [:categories]))]
    ;; update local storage
    (hooks/use-effect
     [state]
     (write-local-storage!
      "2"
      (:config state)
      (:categories state)
      (:categories-log state)))

    ;; upload changes
    (hooks/use-effect
     [state]
     (when-not
      (= (:categories state) (:categories last-upload))
       (upload! false state set-state)
       (set-last-upload assoc :categories (:categories state))))

    (<>
     ($ title-bar)
     (d/div
      {:class "wrapper"
       :key "div.wrapper"}
      ($ categories {:state state :set-state set-state})
      ($ add-category-form {:state state :set-state set-state})
      ($ config {:state state :set-state set-state})
      ($ debugger {:state state :set-state set-state})
      ($ network-response-display {:state state :set-state set-state})))))
