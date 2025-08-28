(ns event-logger.core
  (:require [helix.core :refer [defnc $ <>]]
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
            [cljs.core.async :refer [<! go]])
  (:require-macros [event-logger.build-info :as build-info]))

(def build-date (build-info/build-date))

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
       (first events))
      (t/date-time
       (last events)))
     t/seconds
     (/ (dec (count events)))
     int
     (t/new-duration :seconds)
     describe-diff)))

(defn configured?
  [resource user password]
  (and
   (not (str/blank? resource))
   (not (str/blank? user))
   (not (str/blank? password))))

;; http actions

(defn upload!
  [force config categories set-state]
  (tel/log! {:level :info :msg "uploading" :data config})
  (set-state assoc :network-action "Upload")
  (let [{:keys [resource user password]} config]
    (when (or force (configured? resource user password))
      (go
        (let [response (->
                        resource
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
                "Failed to upload! Check resource config.")))))))))

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
  (empty? (filter (partial = event) existing-events)))

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
      {:date-time b
       :duration (describe-diff
                  (t/between
                   (t/date-time a)
                   (t/date-time b)))}))
   (cons {:date-time (first events)
          :duration nil})))

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
        (set-state update :categories
                   conj {:id new-cat-id :name new-cat-name})))))

(defn delete-category! [state set-state item-id]
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
      (insert-at-index to item))))

(defn move-category [state from to]
  (update-in state [:categories] move from to))

;; event actions

(defn add-event! [state set-state id]
  (clear-confirms! set-state)
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

(defn open-delete-event! [state set-state id event]
  (let [new-confirmation {:id id :event (:date-time event)}]
    (set-confirm!
     set-state
     :delete-event
     (when
      (not= new-confirmation (get-confirm state :delete-event))
       new-confirmation))))

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
           (fn [events] (remove #{(:date-time event)} events)))))
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
   sort
   average-duration
   (str "Avg: ")
   (d/div {:class "average-duration"})))

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
        describe-diff
        (str " ago"))))))

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
          ($ average-component {:category item})
          ($ since-component {:category item}))
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
    (let [events (->> item
                      :events
                      (map normalize-date-str)
                      sort
                      add-durations)]
      (doall
       (for [event (reverse events)]
         ($ event-details
            {:key (str (:id item) "-" event)
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



(comment
  "
  const handleDragEnd = (e) => {
                                e.target.classList.remove('dragging');
                                const newItems = [...items];
                                const draggedItemContent = newItems[dragItem.current];
                                newItems.splice(dragItem.current, 1);
                                newItems.splice(dragOverItem.current, 0, draggedItemContent);
                                dragItem.current = null;
                                dragOverItem.current = null;
                                setItems(newItems);
                                };
")

(defnc categories [{:keys [state set-state]}]
  (let [categories (:categories state)
        drag-item (hooks/use-ref nil)
        drag-over-item (hooks/use-ref nil)
        handle-drag-start (fn [e position]
                            (reset! drag-item position)
                            (set! (.. e -dataTransfer -effectAllowed) "move")
                            (.. e -dataTransfer (setData "text/html" (.. e -target)))
                            (.. e -target -classList (add "dragging")))
        handle-drag-enter (fn [e position]
                            (reset! drag-over-item position))

        handle-drag-end (fn [e]
                          (.. e -target -classList (remove #{"dragging"}))
                          ;; alter items
                          (set-state move-category
                            @drag-item
                            @drag-over-item)
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
          :on-drag-over (fn [e] (.. e preventDefault))
          }
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
  (let [network-response (get state :network-response)]
    (hooks/use-effect
     [network-response]
     (when network-response
       (js/setTimeout
        (partial set-state assoc :network-response nil)
        3000))) ; Keep the timeout
    (when network-response
      (d/div
       {:class "modal-overlay"
        :on-click #(set-state assoc :network-response nil)} ; Add click handler
       (d/div
        {:class "modal-content"}
        (if (get-in state [:network-response :success])
          (d/div
           {:class "response success"}
           (str (:network-action state) " succeeded!"))
          (d/div {:class "response error"}
                 (str
                  (:network-action state)
                  " failed: "
                  (get-in state [:network-response :error-text])))))))))

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
                      (:new-config state)
                      (:categories state)
                      set-state)}
          "Upload"))
        (d/div
         {:class "row"}
         (d/button
          {:class "download"
           :on-click (fn []
                       (download! (:new-config state) set-state))}
          "Download"))
        (d/pre
         (with-out-str (pp/pprint state)))
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
       (upload! false (:config state) (:categories state) set-state)
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
