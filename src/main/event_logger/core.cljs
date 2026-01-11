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
  (:import [goog.events KeyCodes])
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
  "read date-time of events"
  [event]
  (normalize-date-str (:date-time event)))

(defn normalize-event
  [event]
  (let [t (get-event-time event)
        l (:note event)]
    {:note l
     :date-time (normalize-date-str t)}))

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
                                      (merge
                                       {:note ""}
                                       (select-keys % [:date-time :note]))
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
  #_(tel/log!
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
  "is the app configured and able to upload/download data?"
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
                                    :categories (:categories state)
                                    :categories-log (:categories-log state)})})
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
            t-b (get-event-time b)]
        (assoc b
               :duration (describe-diff
                          (t/between
                           (t/date-time t-a)
                           (t/date-time t-b)))))))
   (cons (assoc (first events) :duration nil))))

;; category actions

(defn open-category! [state set-state item-id]
  (clear-confirms! set-state)
  (set-state
   (fn [s]
     (let [s (dissoc s :adding-event :adding-note :editing-event)]
       (if (= item-id (:display-category s))
         (dissoc s :display-category)
         (assoc s :display-category item-id))))))

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
        (log-category-change!
         set-state
         :add-category
         {:id new-cat-id :name new-cat-name})))))

(defn delete-category! [state set-state item-id]
  (log-category-change!
   set-state
   :delete-category
   {:id item-id})
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

(defn apply-log-entry [categories {:keys [type data]}]
  (case type
    :add-category
    (conj categories {:id (:id data) :name (:name data)})

    :delete-category
    (vec (remove #(= (:id %) (:id data)) categories))

    :move-category
    (move categories (:from-index data) (:to-index data))

    :add-event
    (let [{:keys [category-id event]} data
          idx (first (keep-indexed #(when (= (:id %2) category-id) %1) categories))]
      (if idx
        (update-in categories [idx :events]
                   (fn [events]
                     (map normalize-event
                          (reverse
                           (sort-by :date-time
                                    (conj events event))))))
        categories))

    :delete-event
    (let [{:keys [category-id event]} data
          idx (first (keep-indexed #(when (= (:id %2) category-id) %1) categories))]
      (if idx
        (update-in categories [idx :events]
                   (fn [events]
                     (remove
                      #(= (get-event-time %) (get-event-time event))
                      events)))
        categories))

    categories))

;; event actions

(defn add-event! [state set-state id]
  (clear-confirms! set-state)
  (if (adding-event? state)
    (let [time (normalize-date-str (:adding-event state))
          note (str/trim (or (:adding-note state) ""))
          idx (.indexOf (map :id (:categories state)) id)
          event {:note note :date-time time}
          existing-events (get-in state [:categories idx :events])]
      (when (is-new-event? existing-events event)
        (log-category-change!
         set-state
         :add-event
         {:category-id id :event event})
        (set-state
         (fn [s]
           (-> s
               (dissoc :adding-event :adding-note :editing-event))))))
    (do
      (when (not= (:display-category state) id)
        (open-category! state set-state id))
      (set-state
       (fn [s]
         (-> s
             (assoc :adding-event (now-str) :adding-note "")
             (dissoc :editing-event)))))))

(defn open-delete-event! [state set-state id event]
  (let [new-confirmation {:id id :event (:date-time event)}]
    (set-state
     (fn [s]
       (-> s
           (assoc-in [:confirm :delete-event]
                     (when
                      (not= new-confirmation (get-confirm s :delete-event))
                       new-confirmation))
           (dissoc :adding-event :adding-note :editing-event))))))

(defn delete-event! [state set-state event]
  (log-category-change!
   set-state
   :delete-event
   {:category-id (:id (get-confirm state :delete-event))
    :event event})
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

(defn editing-event? [state category-id event]
  (let [editing (:editing-event state)]
    (and (= (:category-id editing) category-id)
         (= (:original-event editing) event))))

(defn start-editing-event! [state set-state category-id event]
  (set-state
   (fn [s]
     (-> s
         (assoc :editing-event {:category-id category-id
                                :original-event event
                                :time (:date-time event)
                                :note (:note event)})
         (dissoc :adding-event :adding-note :confirm)))))

(defn cancel-edit! [set-state]
  (set-state dissoc :editing-event))

(defn cancel-add-event! [set-state]
  (set-state dissoc :adding-event :adding-note))

(defn save-edited-event! [state set-state]
  (let [{:keys [category-id original-event time note]} (:editing-event state)
        note (str/trim (or note ""))
        new-event {:date-time (normalize-date-str time) :note note}]
    (log-category-change!
     set-state :delete-event
     {:category-id category-id :event original-event})
    (log-category-change!
     set-state :add-event
     {:category-id category-id :event new-event})
    (set-state dissoc :editing-event)))

(defn enter-key? [e]
  (== KeyCodes/ENTER (.-which e)))

(defn esc-key? [e]
  (== KeyCodes/ESC (.-which e)))

(defn handle-keydown [on-enter on-esc e]
  (cond
    (enter-key? e) (on-enter)
    (esc-key? e) (on-esc)))

(defn handle-edit-keydown [state set-state e]
  (handle-keydown
   #(save-edited-event! state set-state)
   #(cancel-edit! set-state)
   e))

(defn handle-add-keydown [state set-state item-id e]
  (handle-keydown
   #(add-event! state set-state item-id)
   #(cancel-add-event! set-state)
   e))

(defn handle-add-category-keydown [state set-state e]
  (handle-keydown
   #(do (add-category! state set-state)
        (set-state assoc :new-category ""))
   #(set-state assoc :new-category "")
   e))

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
  [{:keys [event
           expanded-fn?
           expand-action
           delete-action
           state
           set-state
           category-id]}]
  (let [editing? (editing-event? state category-id event)]
    (if editing?
      (d/li
       {:class "event editing"}
       (d/div
        (d/input
         {:class "new-event"
          :type "datetime-local"
          :step "1"
          :value (get-in state [:editing-event :time])
          :on-change #(set-state
                       assoc-in
                       [:editing-event :time]
                       (.. % -target -value))
          :on-key-down (partial handle-edit-keydown state set-state)})
        (d/input
         {:class "new-event-note"
          :type "text"
          :value (get-in state [:editing-event :note])
          :on-change #(set-state
                       assoc-in
                       [:editing-event :note]
                       (.. % -target -value))
          :on-key-down (partial handle-edit-keydown state set-state)})
        (d/div
         {:class "edit-actions"}
         (d/button
          {:class "save"
           :on-click #(save-edited-event! state set-state)}
          "Save")
         (d/button
          {:class "cancel"
           :on-click #(cancel-edit! set-state)}
          "Cancel"))))
      (d/li
       {:class "event"
        :on-click (partial expand-action event)}
       (d/div
        {:class "event-header"}
        (d/span {:class "date-time"} (:date-time event))
        (when
         (expanded-fn? event)
          (d/span
           {:class "actions"}
           (d/button
            {:class "delete"
             :on-click (partial delete-action event)}
            "X")
           (d/button
            {:class "edit"
             :on-click (fn [e]
                         (.stopPropagation e)
                         (start-editing-event!
                          state
                          set-state
                          category-id
                          event))}
            "Edit"))))
       (when (not (str/blank? (:note event)))
         (d/div {:class "note"} (:note event)))
       (when (:duration event)
         (d/div {:class "duration"} (str "(" (:duration event) ")")))))))

(defnc category-details [{:keys [set-state state item]}]
  (let [adding? (:adding-event state)]
    (d/div
     {:class "details" :id (str "details-" (:id item))}
     (d/div {:class "event-header"}
            ($ since-component {:category item})
            ($ average-component {:category item}))
     (when adding?
       (d/div
        (d/input
         {:class "new-event"
          :type "datetime-local"
          :step "1"
          :enterKeyHint "done"
          :value (:adding-event state)
          :name :new-event
          :on-change #(set-state
                       assoc
                       :adding-event
                       (.. % -target -value))
          :on-key-down (partial handle-add-keydown state set-state (:id item))})
        (d/input
         {:class "new-event-note"
          :type "text"
          :placeholder "Note"
          :value (:adding-note state)
          :on-change #(set-state
                       assoc
                       :adding-note
                       (.. % -target -value))
          :on-key-down (partial handle-add-keydown state set-state (:id item))})
        (d/div
         {:class "edit-actions"}
         ($ add-button
            {:state state
             :set-state set-state
             :item item
             :display "Save"})
         (d/button
          {:class "cancel"
           :on-click #(cancel-add-event! set-state)}
          "Cancel"))))
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
               :state state
               :set-state set-state
               :category-id (:id item)
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
         {:state state :set-state set-state :item-id (:id item)})))))

(defnc categories [{:keys [state set-state]}]
  (let [categories (:categories state)
        [drag-state set-drag-state] (hooks/use-state nil)

        display-categories (if (and drag-state
                                    (not=
                                     (:from drag-state)
                                     (:to drag-state)))
                             (move
                              categories
                              (:from drag-state)
                              (:to drag-state))
                             categories)

        handle-drag-start (fn [e position]
                            (set-drag-state {:from position :to position})
                            (set!
                             (.. e -dataTransfer -effectAllowed)
                             "move")
                            (.. e
                                -dataTransfer
                                (setDragImage
                                 (js/document.createElement "canvas") 0 0))
                            (.. e
                                -dataTransfer
                                (setData "text/html" (.. e -target))))

        handle-drag-enter (fn [e position]
                            (when drag-state
                              (set-drag-state assoc :to position)))

        handle-drag-end (fn [e]
                          (when (and drag-state
                                     (not=
                                      (:from drag-state)
                                      (:to drag-state)))
                            (log-category-change!
                             set-state
                             :move-category
                             {:from-index (:from drag-state)
                              :to-index (:to drag-state)
                              :category-id (get-in categories
                                                   [(:from drag-state) :id])}))
                          (set-drag-state nil))]

    (d/ul
     (doall
      (for [[index item] (map-indexed (fn [i v] [i v]) display-categories)]
        (d/li
         {:key (:id item)
          :draggable true
          :class (when (and drag-state (= index (:to drag-state))) "dragging")
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
        {:class (str
                 "toast "
                 (if (get-in state [:network-response :success])
                   "success"
                   "error"))
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
     :on-key-down (partial handle-add-category-keydown state set-state)})
   (d/div
    {:class "edit-actions"}
    (d/button
     {:class "add"
      :on-click (fn []
                  (add-category! state set-state)
                  (set-state assoc :new-category ""))}
     "Add")
    (d/button
     {:class "cancel"
      :on-click (fn []
                  (set-state assoc :new-category ""))}
     "Cancel"))))

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
                                        [:categories]))
        processed-log-ref (hooks/use-ref (count (:categories-log local-data)))]

    ;; watch changelog and apply changes to persistent categories state
    (hooks/use-effect
     [(:categories-log state)]
     (let [log (:categories-log state)
           cnt (count log)
           processed @processed-log-ref]
       (if (> cnt processed)
         (let [new-entries (subvec log processed)
               current-categories (:categories state)
               new-categories (reduce
                                apply-log-entry
                                current-categories
                                new-entries)]
           (reset! processed-log-ref cnt)
           (set-state assoc :categories new-categories))
         (when (< cnt processed)
           (reset! processed-log-ref cnt)))))

    ;; update local storage
    (hooks/use-effect
      [(:config state) (:categories state) (:categories-log state)]
     (write-local-storage!
      "2"
      (:config state)
      (:categories state)
      (:categories-log state)))

    ;; upload changes
    (hooks/use-effect
      [(:categories state) (:categories-log state)]
     (when-not
      (=
        (select-keys state [:categories :categories-log])
        (select-keys last-upload [:categories :categories-log]))
       (upload! false state set-state)
       (set-last-upload
         merge
         (select-keys state [:categories :categories-log]))))

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
