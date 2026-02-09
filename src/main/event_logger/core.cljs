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
            [goog.string :refer [format]]
            [event-logger.version :as version])
  (:import [goog.events KeyCodes]))

(def response-display-ms 3000)

(def build-date version/build-date)

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

(defn- get-event-time
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

(defn- storage->edn
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
  (ls/set-item! :version version)
  (ls/set-item! :config config)
  (ls/set-item! :categories categories)
  (ls/set-item! :categories-log categories-log))

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
  [force state dispatch]
  (when-not (:network-response state)
    (let [config (:config state)
          {:keys [resource user password]} config]
      (when (or force (configured? resource user password))
        (dispatch [:set-network-action "Upload"])
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
            (if (and (:success response) (not (:categories edn-response)))
              (dispatch
               [:set-network-response
                (assoc response
                       :success false
                       :error-text "Failed to upload! Check resource config.")])
              (dispatch [:set-network-response response]))))))))

(defn download!
  ;; TODO move into reducer?
  [config dispatch]
  (dispatch [:set-network-action "Download"])
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
      (if (:success response)
        (if (:categories edn-response)
          (do
            (dispatch [:set-categories (:categories edn-response)])
            (dispatch [:set-network-response response]))
          (dispatch
           [:set-network-response
            (assoc response
                   :success false
                   :error-text "Failed to download! Check resource config.")]))
        (dispatch [:set-network-response response])))))

;; confirmations utils

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

(defn- toggle-category [state item-id]
  (if (= item-id (:display-category state))
    (dissoc state :display-category)
    (assoc state :display-category item-id)))

(defn reducer [state [type data]]
  (tel/log! :info {:type type :data data})
  (case type
    :log-category-change
    (let [{:keys [change-type category-data]} data
          new-log-entry {:timestamp (now-str-ms)
                         :type change-type
                         :data category-data}
          new-log (conj (:categories-log state) new-log-entry)
          new-categories (apply-log-entry (:categories state) new-log-entry)]
      (assoc state
             :categories-log new-log
             :categories new-categories))

    :toggle-category
    (let [item-id data]
      (-> state
          (reducer [:clear-confirms])
          (reducer [:cancel-edit])
          (reducer [:cancel-add-event])
          (toggle-category item-id)))

    :add-category
    (let [new-cat-name (str/replace
                        (->> state :new-category str/trim)
                        #" +" " ")
          new-cat-id (str/replace
                      (str/lower-case new-cat-name)
                      #" " "-")
          existing-categories (set (map :id (:categories state)))]
      (if (and (not (str/blank? new-cat-name))
               (not (existing-categories new-cat-id)))
        (-> state
            (reducer [:log-category-change
                      {:change-type :add-category
                       :category-data {:id new-cat-id :name new-cat-name}}])
            (reducer [:clear-new-category]))
        (if (existing-categories new-cat-id)
          (->
            state
           (reducer [:toggle-category new-cat-id])
           (reducer [:clear-new-category]))
          state)))

    :delete-category
    (-> state
        (reducer [:log-category-change
                  {:change-type :delete-category
                   :category-data {:id data}}])
        (reducer [:toggle-category nil]))

    :move-category
    (let [{:keys [from-index to-index category-id]} data]
      (reducer state [:log-category-change
                      {:change-type :move-category
                       :category-data {:from-index from-index
                                       :to-index to-index
                                       :category-id category-id}}]))

    :add-event
    (let [id data]
      (if (adding-event? state)
        (let [time (normalize-date-str (:adding-event state))
              note (str/trim (or (:adding-note state) ""))
              idx (some (fn [[i c]] (when (= (:id c) id) i))
                        (map-indexed vector (:categories state)))
              event {:note note :date-time time}
              existing-events (when idx (get-in state [:categories idx :events]))]
          (if (and idx (is-new-event? existing-events event))
            (-> state
                (reducer [:log-category-change
                          {:change-type :add-event
                           :category-data {:category-id id :event event}}])
                (reducer [:cancel-edit])
                (reducer [:cancel-add-event])
                (reducer [:clear-confirms]))
            state))
        (-> state
            (assoc :display-category id)
            (assoc :adding-event (now-str) :adding-note "")
            (reducer [:cancel-edit]))))

    :confirm-delete-event
    (let [{:keys [id event]} data
          new-confirmation {:id id :event (:date-time event)}]
      (-> state
          (assoc-in [:confirm :delete-event]
                    (when
                     (not= new-confirmation (get-confirm state :delete-event))
                      new-confirmation))
          (reducer [:cancel-edit])
          (reducer [:cancel-add-event])))

    :delete-event
    (-> state
      (reducer [:log-category-change
                {:change-type :delete-event
                 :category-data {:category-id (:id (get-confirm state :delete-event))
                                 :event data}}])
      (reducer [:clear-confirms]))

    :start-edit-event
    (let [{:keys [category-id event]} data]
      (-> state
        (assoc :editing-event {:category-id category-id
                               :original-event event
                               :time (:date-time event)
                               :note (:note event)})
        (reducer [:clear-confirms])
        (reducer [:cancel-add-event])))

    :save-edit-event
    (let [{:keys [category-id original-event time note]} (:editing-event state)
          note (str/trim (or note ""))
          new-event {:date-time (normalize-date-str time) :note note}]
      (-> state
          (reducer [:log-category-change
                    {:change-type :delete-event
                     :category-data {:category-id category-id :event original-event}}])
          (reducer [:log-category-change
                    {:change-type :add-event
                     :category-data {:category-id category-id :event new-event}}])

          (reducer [:cancel-edit])))

    :cancel-edit (dissoc state :editing-event)
    :cancel-add-event (dissoc state :adding-event :adding-note)

    :set-network-action (assoc state :network-action data)
    :set-network-response (assoc state :network-response data)
    :clear-network-status (assoc state :network-response nil :network-action nil)

    :save-config
    (assoc state :config (select-keys (:new-config state) [:resource :user :password]))

    :update-new-config
    (assoc-in state (cons :new-config (first data)) (second data))

    :set-categories (assoc state :categories data)
    :set-new-category (assoc state :new-category data)
    :clear-new-category (reducer state [:set-new-category ""])
    :set-adding-event (assoc state :adding-event data)
    :set-adding-note (assoc state :adding-note data)
    :set-editing-time (assoc-in state [:editing-event :time] data)
    :set-editing-note (assoc-in state [:editing-event :note] data)
    :confirm-delete-category (assoc-in state
                                   [:confirm :delete-category] data)
    :clear-confirms (dissoc state :confirm)
    :clear-log (assoc state :categories-log [])

    state))

(defn editing-event? [state category-id event]
  (let [editing (:editing-event state)]
    (and (= (:category-id editing) category-id)
         (= (:original-event editing) event))))

(defn enter-key? [e]
  (== KeyCodes/ENTER (.-which e)))

(defn esc-key? [e]
  (== KeyCodes/ESC (.-which e)))

(defn handle-keydown [on-enter on-esc e]
  (cond
    (enter-key? e) (on-enter)
    (esc-key? e) (on-esc)))

(defn handle-edit-keydown [dispatch e]
  (handle-keydown
   #(dispatch [:save-edit-event])
   #(dispatch [:cancel-edit])
   e))

(defn handle-add-keydown [dispatch item-id e]
  (handle-keydown
   #(dispatch [:add-event item-id])
   #(dispatch [:cancel-add-event])
   e))

(defn handle-add-category-keydown [dispatch e]
  (handle-keydown
   #(dispatch [:add-category])
   #(dispatch [:set-new-category ""])
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
    (hooks/use-effect [now]
                      (js/setTimeout (partial set-now (t/date-time)) 1000))
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

(defnc add-button [{:keys [dispatch item display]}]
  (d/button
   {:on-click (fn [] (dispatch [:add-event (:id item)]))}
   display))

(defnc category-controls [{:keys [dispatch state item-id]}]
  (d/div
   {:class "controls"}
   (if (= (get-confirm state :delete-category) item-id)
     (d/button
      {:class "delete"
       :on-click (fn [] (dispatch [:delete-category item-id]))}
      "Really?")
     (d/button
      {:class "delete"
       :on-click (fn [] (dispatch [:confirm-delete-category item-id]))}
      "X"))))

(defnc event-details
  [{:keys [event
           expanded-fn?
           expand-action
           delete-action
           state
           dispatch
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
          :on-change #(dispatch
                       [:set-editing-time
                        (.. % -target -value)])
          :on-key-down (partial handle-edit-keydown dispatch)})
        (d/input
         {:class "new-event-note"
          :type "text"
          :value (get-in state [:editing-event :note])
          :on-change #(dispatch
                       [:set-editing-note
                        (.. % -target -value)])
          :on-key-down (partial handle-edit-keydown dispatch)})
        (d/div
         {:class "edit-actions"}
         (d/button
          {:class "save"
           :on-click #(dispatch [:save-edit-event])}
          "Save")
         (d/button
          {:class "cancel"
           :on-click #(dispatch [:cancel-edit])}
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
                         (dispatch [:start-edit-event
                                    {:category-id category-id
                                     :event event}]))}
            "Edit"))))
       (when (not (str/blank? (:note event)))
         (d/div {:class "note"} (:note event)))
       (when (:duration event)
         (d/div {:class "duration"} (str "(" (:duration event) ")")))))))

(defnc category-details [{:keys [dispatch state item]}]
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
          :on-change #(dispatch
                       [:set-adding-event
                        (.. % -target -value)])
          :on-key-down (partial handle-add-keydown dispatch (:id item))})
        (d/input
         {:class "new-event-note"
          :type "text"
          :placeholder "Note"
          :value (:adding-note state)
          :on-change #(dispatch
                       [:set-adding-note
                        (.. % -target -value)])
          :on-key-down (partial handle-add-keydown dispatch (:id item))})
        (d/div
         {:class "edit-actions"}
         ($ add-button
            {:dispatch dispatch
             :item item
             :display "Save"})
         (d/button
          {:class "cancel"
           :on-click #(dispatch [:cancel-add-event])}
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
               :dispatch dispatch
               :category-id (:id item)
               :expanded-fn? (partial
                              event-expanded?
                              state
                              item)
               :expand-action (fn [e]
                                (dispatch [:confirm-delete-event
                                           {:id (:id item)
                                            :event e}]))
               :delete-action (fn [e]
                                (dispatch [:delete-event e]))}))))
      ($ category-controls
         {:state state :dispatch dispatch :item-id (:id item)})))))

(defnc categories [{:keys [state dispatch]}]
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
                            (dispatch
                             [:move-category
                              {:from-index (:from drag-state)
                               :to-index (:to drag-state)
                               :category-id (get-in categories
                                                    [(:from drag-state) :id])}]))
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
            {:dispatch dispatch
             :item item
             :display "+"})
         (d/span
          {:class "category"
           :on-click (fn [] (dispatch [:toggle-category (:id item)]))}
          (:name item))
         (when (= (:id item) (:display-category state))
           ($ category-details
              {:dispatch dispatch :state state :item item}))))))))

(defnc network-response-display [{:keys [state dispatch]}]
  (let [network-response (get state :network-response)
        network-action (get state :network-action)]
    (hooks/use-effect
     [network-response network-action]
     (when (and network-response network-action)
       (js/setTimeout
        (fn []
          (dispatch [:clear-network-status]))
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
         :on-click #(dispatch [:set-network-response nil])}
        (if (get-in state [:network-response :success])
          (str (:network-action state) " succeeded!")
          (str
           (:network-action state)
           " failed: "
           (get-in state [:network-response :error-text]))))))))

(defnc debugger [{:keys [state dispatch]}]
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
         (d/div "Build Date: " build-date))
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
           :on-click (fn [] (upload! true state dispatch))}
          "Upload"))
        (d/div
         {:class "row"}
         (d/button
          {:class "download"
           :on-click (fn []
                       (download! (:config state) dispatch))}
          "Download"))
        (d/div
         {:class "row"}
         (d/button
          {:class "clear-log"
           :on-click (fn [] (dispatch [:clear-log]))}
          "Clear Log"))
        (d/pre
         (->>
          state
          (obfuscate :config)
          (obfuscate :new-config)
          pp/pprint
          with-out-str)))))))

(defnc config [{:keys [state dispatch]}]
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
                                          (dispatch
                                           [:update-new-config
                                            [[:resource]
                                             (.. e -target -value)]]))
                             :value (get-in state [:new-config :resource])}))
                    (d/div {:class "row"}
                           (d/label {:for :user} "User")
                           (d/input
                            {:name :user
                             :id :user
                             :on-change (fn [e]
                                          (dispatch
                                           [:update-new-config
                                            [[:user]
                                             (.. e -target -value)]]))
                             :value (get-in state [:new-config :user])}))
                    (d/div {:class "row"}
                           (d/label {:for :password} "Password")
                           (d/input
                            {:name :password
                             :id :password
                             :type "password"
                             :on-change (fn [e]
                                          (dispatch
                                           [:update-new-config
                                            [[:password]
                                             (.. e -target -value)]]))
                             :value (get-in state [:new-config :password])}))
                    (d/div {:class "row"}
                           (d/button
                            {:class "save"
                             :on-click (fn []
                                         (dispatch [:save-config]))}
                            "Save Config")))))))

(defnc add-category-form [{:keys [state dispatch]}]
  (d/div
   {:class "add"}
   (d/span {:class "category-debug"} (:new-category state))
   (d/input
    {:class "new-category"
     :key :new-category
     :type "text"
     :name :new-category
     :enterKeyHint "done"
     :placeholder "New Category"
     :value (:new-category state)
     :on-change (fn [e]
                  (dispatch [:set-new-category (.. e -target -value)]))
     :on-key-down (partial handle-add-category-keydown dispatch)})
   (d/div
    {:class "edit-actions"}
    (d/button
     {:class "add"
      :on-click (fn []
                  (dispatch [:add-category]))}
     "Add")
    (d/button
     {:class "cancel"
      :on-click (fn []
                  (dispatch [:set-new-category ""]))}
     "Cancel"))))

(defnc title-bar []
  (d/div {:class "title-bar"}
         (d/h1 "Event Logger")))

(defnc app []
  (let [local-data (read-local-storage)
        [state dispatch] (hooks/use-reducer
                          reducer
                          (assoc
                           local-data
                           :new-category ""))
        [last-upload set-last-upload] (hooks/use-state
                                       (select-keys
                                        local-data
                                        [:categories :categories-log]))]

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
     [(:categories state)]
     (when-not
      (= (select-keys state [:categories :categories-log])
         (select-keys last-upload [:categories :categories-log]))
       (upload! false state dispatch)
       (set-last-upload
        merge
        (select-keys state [:categories :categories-log]))))

    (<>
     ($ title-bar)
     (d/div
      {:class "wrapper"
       :key "div.wrapper"}
      ($ categories {:state state :dispatch dispatch})
      ($ add-category-form {:state state :dispatch dispatch})
      ($ config {:state state :dispatch dispatch})
      ($ debugger {:state state :dispatch dispatch})
      ($ network-response-display {:state state :dispatch dispatch})))))
