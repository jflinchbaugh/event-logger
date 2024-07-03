(ns event-logger.app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [alandipert.storage-atom :refer [local-storage]]
            [clojure.string :as str]
            [tick.core :as t]))

;; --- App State ---

(defonce state (local-storage (r/atom {}) :event-logger))

(defonce new-category-value (r/atom ""))
(defonce confirm-delete-id (r/atom nil))
(defonce adding-event (r/atom nil))
(defonce confirm-delete-event (r/atom nil))
(defonce display-category (r/atom nil))

;; --- Utility Functions ---

(defn reset-editing!
  "reset all in-progress edit states"
  []
  (reset! new-category-value "")
  (reset! confirm-delete-id nil)
  (reset! adding-event nil)
  (reset! confirm-delete-event nil))

(defn format-date-time [dt]
  (t/format :iso-local-date-time dt))

(defn now-str
  "produce a string for the datetime now"
  []
  (-> (t/date-time) (t/truncate :seconds) format-date-time))

(defn process-date-str
  "parse, truncate, and reformat a date string"
  [event-str]
  (->
    event-str
    t/date-time
    (t/truncate :seconds)
    format-date-time))

(defn make-category-id
  "format an id string from a category name"
  [name]
  (str/replace (str/lower-case name) #" +" "-"))

(defn make-category
  "build a whole category map, ready to store, from a name"
  [name]
  (let [trimmed (str/trim name)]
    {:name trimmed
     :id (make-category-id trimmed)
     :events []}))

(defn already-category?
  [current-state category]
  ((->> current-state :categories (map :id) set) (make-category-id category)))

(defn open-category! [id]
  (reset-editing!)
  (reset! display-category id))

(defn toggle-category! [id]
  (reset-editing!)
  (open-category! (if (= id @display-category) nil id)))

(defn add-category! []
  (if (and
       (not (str/blank? @new-category-value))
       (not (already-category? @state @new-category-value)))
    (do
      (swap!
       state
       update-in [:categories]
       conj (make-category @new-category-value))
      (reset-editing!))
    (open-category! (make-category-id @new-category-value))))

(defn track-category-value! [e]
  (reset! new-category-value (-> e .-target .-value)))

(defn category-key-down! [e]
  (when (== 13 (.-which e))
    (add-category!)))

(defn delete-category! [id]
  (swap!
   state
   update-in [:categories]
   (comp vec (partial remove (comp #{id} :id))))
  (open-category! nil))

(defn confirm-delete-category! [id]
  (reset! confirm-delete-id id))

(defn add-event! []
  (swap!
   state
   update-in [:categories]
   (fn [cats]
     (mapv
      (fn [cat]
        (if (not= (:id @adding-event) (:id cat))
          cat
          (update-in
            cat
            [:events]
            (comp distinct conj)
            (process-date-str (:event @adding-event)))))
      cats)))
  (reset-editing!))

(defn open-add-event! [id]
  (open-category! id)
  (reset! adding-event {:id id :event (now-str)}))

(defn track-event-value! [e]
  (swap! adding-event assoc-in [:event] (-> e .-target .-value)))

(defn event-key-down! [e]
  (when (== 13 (.-which e))
    (add-event!)))

(defn open-delete-event! [id event]
  (reset-editing!)
  (reset! confirm-delete-event {:id id :event event}))

(defn delete-event! [event]
  (swap!
    state
    update-in [:categories]
    (fn [cats]
      (mapv
        (fn [cat]
          (if (not= (:id @confirm-delete-event) (:id cat))
            cat
            (update-in
              cat
              [:events]
              (fn [events] (remove #{event} events))
              )))
        cats)))
  (reset-editing!))

(defn displayed-category []
  (->>
    @state
    :categories
    (filter
      (fn [item] (= (:id item) @display-category)))
    first)
  )

;; --- Views ---

(defn category-controls [id]
  [:div.controls
   (when (not @adding-event)
     (if (= @confirm-delete-id id)
       [:button.delete
        {:on-click (partial delete-category! id)}
        "Really?"]
       [:button.delete
        {:on-click (partial confirm-delete-category! id)} "X"]))])

(defn describe-diff [diff]
  (let [days (t/days diff)
        hours (t/hours diff)]
    (cond
      (> 1 hours) (str hours " hours")
      (> 2 hours) (str hours " hour")
      (> 1 days) (str hours " hours")
      (> 2 days) (str days " day")
      :else (str days " days")))
  )

(defn since-component []
  (let [now (r/atom (now-str))]
    (fn []
      (js/setTimeout (fn [] (reset! now (now-str))) (* 3600 1000))
      (let [events (:events (displayed-category))]
        (when-not (-> events empty?)
          [:div.time-since
          (let [last-event (-> events sort last t/date-time)
                now (t/date-time @now)
                diff (t/between last-event now)]
            (describe-diff diff)
            )])))))

(defn add-button [item display]
  [:button {:on-click (if @adding-event
                        add-event!
                        (partial open-add-event! (:id item)))} display])

(defn category-details [item]
  [:div.details {:id (str "details-" (:id item))}
   [since-component item]
   (when @adding-event
     [:div
      [:input.new-event
       {:type "datetime-local"
        :enterKeyHint "done"
        :value (:event @adding-event)
        :name :new-event
        :on-change track-event-value!
        :on-key-down event-key-down!}]
      [add-button item "Save"]])
   [:ul.events
    (doall
      (for [event (reverse (sort (:events item)))]
        [:li
         {:key event}
         [:span.event
          {:on-click (partial open-delete-event! (:id item) event)}
          (process-date-str event)]
         (when (= {:id (:id item) :event event} @confirm-delete-event)
           [:button.delete
            {:on-click (partial delete-event! event)}
            "X"])]))]
   [category-controls (:id item)]])

(defn categories []
  [:ul
   (doall
     (for [item (:categories @state)]
       [:li
        {:key (:id item)}
        [add-button item "+"]
        [:label
         {:on-click (partial toggle-category! (:id item))}
         (:name item)]
        (when (= (:id item) @display-category)
          [category-details item])]))])

(defn add-item-form []
  [:div.add
   [:input.new-category
    {:type "text"
     :enterKeyHint "done"
     :value @new-category-value
     :name :new-category
     :on-change track-category-value!
     :on-key-down category-key-down!}]
   [:button.add {:on-click add-category!} "Add"]])

;; --- App Component ---

(defn app []
  [:div.wrapper
   [categories]
   [add-item-form]])

;; --- Render App ---

(defn render []
  (rdom/render [app] (.getElementById js/document "root")))

;; `^:export` metadata prevents function name from being munged during `:advanced` release compilation
(defn ^:export main []
  (println "Initial render")
  (render))

;; `^:dev/after-load` metadata hook tells Shadow-CLJS to run this function after each hot reload
(defn ^:dev/after-load reload! []
  (println "Reload!")
  (render))
