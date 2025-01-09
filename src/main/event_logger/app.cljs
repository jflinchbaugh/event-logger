(ns event-logger.app
  (:require [helix.core :refer [defnc $]]
            [helix.hooks :as hooks]
            [helix.dom :as d]
            ["react-dom/client" :as rdom]
            [tick.core :as t]
            [clojure.string :as str]))

;; utilities

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

(defn is-new-event? [existing-events event]
  (empty? (filter (partial = event) existing-events)))

(defn have-event? [state]
  (-> state :adding-event))

;; action

(defn open-category! [state set-state item-id]
  (if (= item-id (:display-category state))
    (set-state dissoc :display-category)
    (do
      (set-state dissoc :adding-event)
      (set-state assoc :display-category item-id))))

(defn add-category! [state set-state]
  (let [new-cat-name (str/replace
                      (->> state :new-category str/trim)
                      #" +"
                      " ")
        existing-categories (set (map :id (:categories state)))]
    (when (not (str/blank? new-cat-name))
      (if (existing-categories new-cat-name)
        (open-category! state set-state new-cat-name)
        (set-state
         update :categories
         conj {:id new-cat-name :name new-cat-name})))))

(defn delete-category! [state set-state item-id]
  (set-state update :categories
             (comp vec (partial remove (comp #{item-id} :id))))
  (open-category! state set-state nil))

(defn add-event! [state set-state id]
  (if (have-event? state)
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

;; define components using the `defnc` macro

(defnc add-button [{:keys [state set-state item display]}]
  (d/button
   {:on-click (partial add-event! state set-state (:id item))}
   display))

(defnc category-controls [{:keys [set-state state item-id]}]
  (d/div {:class "controls"}
         (d/button
          {:class "delete"
           :on-click (partial delete-category! state set-state item-id)}
           "X")))

(defnc category-details [{:keys [set-state state item]}]
  (d/div
   {:class "details" :id (str "details-" (:id item))}
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
         {:state state :set-state set-state :item item :display "Save"})))
   (d/ul {:class "events"}
         (doall
          (for [event (reverse (sort (map normalize-date-str (:events item))))]
            (d/li
             {:key (str (:id item) "-" event)}
             (d/span {:class "event"}
                     event))))
     ($ category-controls
        {:state state :set-state set-state :item-id (:id item)}))))

(defnc categories [{:keys [state set-state]}]
  (let [categories (:categories state)]
    (d/ul
     (doall
      (for [item categories]
        (d/li
         {:key (:id item)}
         ($ add-button {:state state :set-state set-state :item item :display "+"})
         (d/label
          {:on-click (partial open-category! state set-state (:id item))}
          (:name item))
         (when (= (:id item) (:display-category state))
           ($ category-details
              {:set-state set-state :state state :item item}))))))))

(defnc debugger [{:keys [state]}]
  (d/pre {:id "debug"} (with-out-str (cljs.pprint/pprint state))))

(defnc add-category-form [{:keys [state set-state]}]
  (d/div
   {:class "add"}
   (d/input
    {:class "new-category"
     :key :new-category
     :type "text"
     :name :new-category
     :enterKeyHint "done"
     :placeholder "new category"
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
  (let [[state set-state] (hooks/use-state
                           {:categories [{:id "cat" :name "cat"}
                                         {:id "dog" :name "dog"}]
                            :new-category ""})]
    (d/div {:class "wrapper"}
           ($ categories {:state state :set-state set-state})
           ($ add-category-form {:state state :set-state set-state})
           ($ debugger {:state state}))))

;; start your app with your favorite React renderer
(defonce root (rdom/createRoot (js/document.getElementById "root")))

(defn render []
  (.render root ($ app)))

(defn ^:export init []
  (render))

(defn ^:dev/after-load reload! []
  (render))
