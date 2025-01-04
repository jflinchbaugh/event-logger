(ns event-logger.app
  (:require [helix.core :refer [defnc $]]
            [helix.hooks :as hooks]
            [helix.dom :as d]
            ["react-dom/client" :as rdom]
            [tick.core :as t]))

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
  (-> state :adding-event nil?))

;; action

(defn add-event! [state set-state id]
  (if (have-event? state)
  (set-state assoc :adding-event (now-str))
  (let [time (:adding-event state)
        idx (.indexOf (map :id (:categories state)) id)
        existing-events (get-in state [:categories idx :events])]
    (when (is-new-event? existing-events time)
      (print "adding" time "to" id "at" idx)
      (set-state update-in [:categories idx :events] conj time)
      (set-state assoc :adding-event nil)))))

(defn open-category! [state set-state item]
  (when (not= (:id  item) (:display-category state))
    (set-state assoc :adding-event nil)
    (set-state assoc :display-category (:id item))))

;; define components using the `defnc` macro

(defnc add-button [{:keys [state set-state item display]}]
(d/button
  {:on-click (partial add-event! state set-state (:id item))}
    display))

(defnc category-details [{:keys [set-state state item]}]
  (d/div {:class "details" :id (str "details-" (:id item))}
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
              event)))))))

(defnc categories [{:keys [state set-state]}]
  (let [categories (:categories state)]
    (d/ul
      (doall
        (for [item categories]
          (d/li
            {:on-click (partial open-category! state set-state item)
             :key (:id item)}
            ($ add-button {:state state :set-state set-state :item item :display "+"})
            (d/label (:name item))
            (when (= (:id item) (:display-category state))
              ($ category-details
                 {:set-state set-state :state state :item item}))))))))

(defnc debugger [{:keys [state]}]
  (d/pre {:id "debug"} (with-out-str (cljs.pprint/pprint state))))

(defnc app []
  (let [[state set-state] (hooks/use-state
                            {:categories [{:id "cat" :name "cat"}
                                          {:id "dog" :name "dog"}]})]
    (d/div {:class "wrapper"}
      ($ categories {:state state :set-state set-state})
      ($ debugger {:state state}))))

;; start your app with your favorite React renderer
(defonce root (rdom/createRoot (js/document.getElementById "root")))

(defn render []
  (.render root ($ app)))

(defn ^:export init []
  (println "initial load")
  (render))

(defn ^:dev/after-load reload! []
  (println "Reload!")
  (render))
