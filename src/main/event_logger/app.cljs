(ns event-logger.app
  (:require [helix.core :refer [defnc $]]
            [helix.hooks :as hooks]
            [event-logger.localstorage :as ls]
            [cognitect.transit :as transit]
            [helix.dom :as d]
            ["react-dom/client" :as rdom]
            [tick.core :as t]
            [clojure.string :as str]
            [clojure.edn :as edn]))

;; utilities

(defn json->clj [x]
  (transit/read (transit/reader :json) x))

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

(defn is-new-event?
  "is this event new, and not already in the list?"
  [existing-events event]
  (empty? (filter (partial = event) existing-events)))

(defn have-event?
  "is there an event being added?"
  [state]
  (-> state :adding-event))

(defn describe-diff [diff]
  (let [days (t/days diff)
        hours (t/hours diff)
        minutes (t/minutes diff)]
    (prn days hours minutes)
    (cond
      (> 1 minutes) "0 minutes ago"
      (> 2 minutes) "1 minute ago"
      (> 1 hours) (str minutes " minutes ago")
      (> 2 hours) "1 hour ago"
      (> 1 days) (str hours " hours ago")
      (> 2 days) "1 day ago"
      :else (str days " days ago"))))

;; action

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

;; define components using the `defnc` macro

(defnc since-component [{:keys [category]}]
  (let [now (now-str)
        events (:events category)]
    (when (seq events)
      (d/div {:class "time-since"}
             (let [last-event (-> events sort last t/date-time)
                   diff (t/between last-event now)]
               (describe-diff diff))))))

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
         (d/label
          {:on-click (partial open-category! state set-state (:id item))}
          (:name item))
         (when (= (:id item) (:display-category state))
           ($ category-details
              {:set-state set-state :state state :item item}))))))))

(defnc debugger [{:keys [state]}]
  (let [[debugger set-debugger] (hooks/use-state false)]
    (d/div {:id "debug"}
           (d/button
            {:class "debug"
             :on-click (fn [] (set-debugger not))}
            "Debug")
           (d/pre
            (when debugger
              (with-out-str (cljs.pprint/pprint state)))))))

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
                           {:categories []
                            :new-category ""})]
    (hooks/use-effect
     :once
     (let [categories  (vec (edn/read-string (ls/get-item :categories)))
           old (json->clj (ls/get-item "[\"~#'\",\"~:event-logger\"]"))]
       (set-state assoc :categories
                  (if (seq categories) categories (:categories old)))
       (set-state assoc :old old)))
    (hooks/use-effect
     [state]
     (ls/set-item! :version "1")
     (ls/set-item! :categories (pr-str (:categories state))))
    (d/div
     {:class "wrapper"}
     ($ categories {:state state :set-state set-state})
     ($ add-category-form {:state state :set-state set-state})
     ($ debugger {:state state}))))

;; start your app with your favorite React render
(defonce root (rdom/createRoot (js/document.getElementById "root")))

(defn render []
  (.render root ($ app)))

(defn ^:export init []
  (render))

(defn ^:dev/after-load reload! []
  (render))
