(ns event-logger.app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [alandipert.storage-atom :refer [local-storage]]
            [clojure.string :as str]
            [cljc.java-time.local-date-time :as ld]
            [cljc.java-time.format.date-time-formatter :as df]
            ))

;; --- App State ---

(defonce state (local-storage (r/atom {}) :event-logger))

(defonce new-category-value (r/atom ""))

(comment

  (reset!
   state
   {:display-category nil
    :categories [{:name "Code"
                  :id "code"
                  :events []}
                 {:name "Sleep"
                  :id "sleep"
                  :events []}]})

  @state

  (reset! new-category-value "hi")

  ;
  )

;; --- Utility Functions ---

(defn make-id [name]
  (str/replace (str/lower-case name) #" +" "-"))

(defn make-category [name]
  {:name name
   :id (make-id name)
   :events []})

(defn is-category? [category]
  ((->> @state :categories (map :id) set) (make-id category)))

(defn open-category! [id]
  (swap! state assoc-in [:display-category] id))

(defn toggle-category! [id]
  (open-category! (if (= id (:display-category @state)) nil id)))

(defn add-category! []
  (if (and
         (not (str/blank? @new-category-value))
         (not (is-category? @new-category-value)))
    (do
      (swap! state
        update-in [:categories]
        conj (make-category @new-category-value))
      (reset! new-category-value ""))
    (open-category! (make-id @new-category-value))))

(defn track-category-value! [e]
  (reset! new-category-value (-> e .-target .-value)))

(defn category-key-down! [e]
  (when (== 13 (.-which e))
    (add-category!)))

(defn delete-category! [id]
  (swap! state
    update-in [:categories]
    (comp vec (partial remove (comp #{id} :id))))
  (open-category! nil))

(defn add-event! [id]
  (open-category! id)
  (swap!
   state
   update-in [:categories]
   (fn [cats]
     (mapv
      (fn [cat]
        (if (not= id (:id cat))
          cat
          (update-in cat [:events] conj (str (ld/now)))))
      cats))))

;; --- Views ---

(defn category-controls [id]
  [:button.delete
   {:on-click (partial delete-category! id)}
   "X"])

(defn category-details [item]
  [:div.details {:id (str "details-" (:id item))}
   [:ul.events
    (for [event (reverse (sort (:events item)))]
      [:li {:key event} (df/format df/iso-local-date-time (ld/parse event))])]
   [category-controls (:id item)]])

(defn categories []
  [:ul
   (for [item (:categories @state)]
     [:li
      {:key (:id item)}
      [:button {:on-click (partial add-event! (:id item))} "+"]
      [:label
       {:on-click (partial toggle-category! (:id item))}
       (:name item)]
      (when (= (:id item) (:display-category @state))
        [category-details item])])])

(defn add-item-form []
  [:div.add
   [:input.new-category
    {:type "text"
     :size 20
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
