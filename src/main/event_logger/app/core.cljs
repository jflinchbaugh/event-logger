(ns event-logger.app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [alandipert.storage-atom :refer [local-storage]]
            [clojure.string :as str]))

;; --- App State ---

(defonce state (local-storage (r/atom {}) :event-logger))

(defonce new-category-value (r/atom ""))

(comment

  (reset!
    state
    {:categories [{:name "Code"
                   :id "code"
                   :occurrences []}
                  {:name "Eat"
                   :id "eat"
                   :occurrences []}]})

  @state

  (reset! new-category-value "hi")

  ;
  )

;; --- Utility Functions ---

(defn add-event! [r event]
  (swap! r identity))

(defn build-category [name]
  {:name name
   :id (str/replace (str/lower-case name) #" +" "-")
   :occurrences []})

(defn add-category! []
  (swap! state update-in [:categories] conj (build-category @new-category-value))
  (reset! new-category-value "")
  )

(defn track-category-value! [e]
  (reset! new-category-value (-> e .-target .-value)))

(defn category-key! [e]
  (when (== 13 (.-which e))
    (add-category!)))

;; --- Views ---

(defn categories []
  [:ul
   (for [item (:categories @state)]
     [:li
      {:key (:id item)}
      [:button "+"]
      [:label (:name item)]
      ])])

(defn add-item-form []
  [:div
   [:br]
   [:input
    {:type "text"
     :size 20
     :value @new-category-value
     :name :new-category
     :on-change track-category-value!
     :on-key-down category-key! }]
   [:button.add {:on-click add-category!} "Add"]])

;; --- App Component ---

(defn app []
  [:div.wrapper
   [categories]
   [add-item-form]
   ])

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
