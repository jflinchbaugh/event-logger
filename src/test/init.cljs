(ns init
  (:require ["global-jsdom" :as global-jsdom]))

;; Initialize JSDOM immediately so that browser globals are available
;; during namespace loading of other files.
(global-jsdom)

;; Mock localStorage if it's not provided by the environment
(when (cljs.core/undefined? js/localStorage)
  (let [storage (atom {})]
    (set! js/localStorage
          #js {:getItem (fn [k] (get @storage k))
               :setItem (fn [k v] (swap! storage assoc k v))
               :removeItem (fn [k] (swap! storage dissoc k))})))
