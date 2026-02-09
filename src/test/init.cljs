(ns init
  (:require ["global-jsdom" :as global-jsdom]))

;; Initialize JSDOM immediately so that browser globals are available
;; during namespace loading of other files.
(global-jsdom)

;; Mock localStorage if it's not provided by the environment
(when (cljs.core/undefined? js/localStorage)
  (set! js/localStorage #js {:getItem (fn [_] nil)
                             :setItem (fn [_ _] nil)
                             :removeItem (fn [_] nil)}))
