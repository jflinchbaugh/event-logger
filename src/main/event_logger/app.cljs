(ns event-logger.app
  (:require [event-logger.core :refer [app]]
            [helix.core :refer [$]]
            [taoensso.telemere :as tel]
            ["react-dom/client" :as rdom]))

;; start your app with your favorite React render
(defonce root (rdom/createRoot (js/document.getElementById "root")))

(defn render []
  (tel/log! :info "rendering app")
  (.render root ($ app)))

(defn ^:export init []
  (render))

(defn ^:dev/after-load reload! []
  (render))
