(ns event-logger.app
  (:require [event-logger.core :refer [app]]
            [helix.core :refer [$]]
            [taoensso.telemere :as tel]
            ["react-dom/client" :as rdom]))

;; start your app with your favorite React render
(defonce root (rdom/createRoot (js/document.getElementById "root")))

(defn https? [location]
  (= (.-protocol location) "https:"))

(defn register-service-worker []
  (when (and js/ServiceWorker #_(https? js/location))
    (.addEventListener js/window "load"
      (fn []
        (-> (.-serviceWorker js/navigator)
          (.register "service-worker.js")
          (.then (fn [registration]
                   (tel/log!
                     :info
                     (str
                       "ServiceWorker registration successful with scope:"
                       (.-scope registration)))))
          (.catch (fn [err]
                    (tel/log!
                      :error
                      (str "ServiceWorker registration failed:" err)))))))))
(defn render []
  (tel/log! :info "rendering app")
  (.render root ($ app)))

(defn ^:export init []
  (render)
  (register-service-worker))

(defn ^:dev/after-load reload! []
  (render))
