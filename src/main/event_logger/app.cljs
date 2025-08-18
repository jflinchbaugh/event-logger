(ns event-logger.app
  (:require [event-logger.core :refer [app]]
            [helix.core :refer [$]]
            [taoensso.telemere :as tel]
            ["react-dom/client" :as rdom]))

(def ^:export BUILD_ID nil)

;; start your app with your favorite React render
(defonce root (rdom/createRoot (js/document.getElementById "root")))

(defn register-service-worker []
  (when js/ServiceWorker
    (.addEventListener js/window "load"
      (fn []
        (-> (.-serviceWorker js/navigator)
          (.register (str "service-worker.js?v=" BUILD_ID))
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
