(ns event-logger.build-info
  (:require [tick.core :as t]))

(defmacro build-date []
  (str (t/format "yyyy-MM-dd'T'HH:mm:ss" (java.time.LocalDateTime/now))))
