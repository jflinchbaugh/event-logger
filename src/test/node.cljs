(ns node
  (:require [init]
            [event-logger.core-test]
            [event-logger.localstorage-test]
            [cljs.test :as test]))

(defn main []
  (test/run-all-tests #".*-test$"))
