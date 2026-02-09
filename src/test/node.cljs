(ns node
  (:require [init]
            [cljs.test :as test]
            [event-logger.core-test]
            [event-logger.localstorage-test]))

(defn main []
  (test/run-all-tests #".*-test$"))
