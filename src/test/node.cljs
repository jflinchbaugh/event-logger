(ns node
  (:require ["global-jsdom" :as global-jsdom]
            [cljs.test :as test]))

(defn main []
  (global-jsdom)
  (test/run-all-tests #".*-test$"))
