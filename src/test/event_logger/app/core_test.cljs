(ns event-logger.app.core-test
  (:require [cljs.test :as t]
            [reagent.core :as r]
            [tick.core :as tc]
            [event-logger.app.core :as sut]))

(t/deftest test-make-category-id
  (t/is (= "my-id" (sut/make-category-id "My ID"))))

(t/deftest test-make-category
  (t/is
    (= {:name "My Name" :id "my-name" :events []}
      (sut/make-category " My Name "))))

(t/deftest test-format-date-time
  (t/is
    (= "2023-01-01T01:01:01"
      (sut/format-date-time
        (tc/date-time "2023-01-01T01:01:01")))))

(comment
  (t/run-all-tests)
                                        ;
  )
