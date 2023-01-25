(ns event-logger.app.core-test
  (:require [cljs.test :as t]
            [reagent.core :as r]
            [event-logger.app.core :as sut]))

(t/deftest test-make-id
  (t/is (= "my-id" (sut/make-id "My ID"))))

(t/deftest test-make-category
  (t/is
    (= {:name "My Name" :id "my-name" :events []}
      (sut/make-category " My Name "))))

(comment
  (t/run-all-tests)
                                        ;
  )
