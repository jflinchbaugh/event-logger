(ns event-logger.app.core-test
  (:require [cljs.test :as t]
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

(t/deftest test-describe-diff
  (t/is (= "0 hours" (sut/describe-diff (tc/of-minutes 0))))
  (t/is (= "0 hours" (sut/describe-diff (tc/of-minutes 59))))
  (t/is (= "1 hour" (sut/describe-diff (tc/of-hours 1))))
  (t/is (= "2 hours" (sut/describe-diff (tc/of-hours 2))))
  (t/is (= "23 hours" (sut/describe-diff (tc/of-hours 23))))
  (t/is (= "1 day" (sut/describe-diff (tc/of-hours 24))))
  (t/is (= "1 day" (sut/describe-diff (tc/of-hours 47))))
  (t/is (= "2 days" (sut/describe-diff (tc/of-hours 48))))

  )

(comment
  (t/run-all-tests)

                                        ;
  )
