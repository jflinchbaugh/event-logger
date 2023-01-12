(ns event-logger.app.core-test
  (:require [cljs.test :as t]
            [reagent.core :as r]
            [event-logger.app.core :as sut]))


#_(deftest increment-atom
  (testing "increases the atom value by one"
    (let [r (r/atom 0)
          _ (incrementer! r)]
      (is (= 1 @r)))))

#_(deftest decrement-atom
  (testing "decreases the atom value by one"
    (let [r (r/atom 0)
          _ (decrementer! r)]
      (is (= -1 @r)))))


(t/deftest test-make-id
  (t/is (= "my-id" (sut/make-id "My ID"))))

(comment
  (t/run-all-tests)
                                        ;
  )
