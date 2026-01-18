(ns event-logger.localstorage-test
  (:require [cljs.test :as t]
            [event-logger.localstorage :as sut]))

(t/deftest test-localstorage
  (t/testing "read/write storage"
    (t/is (nil? (sut/get-item "key")))
    (sut/set-item! "key" "value")
    (t/is (= (sut/get-item "key")))
    (sut/remove-item! "key")))
