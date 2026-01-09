(ns event-logger.core-test
  (:require [cljs.test :as t]
            ["@testing-library/react" :as tlr]
            ["react" :as react]
            [tick.core :as tc]
            [helix.core :refer [$]]
            [cljs.pprint :refer [pprint]]
            [event-logger.core :as sut]
            [event-logger.localstorage :as ls]
            [clojure.string :as str]
            [cljs.core.async :refer [go]]
            [cljs.core.async.interop :refer-macros [<p!]]))

(defn setup-root [f]
  (f)
  (tlr/cleanup))

(defn local-storage-fixture [f]
  (ls/remove-item! :config)
  (ls/remove-item! :categories)
  (ls/remove-item! :categories-log)
  (f)
  (ls/remove-item! :config)
  (ls/remove-item! :categories)
  (ls/remove-item! :categories-log))

(t/use-fixtures :each setup-root local-storage-fixture)

(t/deftest test-now-str
  (t/testing "now-str shows date/time to the second"
    (t/is
     (re-matches
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}"
      (sut/now-str)))))

(t/deftest test-now-str-ms
  (t/testing "now-str shows date/time to milliseconds"
    (t/is
     (re-matches
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{0,3}"
      (sut/now-str-ms)))))

(t/deftest test-normalize-date-str
  (t/is (= "2024-01-01T01:01:01"
           (sut/normalize-date-str "2024-01-01T01:01:01")))
  (t/is (= "2024-01-01T01:01:01"
           (sut/normalize-date-str "2024-01-01T01:01:01.033")))
  (t/is (= "2024-01-01T01:01:00"
           (sut/normalize-date-str "2024-01-01T01:01"))))

(t/deftest test-normalize-event
  (t/is (= {:date-time "2024-01-01T01:01:01" :note nil}
           (sut/normalize-event {:date-time "2024-01-01T01:01:01"})))
  (t/is (= {:date-time "2024-01-01T01:01:01" :note "foo"}
           (sut/normalize-event {:date-time "2024-01-01T01:01:01" :note "foo"}))))

(t/deftest test-describe-diff
  (t/are [expected value unit]
         (= expected (sut/describe-diff (tc/new-duration value unit)))
    "0 seconds"   -1 :seconds
    "0 seconds"   0 :seconds
    "1 second"    1 :seconds
    "2 seconds"   2 :seconds
    "60 seconds"  60 :seconds
    "120 seconds" 120 :seconds
    "2 minutes"   121 :seconds
    "60 minutes"  60 :minutes
    "120 minutes" 120 :minutes
    "2 hours"     121 :minutes
    "24 hours"    24 :hours
    "48 hours"    48 :hours
    "2 days"      49 :hours))

(t/deftest test-average-duration
  (t/testing "no data gives nil"
    (t/is (nil? (sut/average-duration nil)))
    (t/is (nil? (sut/average-duration [])))
    (t/is (nil? (sut/average-duration ["1992-01-01T01:01:01"]))))
  (t/testing "averages"
    (t/is (= "1 second"
             (sut/average-duration [{:date-time "1992-01-01T01:01:01"}
                                    {:date-time "1992-01-01T01:01:02"}])))
    (t/is (= "1 second"
             (sut/average-duration [{:date-time "1992-01-01T01:01:01"}
                                    {:date-time "1992-01-01T01:01:02"}
                                    {:date-time "1992-01-01T01:01:03"}])))

    (t/is (= "2 seconds"
             (sut/average-duration [{:date-time "1992-01-01T01:01:01"}
                                    {:date-time "1992-01-01T01:01:02"}
                                    {:date-time "1992-01-01T01:01:05"}])))
    (t/is (= "4 hours"
             (sut/average-duration [{:date-time "1992-01-01T01:01:01"}
                                    {:date-time "1992-01-01T01:01:02"}
                                    {:date-time "1992-01-01T09:01:01"}])))))
(t/deftest test-local-storage

  (t/testing "read from empty"
    (t/is
     (= {:categories-log []
         :categories []
         :config nil
         :new-config nil}
        (sut/read-local-storage))))

  (t/testing "write"
    (sut/write-local-storage!
     "1"
     {:user "user"}
     [{:cat "time" :events ["2024-01-01T01:01:01"]}]
     [{:cat :log}])
    (t/is (= {:categories-log [{:cat :log}]
              :categories [{:cat "time"
                            :events [{:date-time "2024-01-01T01:01:01"
                                      :note ""}]}]
              :config {:user "user"}
              :new-config {:user "user"}}
             (sut/read-local-storage)))))

(t/deftest test-confirms
  (t/testing "clear"
    (let [state {:confirm {:whatever 1}}
          set-state (fn [f & c] (apply f (cons state c)))]
      (t/is (= {} (sut/clear-confirms! set-state)))))

  (t/testing "set"
    (let [state {:confirm {:whatever 1}}
          set-state (fn [f & c] (apply f (cons state c)))]
      (t/is (=
             {:confirm {:whatever 1 :another 2}}
             (sut/set-confirm! set-state :another 2)))))

  (t/testing "get"
    (let [state {:confirm {:whatever 1}}]
      (t/is (= 1 (sut/get-confirm state :whatever))))))

(t/deftest test-event
  (t/testing "is-new-event?"
    (t/is (sut/is-new-event? [] {:date-time "event"}))
    (t/is (sut/is-new-event?
           [{:date-time "other"}]
           {:date-time "event"}))
    (t/is (not (sut/is-new-event?
                [{:date-time "event"}]
                {:date-time "event"}))))
  (t/testing "adding-event?"
    (t/is (not (sut/adding-event? {})))
    (t/is (sut/adding-event? {:adding-event "event"}))))

(t/deftest test-debugger
  (let [render-result (tlr/render ($ sut/debugger {:state {:things :ok}}))
        container (.-container render-result)
        debug-button (tlr/getByText container "Debug")]

    (t/testing "debugger has a debug button and no info initially"
      (t/is debug-button)
      ;; Assert that the debug wrapper is not visible initially
      (t/is (nil? (tlr/queryByText container "Reload"))))

    (t/testing "clicking the button exposes debug data"
      (tlr/fireEvent.click debug-button)
      ;; Assert that the debug wrapper is now visible and contains expected text
      (t/is (tlr/getByText container "Reload"))
      (t/is (tlr/getByText container "Upload"))
      (t/is (tlr/getByText container "Download"))
      (t/is (tlr/getByText container "{:things :ok}")))

    (t/testing "clicking the button hides debug data"
      (tlr/fireEvent.click debug-button)
      ;; Assert that the debug wrapper is no longer visible
      (t/is (nil? (tlr/queryByText container "Reload"))))))

(t/deftest test-move-category
  (t/testing "move"
    (t/is (= {:categories ["a" "b" "c"]}
             (sut/move-category {:categories ["a" "b" "c"]} 0 0)))
    (t/is (= {:categories ["b" "a" "c"]}
             (sut/move-category {:categories ["a" "b" "c"]} 0 1)))
    (t/is (= {:categories ["b" "c" "a"]}
             (sut/move-category {:categories ["a" "b" "c"]} 0 2)))
    (t/is (= {:categories ["c" "a" "b"]}
             (sut/move-category {:categories ["a" "b" "c"]} 2 0)))
    (t/is (= {:categories ["a" "c" "b"]}
             (sut/move-category {:categories ["a" "b" "c"]} 2 1)))
    (t/is (= {:categories ["a" "b" "c"]}
             (sut/move-category {:categories ["a" "b" "c"]} 2 2)))))

(t/deftest test-title-bar
  (t/testing "title-bar renders the correct title"
    (let [render-result (tlr/render ($ sut/title-bar))
          container (.-container render-result)]
      (t/is (tlr/getByText container "Event Logger")))))

(t/deftest test-app
  (let [render-result (tlr/render ($ sut/app))
        container (.-container render-result)
        add-btn (tlr/getByText container "Add")
        debug-btn (tlr/getByText container "Debug")
        category-input #(tlr/getByPlaceholderText container "New Category")]

    (t/testing "app has a debug button and no info initially"
      (t/is debug-btn)
      ;; Assert that the debug wrapper is not visible initially
      (t/is (nil? (tlr/queryByText container "Reload"))))

    (t/testing "clicking the button exposes debug data"
      (tlr/fireEvent.click debug-btn)
      (t/is (tlr/getByText container "Reload"))
      (t/is (tlr/getByText container "Upload"))
      (t/is (tlr/getByText container "Download")))

    (t/testing "clicking the button hides debug data"
      (tlr/fireEvent.click debug-btn)
      (t/is (nil? (tlr/queryByText container "Reload"))))

    (t/testing "there are basic components and no categories"
      (t/is add-btn)
      (t/is debug-btn)
      (t/is (category-input))
      (t/is
       (not (tlr/queryByText container "+"))
       "no categories shown"))

    (t/testing "add a category"
      (t/is (= 0 (count (tlr/queryAllByText container "+")))
            "there are no categories, those no add buttons")
      (t/is (= ":new-category" (.-name (category-input))))
      (t/is (= "" (.-value (category-input))))

      (tlr/fireEvent.change
       (category-input)
       #js {:target #js {:value "My New Category"}})
      (t/is (= "My New Category" (.-value (category-input))))

      (t/is add-btn)
      (tlr/fireEvent.click add-btn)

      (t/is (= "" (.-value (category-input))) "category input is cleared")

      (t/is
       (= "category"
          (.-className (tlr/getByText container "My New Category")))
       "new category shows in the list")

      (t/is (= 1 (count (tlr/queryAllByText container "+")))
            "new category has 1 + button"))))
