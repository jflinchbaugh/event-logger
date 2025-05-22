(ns event-logger.app-test
  (:require [cljs.test :as t]
            ["@testing-library/react" :as tlr]
            [tick.core :as tc]
            [helix.core :refer [$]]
            [event-logger.app :as sut]
            [shadow.cljs.modern :refer (js-await)]
            [clojure.string :as str]))

(defn setup-root [f]
  (f)
  (tlr/cleanup))

(t/use-fixtures :each setup-root)

(t/deftest test-now-str
  (t/is (re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}" (sut/now-str))))

(t/deftest test-normalize-date-str
  (t/is (= "2024-01-01T01:01:01"
           (sut/normalize-date-str "2024-01-01T01:01:01")))
  (t/is (= "2024-01-01T01:01:01"
           (sut/normalize-date-str "2024-01-01T01:01:01.033")))
  (t/is (= "2024-01-01T01:01:00"
           (sut/normalize-date-str "2024-01-01T01:01"))))

(t/deftest test-describe-diff
  (t/are [expected value unit]
         (= expected (sut/describe-diff (tc/new-duration value unit)))
    "0 seconds ago"  -1 :seconds
    "0 seconds ago"   0 :seconds
    "1 second ago"    1 :seconds
    "2 seconds ago"   2 :seconds
    "1 minute ago"    1 :minutes
    "2 minutes ago"   2 :minutes
    "1 hour ago"      1 :hours
    "2 hours ago"     2 :hours
    "1 day ago"       1 :days
    "2 days ago"      2 :days))

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
    (t/is (sut/is-new-event? [] "event"))
    (t/is (sut/is-new-event? ["other"] "event"))
    (t/is (not (sut/is-new-event? ["event"] "event"))))
  (t/testing "adding-event?"
    (t/is (not (sut/adding-event? {})))
    (t/is (sut/adding-event? {:adding-event "event"}))))

(t/deftest test-debugger
  (let [container (-> ($ sut/debugger {:state {:things :ok}}) tlr/render)
        btn (.. container (getByText #"Debug"))]
    (t/testing "debugger has a debug button and no info"
      (t/is btn)
      (t/is (= "submit" (-> btn .-type)))

      (t/is (= "Debug" (-> container .-container .-innerText))))

    (t/testing "clicking the button exposes debug data"
      (t/is (.click tlr/fireEvent btn))
      (t/is
       (= ["Debug" "{:things :ok}"]
          (-> container .-container .-innerText str/split-lines))))

    (t/testing "clicking the button hides debug data"
      (t/is (.click tlr/fireEvent btn))
      (t/is (= "Debug" (-> container .-container .-innerText))))))

(t/deftest test-app
  (let [container (tlr/render ($ sut/app))
        add-btn (-> container (.getByText "Add"))
        debug-btn (-> container (.getByText "Debug"))
        category-input (-> container (.getByPlaceholderText "New Category"))
        ]
    (t/testing "there are basic components and no categories"
      (t/is add-btn)
      (t/is debug-btn)
      (t/is category-input)
      (t/is (not (-> container (.queryByText "+")))))
    (t/testing "add a category"
      #_(t/is (= ":new-category" (.-name category-input)))
      #_(.change tlr/fireEvent category-input {:target {:value "mine"}})
      #_(-> container (.getByValue "mine"))
      (.click tlr/fireEvent debug-btn)
      #_(t/is (= "thing" (-> container .-container .-innerText)))
      )))
