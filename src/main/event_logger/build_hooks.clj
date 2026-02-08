(ns event-logger.build-hooks
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn version-index-resources
  {:shadow.build/stage :flush}
  [state]
  (let [ts (str (System/currentTimeMillis))]
    (spit "public/index.html" (str/replace (slurp "src/html/index.html") "{ts}" ts)))
  state)

(defn version-cljs
  {:shadow.build/stage :configure}
  [state]
  (let [ts (str (java.time.Instant/now))
        content (str "(ns event-logger.version)\n\n"
                     "(def build-date \"" ts "\")\n")]
    (io/make-parents "src/main/event_logger/version.cljs")
    (spit "src/main/event_logger/version.cljs" content))
  state)
