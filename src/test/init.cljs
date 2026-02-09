(ns init
  (:require ["global-jsdom" :as global-jsdom]))

;; Initialize JSDOM immediately so that browser globals are available
;; during namespace loading of other files.
(global-jsdom)
