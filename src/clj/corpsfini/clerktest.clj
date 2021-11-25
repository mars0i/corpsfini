(ns corpsfini.clerktest
    (:require
      [corpsfini.display :as cfd]
      [corpsfini.finitefield :as cff]
      [corpsfini.finitefield-examples :as cfe]
      [corpsfini.lfsr-examples :as lfe]
      [nextjournal.clerk :as clerk]
      [aerial.hanami.common :as hc :refer [RMV]]
      [aerial.hanami.templates :as ht]))


(cfd/clerk-poly [1 2 0 1 5 45 0 3 1 0 0 0 1 2])

(cfd/clerk-polys [[1 1 0 1] [2 0 2 0 3] [5] [1 2 0 1 5 45 0 3 1 0 0 0 1 2]])

(def fs (lfe/tausworthe-floats 12 12
                               (map first 
                                    (lfe/linrec12seq (repeat 12 1)))))

;(def tausworthe-chart
;  (hc/xform ht/point-chart 
;            :DATA ;; fs in data form for hanami
;            :X "Horsepower"
;            :Y "Miles_per_Gallon"
;            :YTITLE "Miles per gallon"
;            :COLOR "Origin"))



;; -----------------------

;; How to start a clerk web server and file watcher:

(comment
  (nextjournal.clerk/serve! {:browse? true :watch-paths ["src"]})
  (nextjournal.clerk/show! "src/clj/corpsfini/clerktest.clj")
)

;; -----------------------

(def point-chart-example
  (hc/xform ht/point-chart 
            :UDATA "https://vega.github.io/vega-lite/data/cars.json"
            :X "Horsepower"
            :Y "Miles_per_Gallon"
            :YTITLE "Miles per gallon"
            :COLOR "Origin"))


;; Click on the three-dots button to the right of the chart for useful options.
(clerk/vl point-chart-example)
