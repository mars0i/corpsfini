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

(def taus-floats (lfe/tausworthe-floats 12 12
                                        (map first 
                                             (lfe/linrec12seq (repeat 12 1)))))

;; Note the keys must be strings:
(def testdata [{"a" 35, "b" 42, "yow" 1}, {"a" 15, "b" 48, "yow" 2}])

(def taus-pair-maps (cfd/tuples-to-vegalite-data (partition 2 3 taus-floats)))

(def taus-chart
  (hc/xform ht/point-chart 
            :DATA (take 500 taus-pair-maps)
            :X "0"
            :Y "1"
            ;:YTITLE "Pairs of subsequent of 12-bit Tausworthe outputs."
            ;:COLOR "yow"
            ))

(clerk/vl taus-chart)


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
