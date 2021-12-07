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

;; A Hanami chart:
(def taus-chart
  (hc/xform ht/point-chart 
            :DATA (take 500 taus-pair-maps)
            :X "0"
            :Y "1"
            ;:YTITLE "Pairs of subsequent of 12-bit Tausworthe outputs."
            ;:COLOR "yow"
            ))

(clerk/vl taus-chart)

;; It would be nice to be able to produce a 3-D plot in order to illustrate
;; 3-D lattice effects.  The utility of this is illustrated by the plots on 
;; page 675 of this paper:
;;
;; @Article{MatsumotoEtAl:PRNGimpossibilityCompromise,
;;  author =	{Matsumoto, Makoto and Saito, Mutsuo and Haramoto, Hiroshi and Nishimura, Takuji},
;;  title =	{Pseudorandom Number Generation: Impossibility and Compromise},
;;  journal =	{Journal of Universal Computer Science},
;;  year =	{2006},
;;  volume =	{12},
;;  number =	{6},
;;  pages =	{672-690},
;;}
;;
;; It's apparently possible but not documented that you can make 3-D plots
;; in vega-lite:
;; https://github.com/vega/vega/issues/1738
;;
;; Another option for 3-D might be https://github.com/thi-ng/geom .


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
