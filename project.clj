(defproject corpsfini "0.1.0"
  :description "Code for thinking about finite fields and pseudorandomness"
  :url "https://github.com/mars0i/corpsfini"
  :license {:name "Gnu General Public License version 3.0"
            :url "http://www.gnu.org/copyleft/gpl.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
		 [io.github.nextjournal/clerk "0.3.233"]
                 [aerial.hanami "0.15.1"]
                 [net.mikera/core.matrix "0.63.0"]
                ]
  :source-paths ["src/clj"]
  ;:java-source-paths ["src/java"]
  ;:repl-options {:init-ns corpsfini.core}
)


;; Used to silence annoying slf4j warnings with Neanderthal, but
;; not working here:
;[com.taoensso/timbre "4.10.0"]
;[com.fzakaria/slf4j-timbre "0.3.19"]
