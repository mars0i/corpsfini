(ns corpsfini.display
    (:require [nextjournal.clerk :as clerk]
              [clojure.string :as string]))

(def powers-of-x 
  "A lazy infinite sequence of strings of the form:
  (\"x\", \"x^2\" ... \"x^n\", ...).
  Note that the first, constant term is not included."
  (concat [1 "x"] (map (fn [e] (str "x^{" e "}")) (drop 2 (range))))) ; first expt is 2

(defn texify-terms
  [poly]
  (map (fn [coef power] (cond (zero? coef) nil
                              (= 1 power) coef
                              (= 1 coef) power
                              :else (str coef power)))
       poly
       powers-of-x))


(defn texify-poly
  [poly]
  (string/join " + "
    (keep identity  ; squashout the nils
          (texify-terms poly))))
  

(defn clerk-poly
  [poly]
  (clerk/tex (texify-poly poly)))


