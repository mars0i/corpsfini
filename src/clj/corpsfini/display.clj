(ns corpsfini.display
    (:require [nextjournal.clerk :as clerk]
              [clojure.string :as string]))

(def powers-of-x 
  "A lazy infinite sequence of strings of the form:
  (\"x\", \"x^2\" ... \"x^n\", ...).
  Note that the first, constant term is not included."
  (concat [1 "x"] (map (fn [e] (str "x^{" e "}")) (drop 2 (range))))) ; first expt is 2

(defn texify-terms
  "Convert a corpsfini polynomial sequence into a sequence of LaTeX 
  representations of the terms of the polynomial."
  [poly]
  (map (fn [coef power] (cond (zero? coef) nil
                              (= 1 power) coef
                              (= 1 coef) power
                              :else (str coef power)))
       poly
       powers-of-x))

(defn texify-poly
  "Convert a corpsfini polynomial sequence into a LaTeX representation of 
  the polynomial, with terms in order from lowest degree to highest degree."
  [poly]
  (string/join " + "
    (keep identity  ; squash out the nils
          (texify-terms poly))))

(defn texify-polys
  "Convert a sequence of corpsfini polynomials into a LaTeX representation of 
  a sequence of polynomial (with terms within polynomials ordered from lowest 
  degree to highest degree)."
  [polys]
  (str "("
       (string/join "), (" (map texify-poly polys))
       ")"))
  
(defn clerk-poly
  "Display LaTeX representation of a polynomial in clerk."
  [poly]
  (clerk/tex (texify-poly poly)))

(defn clerk-polys
  "Display LaTeX representation of a sequence of polynomials in clerk."
  [polys]
  (clerk/tex (texify-polys polys)))


