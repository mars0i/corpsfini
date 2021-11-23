(ns corpsfini.display'
    (:require [nextjournal.clerk :as clerk]))

(def powers-of-x 
  "A lazy infinite sequence of strings of the form
  (1, \"x\", \"x^2\, ... \"x^n\", ...)."
  (concat ["1" "x"] 
          (map (fn [e] (str "x^" e)) (rest (range)))))

(defn texify-terms
  [poly]
  (let [integer-coef (first poly)
        x-coefs (rest-poly)]
    (keep identity
          (cons
            (if (zero? integer-coef)  ; constant term is special case
              nil
              integer-coef)
            (map (fn [coef power] (if (zero? coef)
                                    nil
                                    (str coef power)))
                 x-coefs
                 powers-of-x)))))



(defn texify-poly
  [poly]
  (join

(defn clerk-poly
  [poly]
  (clerk/tex (texify-polynomial poly)))


