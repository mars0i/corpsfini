(ns corpsfini.display
    (:require [nextjournal.clerk :as clerk]))

(def powers-of-x 
  "A lazy infinite sequence of strings of the form:
  (\"x\", \"x^2\" ... \"x^n\", ...).
  Note that the first, constant term is not included."
  (cons "x" (map (fn [e] (str "x^" e)) (rest (range)))))

;; FIXME off-by-one errors
;; ALSO need to special-case when coef is 1
(defn texify-terms
  [poly]
  (let [integer-coef (first poly)
        x-coefs (rest poly)]
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
  ;; FIXME
  )

(defn clerk-poly
  [poly]
  (clerk/tex (texify-poly poly)))


