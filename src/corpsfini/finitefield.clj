(ns corpsfini.finitefield
    (:require [clojure.math.numeric-tower :as nt]
              [clojure.core.matrix :as mx]
              [utils.genl :as g]))

;; For now, polynomials are Clojure vectors of integers that are elements 
;; of a prime field, with smaller exponents on the left.  
;; Later: Possibly generalize to subfields other than prime fields.

;; See also algrand.finitefield-examples.

;; (Future project:
;; Create an overlapping set of functions (in another namespace) that performs
;; these operations on F_2_n polynomials represented as integers with
;; all operations implemented as bit manipulations using Java functions.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code sections listed below, in order:
;;
;; HIGH-LEVEL FUNCTIONS
;; MISC UTILITY FUNCTIONS
;; INTEGER ARITHMETIC MOD m
;; POLYNOMIAL ARITHMETIC MOD POLY
;; MATRICES OVER PRIME FIELDS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare generate-by-powers trace degree pad-high-zeros strip-high-zeros
         normalize-lengths make-monomial make-zero-poly add-int sub-int
         mult-int expt-int invert-int-nomemo div-int mult-int-poly
         div-int-poly add-poly sub-poly mult-poly-raw mult-poly
         expt-poly div-poly mod-poly =-poly)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIGH-LEVEL FUNCTIONS 
;; The ones most handy for experimenting.

;; For examples of primitive polynomials for p see finitefield_examples.clj .
(defn generate-by-powers
  "Returns an infinite sequence of elements from Fm^n, where m is a prime
  number and n is the degree of primitive polynomial p, generated from initial 
  element e as it is multiplied by 1, then the result e*1, and so on.  If e is
  not supplied, it be the polynomial x (which is represented by [0 1]).  That 
  is, the sequence will then consists of x^0 = [1], x^1 = [0 1], x^2 = ... ."
  ([p m] (generate-by-powers p m [0 1]))
  ([p m e] (iterate (partial mult-poly p m e) [1])))

;; Since I'm restricting the subfield to prime fields, I extract
;; the integer from the result rather than returning a singleton polynomial.
(defn trace
  "Compute the trace of polynomial poly in Fm^n (with primitive polynomial p) 
  to to Fm, where m is prime.  (Since only prime subfields are allowed here,
  the returned value is an integer.)"
  [p m n poly]
  (first 
    (reduce 
      (fn [sum i] 
          (add-poly m sum
                    (expt-poly p m poly (nt/expt m i))))
      [0] (range n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC UTILITY FUNCTIONS

(defn degree
  "Returns the degree of (vector) polynomial p, or nil if all zeros, i.e.
  it represents the zero polynomial."
  [p]
  (let [len (count p)]
    (loop [i (dec len)]
      (cond (neg? i) -1   ; i.e. all zeros, negative degree
            (pos? (p i)) i ; note zero degree means pnomial is nonzero constant
            :else (recur (dec i))))))

(defn pad-high-zeros
  "If sequence p is shorter than minimum-length, concatenate zeros
  onto it so that it has minimum-length."
  [minimum-length p]
  (let [n-zeros (- minimum-length (count p))]
    (if (pos? n-zeros)
      (vec (concat p (repeat n-zeros 0)))
      p)))

;; Not used here, but useful elsewhere
(defn pad-low-zeros
  "If sequence p is shorter than minimum-length, concatenate zeros
  onto it so that it has minimum-length."
  [minimum-length p]
  (let [n-zeros (- minimum-length (count p))]
    (if (pos? n-zeros)
      (vec (concat (repeat n-zeros 0) p))
      p)))

(defn strip-high-zeros
  "If polynomial vector p has extra zeros after the largest nonzero
  term, strip them off.  If nothing is left, return [0]."
  [p]
  (let [deg (degree p)]
    (if (neg? deg)
      [0] ; looks better than []
      (vec (take (inc deg) p)))))

(defn normalize-lengths
  "If one of the polynomial sequences poly1 or poly2 is shorter than the
  other, pad it with initial zeros."
  [poly1 poly2]
  (let [poly1-len (count poly1)
        poly2-len (count poly2)]
    (cond (< poly1-len poly2-len) [(pad-high-zeros poly2-len poly1) poly2]
          (> poly1-len poly2-len) [poly1 (pad-high-zeros poly1-len poly2)]
          :else [poly1 poly2])))

(defn make-monomial
  [exponent coef]
  (conj (vec (repeat exponent 0)) coef))

(defn make-zero-poly
  [len]
  (vec (repeat len 0)))

;; These can be used to create polynomials in my format from text strings 
;; copied from Alanen & Knuth (1964), Sankhya Series A, v. 26 no. 4, p. 309.
(defn alanenknuth-string-to-poly
  "Transform a string in \"Alanen-Knuth\" format--i.e. a string of
  digits representing polynomial coefficients, with high exponents
  on the left--into a representation of a polynomial as a vector of
  integer coefficients with higher exponents on the right.  The
  asterisk character is treated as the number 10.  [See Alanen &
  Knuth (1964), Sankhya Series A, v. 26 no. 4, p. 309.]"
  [ak-str]
  (mapv 
    (fn [c] (if (= c \*) 10 (Character/digit c 10)))
    (reverse ak-str))) ; makes string into seq of chars

(defn alanenknuth-strings-to-polys
  "Transform a sequence of strings representing polyonomials in \"Alanen-Knuth\"
  format into a sequence of integer vector representations of the same 
  polynomials using alanenknuth-string-to-poly."
  [ak-strs]
  (mapv alanenknuth-strings-to-polys ak-strs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTEGER ARITHMETIC MOD m

(defn add-int
  "Add x and y mod m."
  [m x y]
  (mod (+ x y) m))

(defn sub-int
  "Subtract x and y mod m."
  [m x y]
  (mod (- x y) m))

(defn mult-int
  "Multiply x and y mod m."
  [m x y]
  (mod (* x y) m))

(defn expt-int
  "Raise integer x to integer power e, mod m."
  [m x e]
  (long (mod (nt/expt x e) m))) ; may use Clojure BigInt; after mod, long is OK

;; See https://en.wikipedia.org/wiki/Finite_field_arithmetic#Multiplicative_inverse
;; Current version uses theorem that for nonzero elements of a field of prime 
;; order m, x^{m-1} = 1 mod m, so x^{m-2} mod m is x's multiplicative 
;; inverse.  This can produce very large integers internally, making use
;; of Clojure's BigInt facility.
;; I might consider revising to make use of the extended Euclidean algorithm, which
;; won't use such large numbers, as specified here:
;; and https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
(defn invert-int-nomemo
  "Computes the inverse of a nonzero x, mod m, assuming that m is prime.
  Does not memoize: Recomputes every time the same arguments are provided."
  [m x]
  (expt-int m x (- m 2)))

(def invert-int 
  "([m x])
  Computes the inverse of a nonzero x, mod m, assuming that m is prime.
  Memoizes: The inverse of x mod m is only computed the first time the
  function is called with m and x.  Then the result is stored for future
  use in the same Clojure session."
  (memoize invert-int-nomemo))

;; Note that it's not enough to simply divide usng quot and then mod the result.
(defn div-int
  "Divides x by y mod m, or rather, multiplies x by the inverse of y mod m."
  [m x y]
  (mult-int m x (invert-int m y)))

(defn mult-int-poly
  "Multiply coefficients of polynomial poly by scalar value x mod m."
  [m x poly]
  (mapv (partial mult-int m x)
        poly))

(defn div-int-poly
  "Divide coefficients of polynomial poly by scalar value y mod m."
  [m y poly]
  (mapv (partial mult-int m (invert-int m y))
        poly))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POLYNOMIAL ARITHMETIC MOD POLY

;; Additiion and substraction need to mod wrt the underlying integer
;; modulus, but don't need to mod wrt the primitive polynomial as long
;; as the arguments have already been normalized by modding.

;; Division needs to be able to mod wrt the integer modulus, but doesn't 
;; need to mod wrt to the primitive polynomial, as long as the
;; arguments have already been modded wrt it, because the result can't
;; be any longer than the dividend, so they don't need modding wrt the 
;; polynomial.  Even though division uses multiplication, it's always
;; just single-term multiplication, so it doesn't need the polynomial mod.

;; Only multiplication needs mod wrt the primitive polynomial, because
;; the raw result might have higher degree than is allowed.

;; (Perhaps I should add a polynomial arg to addition, subtraction, and
;; division, but it won't do anything.  I suppose it might be useful
;; for passing these functions as options with the same args.  Cross that
;; bridge if and when the need arises.)

(defn add-poly
  [m poly1 poly2]
  "Add polynomials poly1 and poly2 with mod m arithmetic on coefficients.
  Does not carry."
  (let [[poly1' poly2'] (normalize-lengths poly1 poly2)]
    (mapv (partial add-int m) poly1' poly2')))

(defn sub-poly
  [m poly1 poly2]
  "Subtract polynomials poly1 and poly2 with mod m arithmetic on coefficients.
  Does not borrow."
  (let [[poly1' poly2'] (normalize-lengths poly1 poly2)]
    (mapv (partial sub-int m) poly1' poly2')))

;; See below for version that's mod wrt a primitive polynomial
(defn mult-poly-raw
  "Polynomial multiplication of polynomials poly1 and poly2, with
  coefficient multiplication mod m."
  [m poly1 poly2]
  (let [poly1-len (count poly1)
        poly2-len (count poly2)
        ;; result length is count-1 + count-1 + one more for zeroth place:
        starter (make-zero-poly (+ poly1-len poly2-len -1))
        indexes (for [i (range poly1-len), j (range poly2-len)] [i j])]
    ;; Vectors are associative in Clojure, so we can construct using update:
    (reduce (fn [poly [i1 i2]]
               (update poly
                       (+ i1 i2) ; multiplication sums exponents
                       (partial add-int m) (mult-int m (poly1 i1) (poly2 i2)))) ; add new product (old value is passed as first arg to updating fn)
            starter indexes)))

(defn mult-poly
  "Polynomial multiplication of polynomials poly1 and poly2, with
  coefficient multiplication mod m, in a finite field with primitive
  polynomial p."
  [p m poly1 poly2]
  (mod-poly p m (mult-poly-raw m poly1 poly2)))

(defn expt-poly
  "Raise polynomial poly to integer power e, with coefficient multiplication 
  mod m in a finite field with primitive polynomial p."
  [p m poly e]
  (when (neg? e) (throw (Exception. "Exponent is negagive.")))
  (mod-poly p m 
            (loop [i e, product [1]]
              (if (zero? i)
                product
                (recur (dec i) (mult-poly-raw m product poly))))))

;; Pseudocode for div-poly below:
;; let result vec = all zeros
;; if degree dsor > degree dend then ret result vec, and dend as the remainder
;; let a = div max idx of dend by max idx of dsor
;; let b = div max coef of dend by max coef of dsor, mod m
;; place b in loc a in temp result vec (mostly zeros)
;; let c = dsor * temp result vec, mod m
;; let new dend = dend - c, mod m
;; recurse with result vec += temp result vec (filled at diff locs: a merge)

;; Question: Don't I need mod by the defining primitive polynomial?
;; No--as long as the coefficients can only be elements of a finite prime
;; field, it doesn't need it; it just deals with individual terms in a
;; polynomial one at a time, and it doesn't care about the wider field.
;; But sometimes maybe that should be done before this function is called.
(defn div-poly
  "Long division mod primitive polynomial p, with coefficients mod m of,
  polyomial dividend by polynomial divisor.  
  Returns pair containing quotient and remainder polynomials."
  [m dividend divisor]
  (let [deg-dividend (degree dividend)
        deg-divisor (degree divisor)]
    (when (neg? deg-divisor) (throw (Exception. "Division by the zero polynomial.")))
    (loop [quotient (make-zero-poly (inc (- deg-dividend deg-divisor))) 
           dend dividend]
          (let [deg-dend (degree dend)]
            (if (> deg-divisor deg-dend)
              {:quotient quotient, :remainder (strip-high-zeros dend)}
              ;; Divide largest term in dividend by largest term in divisor:
              (let [qexpt (- deg-dend deg-divisor) ; divide exponent = subtract
                    qcoef (div-int m (dend deg-dend) (divisor deg-divisor))
                    newquotient (assoc quotient qexpt qcoef) ; add this term to result
                    multiplier (make-monomial qexpt qcoef)
                    newdend (sub-poly m dend
                                        (mult-poly-raw m divisor multiplier))] 
                (recur newquotient newdend)))))))

;; For debugging div-poly, place immediately above the recur line above:
;                (println "deg-dend:" deg-dend " deg-divisor:" deg-divisor) ; DEBUG
;                (println "qexpt:" qexpt " qcoef:" qcoef) ; DEBUG
;                (println "new quotient:" newquotient) ; DEBUG
;                (println "monomial multiplier:" multiplier) ; DEBUG
;                (println "new dividend:" newdend) ; DEBUG
;                (println)

(defn mod-poly
  [p m poly]
  (:remainder (div-poly m poly p)))

(defn =-poly
  "Tests two polynomials for equality, ignoring differences in number
  of zeros at the end of the representation."
  [poly1 poly2]
  (= (strip-high-zeros poly1)
     (strip-high-zeros poly2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATRICES OVER PRIME FIELDS

(defn finite-mmul
  "Matrix multiplication over the finite field F_q where q is prime.  
  m1 and m2 could be matrices, including row or column vectors in
  matrix form, or simple 1-D vectors.  (core.matrix/inner-product
  will usually adopt the correct orientation for 1-D vectors.)"
  [q m1 m2]
  (mx/emap #(mod % q) (mx/inner-product m1 m2)))

;; Not sure which of the following is more efficient for large powers.
;; Quick tests suggest they're quite similar.  BigInts are slow, perhaps.
;; TODO? For higher powers, it would be more efficient to divide and conquer.

;; This version calls mod only once at the end, performing matrix mult
;; using BigInts.
(defn finite-mpow
  "Raises matrix m over the finite field F_q to power n.  q must be prime.
  (Uses BigInts internally, performing mod only at the final step.)"
  [q m n]
  (let [big-m (mx/emap bigint m)]
    (mx/emap (fn [n] (long (mod n q))) ; convert to F_q longs
             (g/iter #(mx/inner-product big-m %) big-m n))))

;; This version runs mod each time.
(defn finite-mpow-alt
  "Raises matrix m over the finite field F_q to power n.  q must be prime.
  (Performs mod at each step.)"
  [q m n]
  (g/iter #(finite-mmul q m %) m n))
