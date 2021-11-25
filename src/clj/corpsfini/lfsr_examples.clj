;; Miscellaneous experiments.  Not systematic.
(ns corpsfini.lfsr-examples
    (:require ;[clojure.math.numeric-tower :as nt]
              [clojure.core.matrix :as mx]
              [corpsfini.finitefield :as ff]
              [utils.convertbase :as cb]
    ))


;; Lidl & Niederreiter p. 402, Example 8.14:
(def m814 (mx/matrix [[0 0 0 0 1][1 0 0 0 1][0 1 0 0 0][0 0 1 0 0][0 0 0 1 0]]))

(def xs814 (iterate (fn [prev-state] (ff/finite-mmul 2 prev-state m814))
                    [0 0 0 0 1]))

;; or:

;; Think of the LFSR algorithm this way:
;; Calculate the new highest-index element.  
;; (You can think of it as now sitting just past the end of the register.)
;; Then shift every element to the lower element, losing the old lowest one.
;; And as part of this, the old highest element is replaced by the newly
;; calculated value, which is "shifted" in from its "location" off the
;; high end of the register.
(defn linrec814
  "Lidl and Niederreiter example 8.14 using explicit sums rather than 
  matrix multiplication."
  [v]
  ;; shift to lower locs   calc new elem for high index
  [(v 1) (v 2) (v 3) (v 4) (mod (+ (v 0) (v 1)) 2)])

(def ys814 (iterate linrec814 [0 0 0 0 1]))

;; Here's a recurrence (not in L&N).  It generates all 0's at first.
(defn linrecA
  [v]
  [(v 1) (v 2) (v 3) (v 4) (mod (+ (v 0) (v 4)) 2)])

(def ysA (iterate linrecA [0 0 0 0 1]))

(defn linrec822
  "Lidl and Niederreiter example 8.22."
  [[a0 a1]]
  [a1 (mod (+ a0 a1) 2)])

(def ys822 (iterate linrec822 [1 1])) ; Lidl and Niederreiter example 8.22

(defn linrec830
  "Lidl and Niederreiter example 8.30."
  [[a0 a1 a2 a3 a4 a5]]
  [a1 a2 a3 a4 a5 (mod (+ a4 a2 a1 a0) 2)])

(def ys830impulse (iterate linrec830 [0 0 0 0 0 1]))
(def ys830_11 (iterate linrec830 [0 0 0 0 1 1]))
(def ys830_100 (iterate linrec830 [0 0 0 1 0 0]))

(defn linrec4
  "Full period n=4 using taps based on polynomial from N&W p. 37."
  [[a0 a1 a2 a3]]
  [a1 a2 a3 (mod (+ a0 a1) 2)])

(defn linrec5
  "Full period n=5 using taps based on polynomial from N&W p. 37."
  [v]
  [(v 1) (v 2) (v 3) (v 4) (mod (+ (v 0) (v 2)) 2)])

(defn linrec6
  "Full period n=6 using taps based on polynomial from N&W p. 37."
  [[a0 a1 a2 a3 a4 a5]]
  [a1 a2 a3 a4 a5 (mod (+ a0 a1) 2)])

(defn linrec8
  "Full period n=8 using taps based on polynomial from N&W p. 37."
  [[a0 a1 a2 a3 a4 a5 a6 a7]]
  [a1 a2 a3 a4 a5 a6 a7 (mod (+ a0 a2 a3 a4) 2)])


(defn linrec12
  "Full period n=12 using four taps based on polynomial from N&W p. 37."
  [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11]]
  [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 (mod (+ a0 a1 a4 a6) 2)])

(defn linrec12seq
  "Returns a lazy sequence from linrec12 starting from seed."
  [seed]
  (iterate linrec12 seed))

;; FIXME works but not right
(defn tausworthe12
  "Returns a pair containing the new state and a new output number as
  a Clojure fraction.  (Pass to a function such as double to convert to a 
  floating point number.)"
  [decimation wordsize state]
  (let [states (linrec12seq state)
        bitseq (map last states)
        decimated-bitseq (drop decimation bitseq) ; IS THIS RIGHT?
        word-bits (take wordsize decimated-bitseq)   ; IS THIS RIGHT?
        newstate (first (drop (+ decimation wordsize) states))]; IS THIS RIGHT?
    [(cb/sum-digits 2 [0] word-bits) newstate]))

(defn base-n-tausworthe
  "Given intseq, a potentially infinite sequence of integers in the given 
  base, drops the first decimation integers, and then uses wordsize integers
  to construct a Clojure fraction by treating each the integers as subsquent
  digits in a \"decimal\" float representation of a number in [0,1).  Returns
  a pair containing the fraction and the new intseq with decimation integers 
  removed.  Tips: (1) Pass the fraction to double to convert to a floating
  point number.  (2) Set base to 2 and use a sequence of bits to make a true
  Tausworthe generator."
  [base decimation wordsize intseq]
  (let [newseq (drop decimation intseq)
        wordbits (take wordsize newseq)]
    [(cb/sum-digits base [0] wordbits) newseq]))

(defn tausworthe
  "Given bitseq, a potentially infinite sequence of 0's and 1's, drops the 
  first decimation bits, and then uses wordsize bits to construct a Clojure
  fraction by treating each the bits as subsquent digits in a binary \"decimal\"
  float representation of a number in [0,1).  Returns a pair containing the 
  fraction and the new bitseq with decimation bits removed.  Tip: Pass the 
  fraction to double to convert to a floating point number."
  [decimation wordsize bitseq]
  (base-n-tausworthe 2 wordsize decimation bitseq))

;(defn decimate-sequence
;  "Given a sequence of integers, returns a sequence of sequences, each of
;  size wordsize, at intervals of length decimation.  (Actually just a wrapper
;  with a convenient new name for the standard Clojure function 'partition'.)"
;  [wordsize decimation intseq]
;  (partition wordsize decimation intseq))

(defn base-n-tausworthe-fracts
  "Given intseq, a potentially infinite sequence of integers in the given
  base, returns a sequence of fractions, each created by using wordsize 
  integers from the sequence at a position decimation steps from the last 
  word.  Creates the fractions by treating each the integers as subsquent
  digits in a \"decimal\" float representation of a number in [0,1).  Pass
  the fractions to double to convert to floating point numbers."
  [base decimation wordsize intseq]
  (map (partial cb/sum-digits base [0])         ; convert to fractions
       (partition wordsize decimation intseq))) ; word seqs, decimation-shifted

(defn tausworthe-fracts 
  "Given bitseq, a potentially infinite sequence of 0's and 1's, returns 
  a sequence of fractions, each created by using wordsize bits from the 
  sequence at a position decimation steps from the last word.  Creates 
  the fractions by treating each the bits as subsquent digits in a binary
  \"decimal\" float representation of a number in [0,1).  Pass the fractions
  to double to convert to floating point numbers."
  [decimation wordsize bitseq]
  (base-n-tausworthe-seq 2 decimation wordsize bitseq))

(defn tauseworthe-seq
"Given bitseq, a potentially infinite sequence of 0's and 1's, returns 
  a sequence of doubles, each created by using wordsize bits from the 
  sequence at a position decimation steps from the last word.  See
  tausworthe-fracts for more information."
  [decimation wordsize bitseq]
  (map double (tausworthe-fracts decimation wordsize bitseq)))

;;;;;;;;;;;;;;;;;;;;;;;;;

;; OR SHOULD THE OLD ONE NOT BE DROPPED?  Let the sequence extend??
;; Simple LFSR based on vectors of 0's and 1's.
;; Example usage: (def states (iterate 
;;                              (partial lfsr [1 4 5])
;;                              [1 0 0 1 0 1 0 1 0 1 1]))
;; (def bs (map first states)) ; <-- a sequence of supposedly random bits
;; 
(defn lfsr2
  "Applies an F2 LSFR specified by taps, a sequence of (zero-based) indexes,
  and bits, which should be a seed vector of 0's and 1's.  The result drops 
  the first element in bits, and tacks onto the end the bitwise xor of
  elements of bits at locations specified by taps.  That is, a new value
  is constructed from earlier values, and appended to the end (rhs) of the
  random bits, while the first bit in the random bits is removed, perhaps
  after having been examined."
  [taps bits]
  (conj (vec (rest bits)) ; alternatives: drop, pop, rest, next.  see below.
        (reduce 
          (fn [sum tap] (bit-xor sum (nth bits tap)))
          0 taps)))

;; pop strips the last element from a vector, or the first element
;; from a list.  conj adds to end of vector, or to front of list.
;; Other operations remove vector-ness, to one extent or another.
;; I want LIFO semantics, not stack semantics.
;;
;; Or, to put the new value on the front, so that the head of the
;; random sequence is on the right, I could use butlast and cons or conj.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experiments with matrix-based LFSRs

;; Note I use inner-product rather than mmul
;; Logically we need mmul, not inner-product, but the result is the same,
;; and mmul but not inner-product has a bug that turns everything
;; into doubles, rather than preserving Ratio types.

;(mx/set-current-implementation :persistent-vector)
;(mx/set-current-implementation :ndarray)
;(mx/set-current-implementation :vectorz)

(defn right-shift-mat
  [size shift]
  (mx/shift (mx/identity-matrix size) 0 shift))

(defn left-shift-mat
  [size shift]
   (mx/shift (mx/identity-matrix size) 1 shift))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Marsaglia's 32-bit xorshift PRNG from Kneusel pp. 50ff:

(def left13 (left-shift-mat 32 13))
(def right17 (right-shift-mat 32 17))
(def left5 (left-shift-mat 32 5))
(def i32 (mx/identity-matrix 32))

(def xorshift32-mat
  (mx/inner-product (mx/add i32 left13)
                    (mx/add i32 right17)
                    (mx/add i32 left5)))

(def kneusel-seed (mx/matrix (cb/convert-int-to-seq 2 2463534242)))

;; (inner-product kneusel-seed xorshift32) doesn't produce the 
;; correct result because it uses regular multiplication rather than
;; F2 operations.  I need an actual xor across the elements.

