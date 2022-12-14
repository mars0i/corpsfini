;; simple matrix experiments related to quantum mechanics
(ns yo-qm
  (:require [clojure.core.matrix :refer :all]))

(def bra0 (matrix [[1 0]]))
(def ket0 (matrix [[1][0]]))

(def ket1 (matrix [[0][1]]))
(def bra1 (matrix [[0 1]]))

(comment 

  (pm bra0)
  (pm ket0)
  (pm bra1)
  (pm ket1)

  (pm (mmul bra0 ket0))
  (pm (mmul bra1 ket1))

  (pm (mmul ket0 bra0))
  (pm (mmul ket1 bra1))

  (pm ket0)
  (pm bra1)
  (pm (mmul ket0 bra1))

  (pm ket1)
  (pm bra0)
  (pm (mmul ket1 bra0))

  (pm (add (mmul ket0 bra1) (mmul ket1 bra0)))

  (pm (add (mmul ket0 bra1) (mmul ket1 bra0)))
)
