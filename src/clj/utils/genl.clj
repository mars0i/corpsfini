;; general-purpose utilities
(ns utils.genl)


;; Alternative to (reduce f (repeat init n))
(defn iter 
  "Iteratively apply f to init n times without using a seq."
  [f init n]
  (loop [n n
         acc init]
    (if (<= n 0)
      acc
      (recur (dec n) (f acc)))))


;; Doesn't work:
;(defmacro add-to-docstr
;  "Appends string addlstr onto end of existing docstring for symbol sym.
;  (Tip: Consider beginning addlstr with \"\\n  \".)"
;  [sym addlstr] 
;  `(alter-meta! #'~sym update-in [:doc] str ~addlstr))
