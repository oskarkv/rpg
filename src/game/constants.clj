(ns game.constants
  (:use game.utils))

(defmacro defconstants [& pairs]
  (assert-args (even? (count pairs)) "an even number of arguments")
  (when (seq pairs)
  `(do (def ~(with-meta (first pairs) {:const true}) ~(second pairs))
       (defconstants ~@(drop 2 pairs)))))

(defconstants
  attack-distance 1.5
  attack-delay-leeway 0.3)
