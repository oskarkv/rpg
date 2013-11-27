(ns game.constants
  (:use game.utils))

(defmacro def-constants [& pairs]
  (assert-args (even? (count pairs)) "an even number of arguments")
  (when (seq pairs)
  `(do (def ~(with-meta (first pairs) {:const true}) ~(second pairs))
       (def-constants ~@(drop 2 pairs)))))

(def-constants
  attack-distance 1.5
  attack-delay-leeway 0.3)
