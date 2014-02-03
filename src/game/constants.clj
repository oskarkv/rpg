(ns game.constants
  (:use game.utils))

(defmacro defconstants [& pairs]
  (assert-args (even? (count pairs)) "an even number of arguments")
  (when (seq pairs)
  `(do (def ~(with-meta (first pairs) {:const true}) ~(second pairs))
       (defconstants ~@(drop 2 pairs)))))

(defconstants
  attack-distance 1
  attack-delay-leeway 0.3
  attack-nearest-threshold 4
  a*-heuristic-factor 1.2
  player-radius 0.3
  resolution-x 1024
  resolution-y 768
  portrait-height 100
  portrait-width 200
  chat-height 220
  chat-width 330)
