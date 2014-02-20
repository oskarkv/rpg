(ns game.server.stats
  (:require [clojure.math.numeric-tower :as math])
  (:use game.utils))

(defn exp-per-mob [level]
  (let [level (dec level)]
    (+ 10 (* 5 level))))

(defn mobs-per-level [level]
  (let [level (dec level)]
    (+ 10 (* 5 level) (math/round (math/expt level 1.5)))))

(defn exp-to-level [level]
  (let [level (dec level)]
    (* (exp-per-mob level) (mobs-per-level level))))

(defn exp-modifier [mob-level player-level]
  (let [diff (- mob-level player-level)]
    (if (< diff -10)
      0
      (+ 1 (* 0.1 diff)))))

(defn exp-gained [mob-level player-level]
  (* (exp-modifier mob-level player-level) (exp-per-mob mob-level)))
