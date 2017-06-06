(ns game.common.stats
  (:require
   [game.constants :as consts]
   [game.math :as math]
   [game.utils :refer :all]))

(def zero-stats
  (zipmap [:strength :agility :stamina :intelligence :wisdom :spirit :armor]
          (repeat 0)))

(def stats-per-level 5)

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

(defn through-armor [ac attackers-level]
  (math/expt 0.9502 (/ ac attackers-level)))

(defn base-that-gives-reduction [reduction]
  (math/expt (- 1 reduction) 0.1))

(defn random-damage [weapon-damage]
  (let [modify-damage #(* (% 1 consts/damage-random-portion) weapon-damage)]
    (math/round (rand-uniform (modify-damage -) (modify-damage +)))))

(defn actual-damage [char target]
  (* (through-armor (or (:armor target) 0) (:level char))
     (random-damage (:damage char))))

(defn expected-weapon-damage [level]
  level)

(defn bonus-damage-simple [power level]
  (let [scaled 0.5
        max-stats (* 50 stats-per-level)]
    (* 200 (+ 1
              (/ power max-stats (/ (- 1 scaled)))
              (/ power level stats-per-level (/ scaled))))))

(defn bonus-damage [power level]
  (let [flat-fraction 0.5
        scaled 0.5
        wd (expected-weapon-damage level)
        max-stats (* 50 stats-per-level)
        factor (+ (/ power max-stats (/ (- 1 scaled)))
                  (/ power level stats-per-level (/ scaled)))
        flat (* factor wd flat-fraction)
        factor (+ 1 (* factor (- 1 flat-fraction)))]
    {:factor factor :flat flat}))

(defn hit-chance [level target-level]
  (let [chance (+ 0.75 (* (- level target-level) 0.05))]
    (cond
      (> chance 1) 1
      (< chance 0.2) 0.2
      :else chance)))

(defn hit? [attacker target]
  (< (rand-uniform 1) (hit-chance (:level attacker) (:level target))))

(defn hitpoints [stamina level]
  (+ (* level 100) (* stamina 5)))

(defn mana [wisdom level]
  (+ (* level 10) (* wisdom 5)))

(defn hp-regen [level]
  (/ level 3.0))

(defn base-mana-regen [level]
  level)

(defn bonus-mana-regen [spirit level]
  (/ spirit stats-per-level))
