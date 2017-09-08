(ns game.stats
  (:require
   [clojure.set :as set]
   [game.constants :as consts]
   [game.hierarchies :as hier]
   [game.math :as math]
   [game.utils :refer :all]))

(def base-stats
  #{:str :agi :int :vit :wis :sta :spi :armor :mr})

(def other-stats
  #{:attack-power :spell-power :haste :hp :mana :cooldown-reduction
    :resource-cost-reduction :spell-penetration})

(def all-stats (set/union base-stats other-stats))

(def zero-stats
  (zipmap base-stats (repeat 0)))

(def armor-class-improvement
  "How much better each class of armor, from cloth to leather, for example, is
   than the previous."
  0.3)

(def armor-factor
  (fdefault
   (zipmap [:cloth :leather :mail :plate]
           (map #(+ 1 (* % armor-class-improvement)) (range 4)))
   1))

(def gear-rarity-improvement
  "How much better an item is per rarity level."
  0.4)

(def stats-per-level
  "How much of each base stat a player is expected to have per level."
  10)

(def num-stats-maxed 6)

;; Armor should be worth as much as other stats, that's what the item generator
;; thinks. If it actually is depends on how good we make armor and what players
;; think.
;;   Now, the current question is, how much of a piece of armor's stats
;; should go to armor, without any bonus armor (e.g. normal cloth robe's armor
;; compared to other stats)? Since it is possible to get bonus armor on items,
;; this number should probably be lower than 1 / NUMBER-OF-IMPORTANT-STATS. It
;; is like a "supply" of armor. If the supply is too high, the demand for bonus
;; armor will be lower. In that case, it would not feel like getting armor is a
;; choice. Keep in mind that it is not possible to get base armor on all pieces
;; of gear.
(def armor-ratio 0.125)

(def total-stats-per-level (* stats-per-level num-stats-maxed))

(def relative-gear-slot-value
  "How much stats value can each gear slot have, relative to a normal slot."
  (-> (zipmap hier/gear-slots (repeat 1))
    (assoc :chest 1.4
           :legs 1.3
           :arms 1.2
           :head 1.1)))

(def relative-2hander-value
  "How much stats value a 2-hander has compared to a main hand."
  1.75)

(def total-stats-value
  "How much stats value the complete set of gear has."
  (+ (apply + (vals relative-gear-slot-value))
     (apply + (vals (select-keys relative-gear-slot-value
                                 hier/left-right-slots)))))

(def stats-per-slot-per-level
  (/ total-stats-per-level total-stats-value))

(defn exp-per-mob [level]
  (+ 5 (* 5 level)))

(defn mobs-per-level [level]
  (+ 5 (* 5 level) (math/round (math/expt level 1.5))))

(defn exp-to-level [level]
  (* (exp-per-mob level) (mobs-per-level level)))

(defn exp-modifier [mob-level player-level]
  (let [diff (- mob-level player-level)]
    (max 0 (+ 1 (* 0.1 diff)))))

(defn exp-gained [mob-level player-level]
  (* (exp-modifier mob-level player-level) (exp-per-mob mob-level)))

(defn through-armor [ac attackers-level]
  (math/expt 0.9502 (/ ac attackers-level)))

(defn base-that-gives-reduction [reduction at-armor]
  (math/expt (- 1 reduction) (/ at-armor)))

(defn random-damage [damage]
  (let [modify-damage #(* (% 1 consts/damage-random-portion) damage)]
    (math/round (rand-uniform (modify-damage -) (modify-damage +)))))

(defn actual-damage [char target damage]
  (* (through-armor (or (:armor target) 0) (:level char))
     (random-damage damage)))

(defn expected-weapon-damage [level]
  (* 5 level))

(defn attack-power [stats]
  (+ (* 10 (:damage stats))
     (:str stats)))

(defn hit-chance [level target-level]
  (let [diff (- level target-level)]
    (min 1 (max 0.2 (+ 0.75 (* 0.05 diff))))))

(defn hit? [attacker target]
  (< (rand-uniform 1) (hit-chance (:level attacker) (:level target))))

(defn hitpoints [vit level]
  (+ 25 (* 0.1 vit (+ level 10))))

(defn mana [wis level]
  (+ (* level 10) (* wis 5)))

(defn hp-regen [level]
  (/ level 3.0))

(defn base-mana-regen [level]
  level)

(defn bonus-mana-regen [spi level]
  (/ spi stats-per-level))
