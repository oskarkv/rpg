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
  #{:power :hp :mana :regen :hp-regen})

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

;; E.g. str, vit, sta, spi, mr, armor
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
(def armor-ratio (/ 1.0 6))

(def total-stats-per-level (* stats-per-level num-stats-maxed))

(def relative-gear-slot-value
  "How much stats value can each gear slot have, relative to a normal slot."
  (-> (zipmap hier/gear-slots (repeat 1))
    (assoc :chest 1.4
           :legs 1.3
           :arms 1.2
           :head 1.1)))

(def relative-two-hander-value
  "How much stats value a two-hander has compared to a main hand."
  1.75)

(def total-stats-value
  "How much stats value the complete set of gear has."
  (+ (apply + (vals relative-gear-slot-value))
     (apply + (vals (select-keys relative-gear-slot-value
                                 hier/left-right-slots)))))

(def stats-per-slot-per-level
  (/ total-stats-per-level total-stats-value))

(def power-stat
  {:warrior :str
   :wizard :int
   :druid :int})

(defn weapon-dps [level]
  (* 5 level))

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

(defn through-armor [armor attackers-level]
  (math/expt 0.9502 (/ armor attackers-level)))

(defn base-that-gives-reduction [reduction at-armor]
  (math/expt (- 1 reduction) (/ at-armor)))

(defn randomize-damage [damage]
  (let [adjust #(% 1 consts/damage-random-portion)]
    (math/round (* damage (rand-uniform (adjust -) (adjust +))))))

(defn actual-damage [attacker target damage]
  (* (through-armor (or (:armor target) 0) (:level attacker))
     (randomize-damage damage)))

(defn power [stats class]
  (+ (* 10 (:damage stats))
     ((power-stat class) stats)))

(defn hit-chance [attacker-level target-level]
  (let [diff (- attacker-level target-level)]
    (min 1 (max 0.3 (+ 0.75 (* 0.025 diff))))))

(defn hit? [attacker target]
  (< (rand-uniform 1) (hit-chance (:level attacker) (:level target))))

(defn hitpoints [vit level]
  (* vit (+ 5 level)))

(defn mana [wis]
  wis)

(defn energy [sta]
  sta)

(defn hp-regen [level]
  (inc (/ level 5.0)))

(defn regen [spi]
  (/ spi 100.0))

(def chance-denom
  "How many times more unlikely it is per 1 quality to get a drop."
  100)

(def base-drop-chance
  "What is the chance to get a quality 1 item."
  0.1)

(def max-quality 3.5)

(defn drop-chance
  "Returns the chance for an item of the given quality to drop from a normal
   mob. This is the inverse of drop-quality."
  [quality]
  (/ base-drop-chance (math/expt chance-denom (dec quality))))

(defn drop-quality
  "Returns the item quality that the given roll from 0 to 1 represents. A lower
   roll means better quality. This is the inverse of drop-chance."
  [roll-result]
  (if (zero? roll-result)
    max-quality
    (min max-quality (inc (/ (math/lg (/ base-drop-chance roll-result))
                             (math/lg chance-denom))))))
