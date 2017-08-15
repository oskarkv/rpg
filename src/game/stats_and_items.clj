(ns game.stats-and-items
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.constants :as consts]
   [game.hierarchies :as hier]
   [game.math :as math]
   [game.utils :refer :all]))

(def base-stats
  #{:strength :agility :stamina :intelligence :wisdom :spirit
    :armor :magic-resistance})

(def other-stats
  #{:attack-power :spell-power :haste :hp :mana :cooldown-reduction
    :resource-cost-reduction :spell-penetration})

(def all-stats (set/union base-stats other-stats))

(def zero-stats
  (zipmap [:strength :agility :stamina :intelligence :wisdom :spirit :armor]
          (repeat 0)))

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
;; should go to armor, withous any bonus armor (e.g. normal cloth robe's armor
;; compared to other stats)? Since it is possible to get bonus armor on items,
;; this number should probably be lower than 1 / NUMBER-OF-IMPORTANT-STATS. It
;; is like a "supply" of armor. If the supply is too high, the demand of bonus
;; armor will be lower. In that case, it would not feel like getting armor is a
;; choice. Keep in mind that it is not possible to base armor on all pieces of
;; gear.
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

(defn base-that-gives-reduction [reduction at-armor]
  (math/expt (- 1 reduction) (/ at-armor)))

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

(declare items)

(defn random-variables-with-mean
  "Returns num-vars random variables between 0 and 1, with average value avg."
  [avg num-vars]
  (when (pos? num-vars)
    (let [vars (repeatedly num-vars rand)
          vars-avg (/ (apply + vars) num-vars)
          adjusting-fn (fn [avg target]
                         (if (> avg target)
                           #(* (/ target avg) %)
                           #(+ % (* (- 1 %) (/ (- target avg) (- 1 avg))))))]
      (map (adjusting-fn vars-avg avg) vars))))

(defn rolls->stats
  "Takes a map of stats (from stat name to magnitude) and a set of rolls
   (numbers between 0 and 1) and modifies the stats of the map depending on the
   rolls and consts/stats-random-part."
  [base-stats rolls]
  (letfn [(rolls->stats-factors [vars]
            (map #(+ 1 (* consts/stats-random-part (- (* % 2) 1))) vars))]
    (into {} (map (fn [[stat magnitude] factor]
                    [stat (math/round (* factor magnitude))])
                  base-stats
                  (rolls->stats-factors rolls)))))

(defn roll-for-stats
  "Returns a new item with the stats randomly modified up or down a bit."
  [{id :id :as item}]
  (if-let [stats (get-in items [id :stats])]
    (let [rolls (random-variables-with-mean (rand) (count stats))
          actual-stats (rolls->stats stats rolls)]
      (assoc item :stats actual-stats))
    item))

(defn stack
  "Returns a vector of stack sizes that would result when trying to stack n
   items with a max stack size of max-stack-size."
  [n max-stack-size]
  (loop [left n stacks []]
    (if-not (pos? left)
      stacks
      (let [this-stack (min max-stack-size left)]
        (recur (- left this-stack) (conj stacks this-stack))))))

(defn unstack
  "Returns a seq of stacks of the given item. The seq will be longer than 1 if
   :quantity > :stackable."
  [{:keys [quantity id] :as item}]
  (let [allowed (or (get-in items [id :stackable]) 1)
        stacks (stack (or quantity 1) allowed)]
    (map (fn [n] (if (< 1 allowed)
                   (assoc item :quantity n)
                   (dissoc item :quantity)))
         stacks)))

(defn what-kind [thing]
  (condf thing
    hier/classes :classes
    hier/gear-slots :slots))

(defn make-stats-map [m]
  (let [{:keys [damage delay]} m
        stats (dissoc m :damage :delay)]
    (cond-> {:stats stats}
      damage (assoc :damage damage)
      delay (assoc :delay delay)
      (zero? (count stats)) (dissoc :stats))))

(defn all-info
  "Merges the full info of an item into a light version of it."
  [light-item]
  (merge (items (:id light-item)) light-item))

(defn concrete-slots [abstract-slots]
  (when abstract-slots
    (set (mapcat (fn [slot]
                   (if (hier/left-right-slots slot)
                     (hier/left-right slot)
                     [slot]))
                 abstract-slots))))

(defn item [name icon & info]
  (-> (apply merge-with set/union
             {:name name :icon icon}
             (for [e info]
               (condf e
                 map? (if (some #{:stackable :price} (keys e))
                        e
                        (make-stats-map e))
                 vector? {(what-kind (first e)) (set e)}
                 number? {:weight e}
                 hier/types {:type e}
                 keyword? {e true})))
    (update :slots concrete-slots)
    remove-map-nils))

(defn check-item [item]
  (let [{:keys [name slots type weight race class two-hand stackable]
         item-stats :stats}
        item]
    (every? identity
            [(string? name)
             (every? hier/classes class)
             (contains? (conj hier/types nil) type)
             (every? hier/gear-slots slots)
             (number? weight)
             (every? all-stats (keys item-stats))
             (contains? #{true nil false} two-hand)
             (or (not stackable) (number? stackable))])))

(defn report-item [item]
  (if (check-item item)
    true
    (do (throw-error item " is not legal") false)))

(defn check-items [items]
  (when (and (every? report-item items)
             (reduce (fn [name-set name]
                       (if (name-set name)
                         (do (throw-error "Item " name " already exists")
                             (reduced false))
                         (conj name-set name)))
                     #{} (map :name items)))
    items))

(defn correct-slot? [item path]
  (if (nil? item)
    true
    (let [slot (peek path)
          item-type (items (:id item))]
      (if (contains? hier/gear-slots slot)
        (contains? (:slots item-type) slot)
        true))))

(defn get-tooltip [item]
  (let [{:keys [type damage delay stats] :as item}
        (all-info item)
        list-fn (fn [k] (when (k item)
                          (str (name k) ": "
                               (str/join ", " (map name (k item))))))]
    (->$ [(:name item)
          (when type (name type))
          (when damage (str "damage / delay: " damage " / " delay))]
      (into (for [[s v] stats]
              (str (name s) ": " v)))
      (into (keep list-fn [:classes :slots :races]))
      (str/join "\n" $))))

(def items
  (check-items
   [(item "Leather Vest" "leather_armor.png" 5 [:chest] :leather {:armor 20})
    (item "Snake Skin" "snakeskin.png" 0.1 {:stackable 20})
    (item "Rusty Sword" "longsword.png" 5 {:damage 3 :delay 20}
          [:main-hand :off-hand] [:paladin :warrior])]))
