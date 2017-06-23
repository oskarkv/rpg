(ns game.common.items
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.constants :as consts]
   [game.math :as math]
   [game.utils :refer :all]))

(declare items)

(defn left-right
  "Create left-slot and right-slot keywords."
  [slot]
  (map #(keyword (str % "-" (name slot))) ["left" "right"]))

(defn check-gear-slots
  "Check if gear-set and gear-vector have the same elements."
  [gear-set gear-vector]
  (if (= gear-set (set gear-vector))
    gear-vector
    (throw-error "gear-slots and gear-slots-vector do not match")))

(defs
  armor-slots #{:head :face :neck :chest :back :waist :legs :feet
                :shoulders :arms :wrist :hands :finger :ear}
  held-slots #{:main-hand :off-hand :ranged}
  left-right-slots #{:ear :finger :wrist}
  gear-slots (-> (set/union armor-slots held-slots)
               (set/difference left-right-slots)
               (set/union (mapcat left-right left-right-slots)))
  ;; Used to decide the order in the gear panel
  gear-slots-vector (check-gear-slots
                     gear-slots
                     [:left-ear :head :face :right-ear
                      :arms :shoulders :neck :back
                      :left-wrist :chest :waist :right-wrist
                      :left-finger :legs :feet :right-finger
                      :main-hand :off-hand :ranged :hands])
  equip-slots #{:held :armor}
  melee-weapons #{:sword :staff :club :dagger :spear :axe}
  ranged-weapons #{:crossbow :bow :wand}
  armor-types #{:cloth :leather :mail :plate :jewelry}
  weapons #{:ranged :melee}
  non-equip #{:trade :quest :consumable}
  abstract-slots (set/union armor-slots held-slots)
  types (set/union melee-weapons ranged-weapons non-equip armor-types)
  classes #{:warrior :rogue :paladin :shadow-knight :shaman :druid :cleric
            :wizard :enchanter :necromancer}
  races #{:gnome :dwarf :human :darkelf :troll :ogre}
  stats #{:damage :delay :hp :mana :armor})


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

(defn stack [n stacksize]
  (loop [left n stacks []]
    (if-not (pos? left)
      stacks
      (let [this-stack (min stacksize left)]
        (recur (- left this-stack) (conj stacks this-stack))))))

(defn unstack [{:keys [quantity id] :as item}]
  (let [allowed (or (get-in items [id :stackable]) 1)
        stacks (stack (or quantity 1) allowed)]
    (map (fn [n] (if (< 1 allowed)
                   (assoc item :quantity n)
                   (dissoc item :quantity)))
         stacks)))

(def item-type-hierarchy
  (-> (make-hierarchy)
    (derive-many armor-slots :armor)
    (derive-many held-slots :held)
    (derive-many equip-slots :equipment)
    (derive-many melee-weapons :melee)
    (derive-many ranged-weapons :ranged)
    (derive-many weapons :weapon)
    (derive-many non-equip :inv-types)))

(defn what-kind [thing]
  (condf thing
    classes :classes
    abstract-slots :slots
    races :races))

(defn make-stats-map [m]
  (let [{:keys [damage delay]} m
        stats (dissoc m :damage :delay)]
    (cond-> {:stats stats}
      damage (assoc :damage damage)
      delay (assoc :delay delay)
      (zero? (count stats)) (dissoc :stats))))

(defn all-info [light-item]
  (merge (items (:id light-item)) light-item))

(defn concrete-slots [abstract-slots]
  (when abstract-slots
    (set (mapcat (fn [slot]
                   (if (left-right-slots slot)
                     (left-right slot)
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
                 types {:type e}
                 keyword? {e true})))
    (update :slots concrete-slots)
    remove-map-nils))

(defn check-item [item]
  (let [{:keys [name slots type weight race class two-hand stackable]
         item-stats :stats}
        item]
    (every? identity
            [(string? name)
             (every? races race)
             (every? classes class)
             (contains? (conj types nil) type)
             (every? gear-slots slots)
             (number? weight)
             (every? stats (keys item-stats))
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
      (if (contains? gear-slots slot)
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
