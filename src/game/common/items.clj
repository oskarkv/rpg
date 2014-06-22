(ns game.common.items
  (:require [clojure.math.numeric-tower :as math]
            [game.constants :as consts])
  (:use game.utils
        clojure.set))

(declare items)

(defn left-right [slot]
  (map #(keyword (str % "-" (name slot))) ["left" "right"]))

(defn check-gear-slots [gear-set gear-vector]
  (if (= gear-set (set gear-vector))
    gear-vector
    (throw-error "gear-slots and gear-slots-vector do not match")))

(def-let
  [armor-slots #{:head :face :neck :chest :back :waist :legs :feet
                 :shoulders :arms :wrist :hands :finger :ear}
   held-slots #{:main-hand :off-hand :ranged}
   left-right-slots #{:ear :finger :wrist}
   gear-slots (-> (union armor-slots held-slots)
                  (difference left-right-slots)
                  (union (mapcat left-right left-right-slots)))
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
   abstract-slots (union armor-slots held-slots)
   types (union melee-weapons ranged-weapons non-equip armor-types)
   classes #{:paladin :druid :warrior :wizard}
   races #{:gnome :dwarf :human :darkelf :troll :ogre}
   stats #{:damage :delay :hp :mana :armor}])

(defn adjusting-fn [mean target]
  (if (> mean target)
    #(* (/ target mean) %)
    #(+ % (* (- 1 %) (/ (- target mean) (- 1 mean))))))

(defn random-variables-with-mean [mean num-vars]
  (let [vars (repeatedly num-vars rand)
        vars-mean (/ (apply + vars) num-vars)]
    (map (adjusting-fn vars-mean mean) vars)))

(defn random-variables->stats-factors [vars]
  (map #(+ 1 (* consts/stats-random-part (- (* % 2) 1))) vars))

(defn rolls->stats [base-stats rolls]
  (into {} (map (fn [[stat magnitude] factor]
                  [stat (math/round (* factor magnitude))])
                base-stats
                (random-variables->stats-factors rolls))))

(defn roll-for-stats [{id :id :as item}]
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
    (map (fn [n] (if (= 1 n)
                   (dissoc item :quantity)
                   (assoc item :quantity n)))
         stacks)))

(defn derive-many [hierarchy coll parent]
  (reduce (fn [h elem] (derive h elem parent))
          hierarchy coll))

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
  (let [{:keys [damage delay]} m]
    (cond-> {:stats (dissoc m :damage :delay)}
      damage (assoc :damage damage)
      delay (assoc :delay delay))))

(defn concrete-slots [abstract-slots]
  (when abstract-slots
    (set (mapcat (fn [slot]
                   (if (left-right-slots slot)
                     (left-right slot)
                     [slot]))
                 abstract-slots))))

(defn item [& info]
  (-> (apply merge-with union
             (for [e info]
               (condf e
                 string? {:name e}
                 map? (if (some #{:stackable :price} (keys e))
                        e
                        (make-stats-map e))
                 vector? {(what-kind (first e)) (set e)}
                 number? {:weight e}
                 types {:type e}
                 keyword? {e true})))
      (update-in [:slots] concrete-slots)
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

(def items (check-items
             [(item "Leather Vest" 5 [:chest] :leather {:armor 20})
              (item "Snake Skin" 0.1 {:stackable 20})
              (item "Rusty Sword" 5 [:main-hand :off-hand])]))
