(ns game.common.items
  (:require [clojure.math.numeric-tower :as math])
  (:use game.utils
        clojure.set))

(def-let
  [armor-slots #{:head :face :neck :chest :back :waist :legs :feet
                 :shoulders :arms :wrist :hands :finger :ear}
   held-slots #{:main-hand :off-hand :ranged}
   equip-slots #{:held :armor}
   melee-weapons #{:sword :staff :club :dagger :spear :axe}
   ranged-weapons #{:crossbow :bow :wand}
   armor-types #{:cloth :leather :mail :plate :jewelry}
   weapons #{:ranged :melee}
   non-equip #{:trade :quest :consumable}
   slots (union armor-slots held-slots)
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
    classes :class
    slots :slot
    races :race
    types :type))

(defn make-stats-map [m]
  (let [{:keys [damage delay]} m]
    (cond-> {:stats (dissoc m :damage :delay)}
      damage (assoc :damage damage)
      delay (assoc :delay delay))))

(defn item [& info]
  (apply merge-with union
         (for [e info]
           (condf e
             string? {:name e}
             map? (make-stats-map e)
             vector? {(what-kind (first e)) (set e)}
             number? {:weight e}
             types {:type e}
             keyword? {e true}))))

(defn check-item [item]
  (let [{:keys [name slot type weight race class two-hand stackable]
         item-stats :stats}
        item]
    (every? identity
            [(string? name)
             (every? races race)
             (every? classes class)
             (contains? (conj types nil) type)
             (every? slots slot)
             (number? weight)
             (every? stats (keys item-stats))
             (contains? #{true nil false} two-hand)
             (contains? #{true nil false} stackable)])))

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

(def items (check-items
             [(item "Leather Vest" 5 [:chest] :leather {:armor 4})
              (item "Snake Skin" 0.1 :stackable)
              (item "Rusty Sword" 5 [:main-hand :off-hand])]))
