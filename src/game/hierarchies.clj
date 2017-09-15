(ns game.hierarchies
  (:require
   [clojure.set :as set]
   [game.math :as math]
   [game.utils :refer :all]))

(declare left-right-slots)

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

(defn to-actual-slots [slots]
  (->> slots
    (map (fn [slot]
           (if (left-right-slots slot)
             (left-right slot)
             slot)))
    flatten
    (into #{})))

(defs ;;; Slots
  armor-type-slots #{:head :face :chest :waist :legs :feet
                     :shoulders :arms :hands :neck :wrist}
  armor-slots (math/union armor-type-slots [:back])
  left-right-slots #{:finger :ear :wrist}
  jewelry-slots (math/union left-right-slots [:neck])
  body-slots (math/union armor-slots jewelry-slots [:back])
  held-slots #{:main-hand :off-hand :ranged}
  gear-slots (set/union body-slots
                        held-slots)
  ;; Used to decide the order in the gear panel
  gear-slots-vector (check-gear-slots
                     (to-actual-slots gear-slots)
                     [:left-ear :head :face :right-ear
                      :arms :shoulders :neck :back
                      :left-wrist :chest :waist :right-wrist
                      :left-finger :legs :feet :right-finger
                      :main-hand :off-hand :ranged :hands]))

(defs ;;; Types
  ;; Held types
  two-hander-types #{:2h-sword :2h-axe :2h-mace :2h-staff :polearm}
  one-hander-types #{:sword :axe :dagger :staff :mace :spear}
  melee-weapons (set/union one-hander-types two-hander-types)
  ranged-weapons #{:crossbow :bow :wand}
  all-weapons (set/union melee-weapons ranged-weapons)
  held-non-weapons #{:charm :caster-shield :tank-shield}
  all-held-types (set/union all-weapons held-non-weapons)
  ;; Body types
  armor-types #{:cloth :leather :mail :plate :cloak}
  body-types (set/union armor-types #{:jewelry}))

(def class->armor-type
  {:warrior :plate
   :wizard :cloth
   :druid :leather})

(def class->held-types
  {:warrior #{:sword :2h-sword :dagger :axe :2h-axe :mace :2h-mace
              :polearm :crossbow :bow :tank-shield}
   :wizard #{:sword :dagger :staff :2h-staff :wand :charm :caster-shield}
   :druid #{:sword :staff :2h-staff :mace :dagger :charm :caster-shield}})

(def slot->types
  (fmap
   (comp set flatten-more)
   (merge-with
    conj
    (zipmap gear-slots (repeat []))
    (zipmap armor-type-slots (repeat :armor))
    (zipmap jewelry-slots (repeat :jewelry))
    {:back :cloak}
    {:main-hand melee-weapons}
    {:off-hand one-hander-types}
    {:off-hand held-non-weapons}
    {:ranged ranged-weapons}
    {:ranged :charm})))

(let [armors (set/union #{:cloth :leather :mail :plate}
                        armor-slots)]
  (defn armor? [type-or-slot]
    (armors type-or-slot)))

(defn weapon? [type]
  (all-weapons type))

(defs ;;; Types that has to do with names
  sharp-weapons #{:sword :dagger :spear :axe :polearm}
  blunt-weapons #{:club :mace :staff}
  metal-weapons (set/union sharp-weapons #{:mace})
  weapon-types (set/union melee-weapons ranged-weapons)
  weapons #{:ranged :melee}
  types (set/union melee-weapons ranged-weapons armor-types
                    #{:caster-offhand}))

(def type-tags
  {;;; Blunt weapons
   :mace #{:metal :blunt :heavy}
   :staff #{:wood :blunt}
   :club #{:wood :blunt}
   ;;; Sharp weapons
   :sword #{:metal :sharp :blade}
   :axe #{:metal :sharp :blade}
   :dagger #{:metal :sharp :blade}
   :spear #{:metal :sharp}
   ;;; Ranged weapons
   :crossbor #{:ranged}
   :bow #{:ranged}
   :wand #{:magical}
   :mail #{:metal}
   :plate #{:metal}})

(def classes
  {;;; Tanks
   :warrior #{:tank :plate :melee}
   :paladin #{:tank :healer :plate :melee}
   :shadow-knight #{:tank :plate :melee}
   ;;; Casters
   :wizard #{:caster :cloth}
   :necromancer #{:caster :cloth}
   :enchanter #{:caster :cloth}
   ;;; Healers
   :druid #{:caster :healer :leather}
   :shaman #{:caster :healer :mail}
   :cleric #{:caster :healer :mail}
   ;;; Melee
   :rogue #{:leather :melee :agile}
   ;;; Ranged
   :ranger #{:leather :ranged :agile}})
