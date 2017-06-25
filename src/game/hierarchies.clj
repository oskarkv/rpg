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
  left-right-slots #{:finger :ear :wrist}
  armor-slots #{:head :face :chest :waist :legs :feet
                :shoulders :arms :hands :neck :wrist}
  ;; Other slots are slots that do not necessarily have an armor class
  ;; (e.g. plate)
  other-slots (math/union left-right-slots [:neck :back])
  body-slots (math/union armor-slots other-slots)
  held-slots #{:main-hand :off-hand :ranged}
  gear-slots (set/union body-slots
                        held-slots)
  ;; With :left- and :right- versions
  actual-gear-slots (to-actual-slots gear-slots)
  ;; Used to decide the order in the gear panel
  gear-slots-vector (check-gear-slots
                     actual-gear-slots
                     [:left-ear :head :face :right-ear
                      :arms :shoulders :neck :back
                      :left-wrist :chest :waist :right-wrist
                      :left-finger :legs :feet :right-finger
                      :main-hand :off-hand :ranged :hands]))

(defs ;;; Types
  melee-weapons #{:sword :staff :club :dagger :spear :axe}
  ranged-weapons #{:crossbow :bow :wand}
  armor-types #{:cloth :leather :mail :plate :other}
  weapons #{:ranged :melee}
  types (set/union melee-weapons ranged-weapons armor-types))

(defs ;;; Classes
  cloth-classes #{:wizard :enchanter :necromancer}
  leather-classes #{:rogue :druid :ranger}
  mail-classes #{:shaman :cleric}
  plate-classes #{:warrior :shadow-knight :paladin}
  classes (set/union cloth-classes
                     leather-classes
                     mail-classes
                     plate-classes))
