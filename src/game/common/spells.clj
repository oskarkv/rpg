(ns game.common.spells
  (:require
   [game.utils :refer :all]))

(defmacro spell-helper [& args]
  `(defn ~'spell ~(vec args)
     (make-map ~@args)))

(spell-helper mana-cost cast-time cast-range cooldown target-type description)

(def spells
  {:regrowth (spell 10 0 3 2 :target "Heal over time")})
