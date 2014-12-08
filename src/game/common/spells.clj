(ns game.common.spells
  (:use game.utils))

(defmacro make-map [& syms]
  (into {} (for [sym syms]
    [(keyword sym) sym])))

(defmacro spell-helper [& args]
  `(defn spell ~(vec args)
     (make-map ~@args)))

(spell-helper cast-time mana-cost cast-range cooldown target-type description)

(def spells
  {:regrowth (spell 0 10 3 2 :target "Heal over time")})
