(ns game.client.input
  (:require [game.constants :as const]))

(defn load-key-bindings []
  (->
    [["forward" "w" :hold]
     ["back" "r" :hold]
     ["left" "a" :hold]
     ["right" "s" :hold]
     ["toggle-attack" "f" :tap]
     ["left-click" "m-left" :tap]
     ["right-click" "m-right" :tap]
     ["open-inv" "c" :tap]]
    (into (for [i (range const/spell-slots)]
            [(str "spell#" i) (str (inc i)) :tap]))))
