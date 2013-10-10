(ns game.client.input)

(defn load-key-bindings []
  [["forward" "w" :hold]
   ["back" "r" :hold]
   ["left" "a" :hold]
   ["right" "s" :hold]
   ["attack" "f" :tap]
   ["target" "m-left" :tap]])
