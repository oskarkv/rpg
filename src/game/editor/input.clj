(ns game.editor.input)

(defn load-key-bindings []
  [["cam-up" "w" :hold]
   ["cam-down" "s" :hold]
   ["cam-left" "a" :hold]
   ["cam-right" "d" :hold]
   ["perform-selected-action" "m-left" :tap]
   ["cancel" "m-right" :tap]])