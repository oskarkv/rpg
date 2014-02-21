(ns game.constants
  (:use game.utils))

(defmacro defconstants [& pairs]
  (assert-args (even? (count pairs)) "an even number of arguments")
  (when (seq pairs)
  `(do (def ~(with-meta (first pairs) {:const true}) ~(second pairs))
       (defconstants ~@(drop 2 pairs)))))

(defconstants
  ;;; GAME SPECIFIC ;;;
  attack-distance 1
  attack-delay-leeway 0.3
  attack-nearest-threshold 3
  a*-heuristic-factor 1.2
  player-radius 0.3
  resolution-x 1024
  resolution-y 768
  portrait-height 100
  portrait-width 200
  chat-height 220
  chat-width 330)

(defconstants
  ;;; GENERAL ;;;
  zone-folders "zones/"

  ;;; EDITOR SPECIFIC ;;;
  editor-assets "assets/editor/"
  editor-toolbar (str editor-assets "toolbar/")

  ;;; TOOLBAR BUTTONS ;;;
  spawn-button-icon (str editor-toolbar "red.png"))
