(ns game.constants
  (:use game.utils))

(defmacro defconstants [& pairs]
  (assert-args (even? (count pairs)) "an even number of arguments")
  (when (seq pairs)
  `(do (def ~(with-meta (first pairs) {:const true}) ~(second pairs))
       (defconstants ~@(drop 2 pairs)))))

(defconstants
  ;;; GAME
  attack-distance 1
  loot-distance 1
  attack-delay-leeway 0.3
  attack-nearest-threshold 3
  a*-heuristic-factor 1.2
  player-radius 0.3
  stats-random-part 0.1
  corpse-decay-time 10000 ;ms
  dir-update-interval 50
  damage-random-portion 0.75
  exp-bonus-factor 15
  regen-interval 1
  ;;; GRAPHICS
  resolution-x 1024
  resolution-y 768
  ;;; HUD
  portrait-height 100
  portrait-width 200
  chat-height 220
  chat-width 330
  tooltip-margin 10
  tooltip-text-size 20
  icon-size 40
  icon-gap 5
  header-height 23
  ;;; INPUT
  camera-rotation-speed 2
  ;;; GENERAL
  zone-folders "zones/"
  editor-assets "assets/editor/"
  icons (str editor-assets "icons/")

  add-icon (str icons "add.png")
  delete-icon (str icons "delete.png")
  editor-toolbar (str editor-assets "toolbar/")
  spawn-button-icon (str editor-toolbar "red.png"))
