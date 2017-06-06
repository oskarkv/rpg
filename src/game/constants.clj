(ns game.constants
  (:require
   [game.utils :refer :all])
  (:import
   (com.jme3.input MouseInput)))

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
  regen-pools [:hp :mana]
  spell-slots 8
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
  tooltip-opacity 0.6
  inv-icon-size 40
  spell-icon-size 50
  header-height 23
  ;;; INPUT
  camera-rotation-speed 2
  mouse-left MouseInput/BUTTON_LEFT
  mouse-middle MouseInput/BUTTON_MIDDLE
  mouse-right MouseInput/BUTTON_RIGHT
  ;;; GENERAL
  zone-folders "zones/"
  editor-assets "assets/editor/"
  icons (str editor-assets "icons/")
  add-icon (str icons "add.png")
  delete-icon (str icons "delete.png")
  editor-toolbar (str editor-assets "toolbar/")
  spawn-button-icon (str editor-toolbar "red.png"))
