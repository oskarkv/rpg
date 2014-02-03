(ns game.client.hud
  (:require [game.common.core :as cc]
            [game.constants :as consts])
  (:import (tonegod.gui.core Screen)
           (tonegod.gui.controls.windows Window)
           (tonegod.gui.controls.text Label)
           (com.jme3.math Vector2f)
           (com.jme3.font BitmapFont$VAlign)
           (tonegod.gui.controls.extras ChatBox)))

(defn get-hp-text [char]
  (format "HP: %d/%d" (:hp char) (:max-hp char)))

(defn update-hp [label char]
  (.setText label (str (:name char) "\n" (get-hp-text char))))

(deftype HudSystem [gui-node screen chat-box self-label target-label]
  cc/Lifecycle
  (start [this]
    (.addControl gui-node screen))
  (stop [this])
  cc/Updatable
  (update [this game-state]
    (let [{:keys [own-id chars]} game-state
          self (chars own-id)
          target-id (:target self)
          target (chars target-id)]
      (if target
        (update-hp target-label target)
        (.setText target-label ""))
      (update-hp self-label self))))

(defn init-hud-system [app]
  (let [gui-node (.getGuiNode app)
        screen (Screen. app "gamedef/style_map.xml")
        pw consts/portrait-width
        ph consts/portrait-height
        ry consts/resolution-y
        ch consts/chat-height
        cw consts/chat-width
        gap 2
        psize (Vector2f. pw ph)
        self-label (Label. screen "self" (Vector2f. gap gap) psize)
        target-label (Label. screen "target" (Vector2f. (+ pw (* 2 gap)) gap)
                             psize)
        chat-box (proxy [ChatBox]
                   [screen "chat"
                    (Vector2f. gap (- ry ch gap))
                    (Vector2f. cw ch)]
                   (onSendMsg [msg]
                     (.receiveMsg this msg)))]
    (.setTextVAlign self-label BitmapFont$VAlign/Top)
    (.setTextVAlign target-label BitmapFont$VAlign/Top)
    (.addElement screen self-label)
    (.addElement screen target-label)
    (.addElement screen chat-box)
    (.setGlobalAlpha screen 1)
    (->HudSystem gui-node screen chat-box self-label target-label)))

