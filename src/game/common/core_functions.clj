(ns game.common.core-functions
  (:import (com.jme3.system AppSettings)
           (com.jme3.app SimpleApplication))
  (:require [game.common.core :as cc]
            [game.math :as math])
  (:use [game.utils :as utils]))

(defn process-network-msgs [game-state {:keys [net-sys get-msg send-msg]}
                            process-fn & process-args]
  (dotimes [i 3]
    (cc/update net-sys nil))
  (loop [msg (get-msg) game-state game-state events []]
    (if msg
      (let [{:keys [new-game-state event]}
            (apply process-fn msg game-state process-args)]
        (cc/update net-sys nil)
        (recur (get-msg) new-game-state (conj events event)))
      {:new-game-state game-state :events (remove nil? events)})))

(defn create-jme3-app [start-fn stop-fn init-fn update-fn]
  (let [app
        (doto (proxy [SimpleApplication] []
                (simpleInitApp []
                  (init-fn this))
                (simpleUpdate [tpf]
                  (update-fn)))
          (.setShowSettings false)
          (.setSettings (AppSettings. true))
          (.setPauseOnLostFocus false))]
    (extend-type (type app)
      cc/Lifecycle
      (start [this]
        (start-fn this))
      (stop [this]
        (stop-fn this)))
    app))

(defmacro call-update-fns [game-state events & calls]
  (with-gensyms [new-game-state new-events new-event]
    (if (seq calls)
      `(let [{~new-game-state :new-game-state
              ~new-events :events
              ~new-event :event}
             (-> ~game-state ~(first calls))
             ~new-game-state (or ~new-game-state ~game-state)
             ~new-events (concat ~events ~new-events)
             ~new-events (if ~new-event
                           (conj ~new-events ~new-event)
                           ~new-events)]
         (call-update-fns ~new-game-state ~new-events ~@(rest calls)))
      {:new-game-state game-state :events events})))

(defn move-chars [game-state move-char-fn]
  {:new-game-state
   (update-in game-state [:chars] (partial fmap move-char-fn))})

(defn extrapolate-char
  ([{:keys [last-move pos] :as char}] (extrapolate-char char pos last-move))
  ([{:keys [move-dir speed] :as char} from-pos from-time]
   (let [curr-time (current-time-ms)
         extrap-time (/ (- curr-time from-time) 1000.0)
         new-pos (math/extrapolate-pos from-pos move-dir extrap-time speed)]
     (assoc char :pos new-pos :last-move curr-time))))
