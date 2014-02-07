(ns game.common.core-functions
  (:import (com.jme3.system AppSettings)
           (com.jme3.app SimpleApplication))
  (:require [game.common.core :as cc]
            [game.math :as math]
            [game.constants :as consts])
  (:use [game.utils :as utils]))

(defn map->msg [{:keys [type] :as m}]
  ((apply juxt #(cc/type->int (:type %)) (type cc/type->keys)) m))

(defn msg->map [msg]
  (let [type (cc/int->type (first msg))]
    (assoc (zipmap (type cc/type->keys) (rest msg))
           :type type)))

(defn process-network-msgs
  [game-state {:keys [net-sys get-msg send-msg]} process-fn & process-args]
  (dotimes [i 3]
    (cc/update net-sys nil))
  (loop [msg (get-msg) game-state game-state events []]
    (if msg
      (let [{:keys [new-game-state event]}
            (apply process-fn game-state msg process-args)]
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
          (.setSettings
            (doto (AppSettings. true)
              (.setResolution consts/resolution-x consts/resolution-y)))
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

(defn calculate-move-time-delta [{:keys [last-move] :as game-state}]
  (let [curr-time (current-time-ms)
        time-delta (/ (- curr-time last-move) 1000.0)]
    {:new-game-state
     (assoc game-state :last-move curr-time :move-time-delta time-delta)}))

(defn move-toward-pos [{:keys [pos speed] :as char} time-delta target-pos]
  (let [dir (math/norm-diff target-pos pos)
        updated-pos (math/extrapolate-pos pos dir time-delta speed)
        new-dir (math/norm-diff target-pos updated-pos)
        dp (math/dot-product dir new-dir)]
    (if (> dp 0)
      (assoc char :pos updated-pos)
      (assoc char :pos target-pos))))
