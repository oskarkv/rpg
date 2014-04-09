(ns game.common.core-functions
  (:import (com.jme3.app SimpleApplication))
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
  (loop [msg (get-msg) game-state game-state events []]
    (if msg
      (let [{:keys [new-game-state event]}
            (apply process-fn game-state msg process-args)]
        (recur (get-msg) new-game-state (conj events event)))
      {:new-game-state game-state :events (remove nil? events)})))

(defn create-jme3-app [start-fn stop-fn init-fn update-fn init-app-settings-fn]
  (let [app
        (init-app-settings-fn
          (proxy [SimpleApplication] []
            (simpleInitApp []
              (init-fn this))
            (simpleUpdate [tpf]
              (update-fn))))]
    (extend-type (type app)
      cc/Lifecycle
      (start [this]
        (start-fn this))
      (stop [this]
        (stop-fn this)))
    app))

(defn process-taps [game-state taps process-tap]
  (loop [[tap & more] taps game-state game-state events []]
    (if tap
      (let [{:keys [new-game-state event]} (process-tap game-state tap)]
        (recur more (or new-game-state game-state) (conj events event)))
      {:new-game-state game-state
       :events (remove nil? events)})))

(defn process-player-input [game-state key-state process-tap]
  (process-taps game-state (:taps key-state) process-tap))

(defn complete-return-map [game-state result]
  (let [{:keys [new-game-state events event]} result
        events (if event (conj events event) events)
        new-game-state (or new-game-state game-state)]
    {:new-game-state new-game-state :events events}))

(defmacro call-update-fns [game-state events hook-fn & calls]
  (with-gensyms [new-game-state new-events all-events]
    (if (seq calls)
      `(let [{~new-game-state :new-game-state ~new-events :events}
             (complete-return-map ~game-state (-> ~game-state ~(first calls)))
             ~all-events (concat ~events ~new-events)
             ~@(when hook-fn
                 `[{~new-game-state :new-game-state ~new-events :events}
                   (complete-return-map ~new-game-state
                                        (~hook-fn ~new-game-state ~new-events))
                   ~all-events (concat ~events ~new-events)])]
         (call-update-fns ~new-game-state ~all-events ~hook-fn ~@(rest calls)))
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
