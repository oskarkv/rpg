(ns game.common.core-functions
  (:import (com.jme3.app SimpleApplication))
  (:require (game.common [core :as cc]
                         [items :as items]
                         [stats :as stats])
            [game.math :as math]
            [game.constants :as consts])
  (:use game.utils))

(let [id-counter (atom 0)]
  (defn get-new-id []
    (swap! id-counter inc)))

(defn map->msg [{:keys [type] :as m}]
  ((apply juxt #(cc/type->int (:type %)) (type cc/type->keys)) m))

(defn msg->map [msg]
  (let [type (cc/int->type (first msg))]
    (assoc (zipmap (type cc/type->keys) (rest msg))
           :type type)))

(defn complete-return-map [game-state result]
  (let [{:keys [new-game-state events event msgs msg]} result
        msgs (conj-some msgs msg)
        events (conj-some events event)
        new-game-state (or new-game-state game-state)]
    {:new-game-state new-game-state :events events :msgs msgs}))

(defn process-events [process-fn game-state input-events]
  (let [events-q (reduce conj clojure.lang.PersistentQueue/EMPTY input-events)]
    (loop [game-state game-state events-q events-q new-events [] new-msgs []]
      (if-let [e (first events-q)]
        (let [{:keys [new-game-state events msgs]}
              (complete-return-map
                game-state (process-fn game-state e))]
          (recur new-game-state (reduce conj (pop events-q) events)
                 (reduce conj new-events events)
                 (reduce conj new-msgs msgs)))
        {:new-game-state game-state :new-events new-events
         :new-msgs new-msgs}))))

; de 2 fnsen under är bara kvar pga editorn
(defn process-taps [game-state taps process-tap]
  (loop [[tap & more] taps game-state game-state events []]
    (if tap
      (let [{:keys [new-game-state event]} (process-tap game-state tap)]
        (recur more (or new-game-state game-state) (conj events event)))
      {:new-game-state game-state
       :events (remove nil? events)})))

(defn process-player-input [game-state key-state process-tap]
  (process-taps game-state (:taps key-state) process-tap))

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
                   ~all-events (concat ~all-events ~new-events)])]
         (call-update-fns ~new-game-state ~all-events ~hook-fn ~@(rest calls)))
      {:new-game-state game-state :events events})))

(defn calculate-move-time-delta [{:keys [last-move] :as game-state}]
  (let [curr-time (current-time-ms)
        time-delta (/ (- curr-time last-move) 1000.0)]
    {:new-game-state
     (assoc game-state :last-move curr-time :move-time-delta time-delta)}))

(defn player? [char]
  (= :player (:type char)))

(defn mob? [char]
  (= :mob (:type char)))

(defn sum-stats [gear]
  (merge stats/zero-stats
         (apply merge-with + (map :stats (vals gear)))))

(defn update-stats [{:keys [gear level] :as char}]
  (let [stats (sum-stats gear)
        {:keys [strength agility stamina wisdom intelligence spirit
                armor]} stats
        attack-power (+ strength agility)]
    (merge char
           {:max-hp (stats/hitpoints stamina level)
            :hp-regen (stats/hp-regen level)
            :armor armor
            :damage (int (stats/bonus-damage-simple attack-power level))})))

(defn move-toward-pos [{:keys [pos speed] :as char} time-delta target-pos]
  (let [dir (math/norm-diff target-pos pos)
        updated-pos (math/extrapolate-pos pos dir time-delta speed)
        new-dir (math/norm-diff target-pos updated-pos)
        dp (math/dot-product dir new-dir)]
    (if (> dp 0)
      (assoc char :pos updated-pos)
      (assoc char :pos target-pos))))

(defn pos-close-enough? [pos-1 pos-2 limit]
  (>= limit (math/distance pos-1 pos-2)))

(defn id-close-enough? [game-state id-1 id-2 limit]
  (let [get-pos (fn [id] (or (get-in game-state [:chars id :pos])
                             (get-in game-state [:corpses id :pos])))
        pos-1 (get-pos id-1)
        pos-2 (get-pos id-2)]
    (pos-close-enough? pos-1 pos-2 limit)))

(defn reset-queue [event-queue]
  (dosync
    (let [q @event-queue]
      (ref-set event-queue [])
      q)))

(defn queue-conj [queue item]
  (dosync (alter queue conj item)))

(defn possible-slot? [game-state from to]
  (let [item (get-in game-state from)]
    (items/correct-slot? item to)))

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
