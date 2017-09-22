(ns game.common.core-functions
  (:require
   [game.common.core :as cc]
   [game.math :as math]
   [game.stats :as stats]
   [game.utils :refer :all]))

(let [id-counter (atom 0)]
  (defn get-new-id []
    (swap! id-counter inc)))

(defn map->msg [{:keys [type] :as m}]
  ((apply juxt #(cc/type->int (:type %)) (type cc/type->keys)) m))

(defn msg->map [msg]
  (let [type (cc/int->type (first msg))]
    (assoc (zipmap (type cc/type->keys) (rest msg))
           :type type)))

(defmacro call-update-fns [game-state hook-fn & calls]
  (let [calls (map (fn [c] `((fn [gs#] (or (-> gs# ~c) gs#)))) calls)
        calls (interleave calls (repeat (list hook-fn)))]
    `(-> ~game-state
       ~@calls)))

(defn calculate-move-time-delta [{:keys [last-move] :as game-state}]
  (let [curr-time (current-time-ms)
        time-delta (/ (- curr-time last-move) 1000.0)]
    (assoc game-state :last-move curr-time :move-time-delta time-delta)))

(defn sum-stats [gear]
  (merge stats/zero-stats
         (apply merge-with + (map :stats (vals gear)))))

(defn update-stats [{:keys [gear level class] :as char}]
  (let [stats (sum-stats gear)
        {:keys [str agi sta wis int spi armor]} stats]
    (merge char
           {:max-hp (stats/hitpoints sta level)
            :hp-regen (stats/hp-regen level)
            :max-mana (* level 50)
            :mana-regen (* level 2)
            :armor armor
            :damage (int (stats/power stats class))})))

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

(defn enqueue-fn [& queues]
  (fn [& es]
    (doseq [e es q queues]
      (some->> e (queue-conj q)))))

(defn make-process-events
  "Given a function that process one event, returns a function can process many
   events."
  [process-event]
  (fn [game-state events]
    (reduce (fn [gs e] (or (process-event gs e) gs))
            game-state events)))

(defn make-process-event-queue
  "Returns a function that accepts a game-state, and takes all events out of
   event-queue and calls (process game-state events) on them. Repeats until the
   event-queue has no events."
  [process event-queue]
  (fn [game-state]
    (let [get-events #(seq (reset-queue event-queue))]
      (loop [events (get-events) game-state game-state]
        (if (seq events)
          (recur (get-events)
                 (process game-state events))
          game-state)))))
