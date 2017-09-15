(ns game.common.core-functions
  (:require
   [game.common.core :as cc]
   [game.items :as items]
   [game.stats :as stats]
   [game.math :as math]
   [game.utils :refer :all])
  (:import
   (com.jme3.app SimpleApplication)))

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
    (make-map new-game-state events msgs)))

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
        (make-map game-state new-events new-msgs)))))

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
      (make-map game-state events))))

(defmacro call-update-fns* [game-state hook-fn & calls]
  (let [calls (map (fn [c] `((fn [gs#] (or (-> gs# ~c) gs#)))) calls)
        calls (interleave calls (repeat (list hook-fn)))]
    `(-> ~game-state
       ~@calls)))

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

(defn calculate-move-time-delta [{:keys [last-move] :as game-state}]
  (let [curr-time (current-time-ms)
        time-delta (/ (- curr-time last-move) 1000.0)]
    (assoc game-state :last-move curr-time :move-time-delta time-delta)))

(defn player? [char]
  (= :player (:type char)))

(defn mob? [char]
  (= :mob (:type char)))

(defn sum-stats [gear]
  (merge stats/zero-stats
         (apply merge-with + (map :stats (vals gear)))))

(defn update-stats [{:keys [gear level] :as char}]
  (let [stats (sum-stats gear)
        {:keys [str agi sta wis int spi armor]} stats]
    (merge char
           {:max-hp (stats/hitpoints sta level)
            :hp-regen (stats/hp-regen level)
            :max-mana (* level 50)
            :mana-regen (* level 2)
            :armor armor
            :damage (int (stats/attack-power stats))})))

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

(defn make-process-and-send-fn [process after event-queue]
  (fn [game-state]
    (let [get-events #(seq (reset-queue event-queue))
          ngs (loop [events (get-events) game-state game-state]
                (if (seq events)
                  (recur (get-events)
                         (process game-state events))
                  game-state))]
      (when after (after))
      ngs)))

(defn possible-slot? [game-state from to]
  (let [item (get-in game-state from)]
    (items/correct-slot? item to)))

(defn inv-swap [game-state [from to :as paths] enqueue id]
  (when (and (possible-slot? game-state from to)
             (possible-slot? game-state to from))
    (let [path-len (count (first paths))
          gear-index (- path-len 2)]
      (enqueue (when (some #{:gear} (map #(% gear-index) paths))
                 {:type :changed-gear :id id})))
    (swap-in game-state from to)))

(defn find-first-nil [v]
  (first (keep-indexed (fn [idx value] (if (nil? value) idx)) v)))

(defn divide-into-piles
  ([maxes total] (divide-into-piles maxes total []))
  ([maxes total acc]
   (if (seq maxes)
     (let [curr (first maxes)]
       (if (> total curr)
         (recur (rest maxes) (- total curr) (conj acc curr))
         (conj acc total)))
     (conj acc total))))

(defn stack-loot-dest [inv {:keys [quantity id] :as item}]
  (let [size (:stackable (items/all-info item))
        idx-place (->> (map-indexed vector inv)
                    (filter (fn [[idx item]]
                              (and item
                                   (== id (:id item))
                                   (< (:quantity item) size))))
                    (map (fn [[idx item]] [idx (- size (:quantity item))])))
        distribution (divide-into-piles (map second idx-place) quantity)
        will-use (zip (map first idx-place) distribution)
        need-extra-slot (< (count will-use) (count distribution))
        first-nil (find-first-nil inv)]
    (conj-some {:add-to will-use}
               (when (and need-extra-slot first-nil)
                 {:extra-slot [first-nil (peek distribution)]}))))

(defn loot-stack [game-state from-path to-inv-path]
  (let [{:keys [quantity] :as item} (get-in game-state from-path)
        {:keys [add-to extra-slot]} (stack-loot-dest
                                     (get-in game-state to-inv-path) item)
        [extra-idx extra-n] extra-slot
        add-to-stack (fn [gs [idx n]]
                       (update-in gs (conj to-inv-path idx :quantity) + n))
        ngs (reduce add-to-stack game-state add-to)
        total-looted (+ (reduce + (map second add-to))
                        (if extra-slot extra-n 0))]
    (cond-> (update-in ngs from-path
                       (if (= total-looted quantity)
                         (constantly nil)
                         #(update % :quantity - total-looted)))
      extra-slot (assoc-in (conj to-inv-path extra-idx)
                           (assoc item :quantity extra-n)))))

(defn loot-nonstack [game-state from-path to-inv-path]
  (when-let [to-idx (find-first-nil (get-in game-state to-inv-path))]
    (move-in game-state from-path (conj to-inv-path to-idx))))

(defn loot-item [game-state from-path to-inv-path]
  ((if (:quantity (get-in game-state from-path))
     loot-stack
     loot-nonstack)
   game-state from-path to-inv-path))

(defn ensure-to-stack [game-state from-path to-path]
  (if-not (get-in game-state to-path)
    (let [fake-item (assoc (get-in game-state from-path)
                           :quantity 0)]
      (assoc-in game-state to-path fake-item))
    game-state))

(defn do-move-quantity [game-state from-path to-path quantity]
  (let [game-state (ensure-to-stack game-state from-path to-path)
        [from-q-path to-q-path] (map #(conj % :quantity) [from-path to-path])
        [from-q to-q] (map #(get-in game-state %) [from-q-path to-q-path])
        max-stack (:stackable (items/all-info (get-in game-state to-path)))
        moved-q (min quantity from-q (- max-stack to-q))
        ngs (update-in game-state to-q-path + moved-q)]
    (if (<= from-q moved-q)
      (dissoc-in ngs from-path)
      (update-in ngs from-q-path - moved-q))))

(defn move-quantity [game-state from-path to-path quantity]
  (when (and (possible-slot? game-state from-path to-path)
             (not= from-path to-path))
    (do-move-quantity game-state from-path to-path quantity)))

(defn destroy-item [game-state path destroy-quantity]
  (if destroy-quantity
    (let [quant-path (conj path :quantity)
          quantity (get-in game-state quant-path)]
      (if (> quantity destroy-quantity)
        (update-in game-state quant-path - destroy-quantity)
        (dissoc-in game-state path)))
    (dissoc-in game-state path)))
