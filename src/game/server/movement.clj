(ns game.server.movement
  (:require
   [game.common.core-functions :as ccfns]
   [game.constants :as consts]
   [game.math :as math]
   [game.server.base :as b]
   [game.utils :refer :all]))

(defmethod b/process-event :c-move [game-state {:keys [id pos move-dir]}]
  (update-in game-state [:chars id] merge
             {:recv-pos pos :move-dir (math/normalize move-dir)
              :recv-time (current-time-ms)}))

(defmethod b/process-event :chars-moved [game-state {ids :moved-ids}]
  (b/enqueue-msgs
   [(:player-ids game-state)
    {:type :s-move :positions
     (into {} (for [id ids
                    :let [pos (get-in game-state [:chars id :pos])]
                    :when pos]
                [id (map float pos)]))}]))

(defn move-player*
  "Moves a player along his move-dir. If the player has a recv-pos, move him an
   extra amount depending on the time the recv-pos was received (because it
   could have been almost a full frame earlier)."
  [char time-delta last-move]
  (let [{:keys [pos move-dir recv-pos recv-time speed]} char]
    (if recv-pos
      (recur (-> char (dissoc :recv-pos) (assoc :pos recv-pos))
             (/ (- last-move recv-time) 1000.0)
             last-move)
      (assoc char :pos (math/extrapolate-pos pos move-dir time-delta speed)))))

(defn move-mob* [{:keys [pos speed path target] :as mob} time-delta chars]
  (let [target-pos (get-in chars [target :pos])]
    (if (and path (> (math/distance pos target-pos) consts/attack-distance))
      (let [[next-point & path-left] path
            time-cost (/ (math/distance pos next-point) speed)]
        (if (< time-cost time-delta)
          (recur (assoc mob :pos next-point :path path-left)
                 (- time-delta time-cost) chars)
          (ccfns/move-toward-pos mob time-delta next-point)))
      mob)))

(defn moved-wrapper [move-fn]
  (fn [char & args]
    (let [pos (:pos char)
          new-char (apply move-fn char args)
          new-pos (:pos new-char)]
      (assoc new-char :moved-this-frame (not (rec== pos new-pos))))))

(def move-player (moved-wrapper move-player*))

(def move-mob (moved-wrapper move-mob*))

(defmacro defmovefn [name type move-fn & args]
  `(defn ~name [{:keys [~@args] :as game-state#}]
     (let [{group# ~type} (group-by (fn [[id# char#]] (:type char#))
                                    (:chars game-state#))
           move-char# #(~move-fn % ~@args)]
       (update game-state# :chars into (fmap move-char# group#)))))

(defmovefn move-players :player move-player move-time-delta last-move)

(defmovefn move-mobs :mob move-mob move-time-delta chars)

(defn check-if-moved [game-state]
  (when-let [moved (reduce (fn [moved [id char]]
                             (if (:moved-this-frame char)
                               (conj moved id)
                               moved))
                           nil
                           (:chars game-state))]
    (b/enqueue-events {:type :chars-moved :moved-ids moved})))
