(ns game.server.ai
  (:require (game [math :as gmath]
                  [constants :as consts])
            (game.server [pathfinding :as pf]))
  (:use game.utils))

(defn attack-nearest [game-state mob players]
  (let [{:keys [pos]} mob
        [nearest dist]
        (reduce (fn [[nearest-id dist :as old] [id char]]
                  (let [new-dist (gmath/distance (:pos char) pos)]
                    (if (< new-dist dist)
                      [id new-dist]
                      old)))
                [nil Integer/MAX_VALUE]
                players)]
    (if (< dist consts/attack-nearest-threshold)
      (assoc mob :target nearest :attacking true)
      (assoc mob :target nil))))

(defn decide-mob-path [game-state mob]
  (if (:target mob)
    (let [{:keys [target pos]} mob
          {:keys [terrain]} game-state
          target-pos (get-in game-state [:chars target :pos])
          path (pf/find-path terrain pos target-pos consts/player-radius)]
      (assoc mob :path path))
    mob))

(defn call-on-all-mobs [{:keys [chars] :as game-state} f]
  (let [mobs (filter (fn [[id char]] (= :mob (:type char))) chars)
        new-mobs (fmap #(f game-state %) mobs)]
    {:new-game-state (assoc game-state :chars (into chars new-mobs))}))

(defn decide-mob-actions [{:keys [chars player-ids] :as game-state}]
  (let [players (select-keys chars player-ids)
        ai-fn #(attack-nearest % %2 players)]
    (call-on-all-mobs game-state ai-fn)))

(defn decide-mob-paths [game-state]
  (call-on-all-mobs game-state decide-mob-path))