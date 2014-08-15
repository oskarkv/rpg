(ns game.editor.editor-functions
  (:require (game.key-value-store [protocols :as kvs])
            (game.common [graphics :as gfx])
            (game [constants :as consts])))

(defn- set-spawn-id-ctr [start]
  (let [spawn-id-counter (atom start)]
    (defn next-spawn-id []
      (swap! spawn-id-counter inc))))

(defn init-editor [spawn-start-val]
  (let [mob-id-counter (atom 0)]
    (defn next-mob-id []
      (swap! mob-id-counter inc)))
  (set-spawn-id-ctr 0))

(defn create-spawn-location [game-state]
  (let [coords (gfx/get-target-coords)]
    (if coords
      {:new-game-state
       (assoc-in game-state
                 [:game-map :spawns (next-spawn-id)]
                 {:pos coords :type 0 :respawn-time 3})
       :event [:create-spawn-location]})))

(defn add-zone-mob [game-state mob-type]
  (assoc-in game-state [:game-map :mob-list (next-mob-id)]
            {:type mob-type :level {:min 1 :max 1}}))

(defn edit-zone-mob [game-state id & args]
  (assoc-in game-state [:game-map :mob-list id] args))

(defn remove-zone-mob [game-state id]
  (dissoc [:game-map :mob-list] id))

(defn select-spawn [game-state]
  (if-let [spawn
           (get-in game-state [:game-map :spawns (gfx/pick-target :spawns)])]
    spawn))

(defn cancel-action [game-state]
  {:new-game-state (dissoc game-state :current-action)
   :event nil})

(defn save-zone [game-state key-value-store]
  (kvs/store
    key-value-store (str consts/zone-folders "gfay") (:game-map game-state)))

(defn load-zone [game-state key-value-store]
  (let [zone (kvs/load key-value-store (str consts/zone-folders "gfay"))
        new-game-state (assoc game-state :game-map zone)]
    (set-spawn-id-ctr (apply max (keys (:spawns zone))))
    new-game-state))

