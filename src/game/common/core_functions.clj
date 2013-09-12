(ns game.common.core-functions
  (:require [game.common.core :as core]))

(defn process-network-msgs [game-state {:keys [net-sys get-msg send-msg]}
                            process-fn & process-args]
  (core/update net-sys nil)
  (loop [msg (get-msg) game-state game-state events []]
    (if msg
      (let [{:keys [new-game-state event]}
            (apply process-fn msg game-state process-args)]
        (core/update net-sys nil)
        (recur (get-msg) new-game-state (conj events event)))
      {:new-game-state game-state :events (remove nil? events)})))
