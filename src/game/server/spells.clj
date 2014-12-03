(ns game.server.spells
  (:require (game.common [core-functions :as ccfns])
            (game.server [base :as b]
                         [combat :as cb]))
  (:use game.utils))

(defn check-effects [game-state]
  (let [effects (:effects game-state)
        curr-time (current-time-ms)
        time-to-decay (fn [[id {:keys [decay-time]}]] (> curr-time decay-time))
        ids-effects (take-while time-to-decay effects)]
    (reduce
      (fn [gs [id e]]
        (let [f (:on-decay e)
              {:keys [new-game-state new-effect]} (f gs e)]
          (cond-> (or new-game-state gs)
            true (dissoc-in [:effects id])
            new-effect (assoc-in [:effects id] new-effect))))
      game-state
      ids-effects)))

(defn test-hot [game-state e]
  (let [{:keys [target ticks-left amount source]} e
        ngs (update-in game-state [:chars target] cb/heal-char amount)]
    (b/enqueue-msgs [(:player-ids game-state)
                     {:type :s-heal :target target :by source
                     :amount (cb/heal-amount (get-in game-state [:chars target])
                                             amount)}])
    {:new-game-state ngs
     :new-effect (when (pos? (:ticks-left e))
                   (-> e
                       (update-in [:decay-time] + 1500)
                       (update-in [:ticks-left] dec)))}))
