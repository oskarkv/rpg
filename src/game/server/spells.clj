(ns game.server.spells
  (:require (game.common [core-functions :as ccfns])
            (game.server [base :as b]
                         [combat :as cb]))
  (:use game.utils))

(defn heal-over-time [game-state e]
  (let [{:keys [target ticks-left amount source]} e]
    (when (and (pos? ticks-left)
               (get-in game-state [:chars target]))
      (b/enqueue-msgs [(:player-ids game-state)
                       {:type :s-heal :target target :by source
                        :amount (cb/heal-amount
                                  (get-in game-state [:chars target])
                                  amount)}])
      {:new-game-state
       (update-in game-state [:chars target] cb/heal-char amount)
       :new-effect (-> e
                       (update-in [:decay-time] + 1500)
                       (update-in [:ticks-left] dec))})))

(defn regrowth [game-state caster target]
  (update-in game-state [:effects]
             assoc (b/new-game-id)
             {:decay-time (current-time-ms)
              :amount 20
              :target target
              :ticks-left 3
              :source caster
              :on-decay heal-over-time}))

(def spells (reduce (fn [m k] (assoc m k (resolve (symbol (name k)))))
                    {} [:regrowth]))

(defmethod b/process-event :c-cast-spell [game-state {:keys [id number target]}]
  (when (< number 8)
    (when-let [f (spells (get-in game-state [:chars id :spells number]))]
      (f game-state id target))))

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
