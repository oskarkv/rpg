(ns game.server.spells
  (:require (game.common [core-functions :as ccfns]
                         [spells :as comsp])
            (game.server [base :as b]
                         [combat :as cb]))
  (:use game.utils))

(defn heal-over-time-tick [game-state e]
  (let [{:keys [target ticks-left amount source tick-time]} e]
    (when (and (pos? ticks-left)
               (get-in game-state [:chars target]))
      (b/enqueue-msgs [(:player-ids game-state)
                       {:type :s-heal :target target :by source
                        :amount (cb/amount-to-give
                                  (get-in game-state [:chars target])
                                  :hp
                                  amount)}])
      {:new-game-state
       (update-in game-state [:chars target] cb/give-char :hp amount)
       :new-effect (-> e
                       (update-in [:decay-time] + (int (* tick-time 1000)))
                       (update-in [:ticks-left] dec))})))

(defn heal-over-time [amount ticks tick-time]
  (fn [game-state caster target]
    (update-in game-state [:effects]
               assoc (b/new-game-id)
               {:decay-time (current-time-ms)
                :amount amount
                :target target
                :ticks-left ticks
                :tick-time tick-time
                :source caster
                :on-decay heal-over-time-tick})))

(defn make-spell-effects [& pairs]
  (into {} (for [[k v] (partition 2 pairs)]
             [k {:effect-fn v}])))

(def spell-effects
  (make-spell-effects
    :regrowth (heal-over-time 10 3 1.5)))

(def spells (merge-with merge comsp/spells spell-effects))

(defmethod b/process-event :c-cast-spell [game-state {:keys [id number target]}]
  (when (< number 8)
    (when-let [f (some-> (get-in game-state [:chars id :spells number])
                         spells :effect-fn)]
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
