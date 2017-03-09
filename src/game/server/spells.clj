(ns game.server.spells
  (:require [game.constants :as const]
            (game.common [core-functions :as ccfns]
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
                       (update :decay-time + (int (* tick-time 1000)))
                       (update :ticks-left dec))})))

(defn heal-over-time [amount ticks tick-time]
  (fn [game-state caster target]
    (update game-state :effects
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
  (when-lets [_ (< number const/spell-slots)
              _ (get-in game-state [:chars target])
              path [:chars id :spells number]
              {:keys [spell last-cast]} (get-in game-state path)
              curr-time (current-time-ms)
              {:keys [cooldown effect-fn cast-range mana-cost]} (spells spell)
              respond #(b/enqueue-msgs [[id] {:type :s-spell-response
                                              :response %1 :number number}])]
    (cond
      (< curr-time (+ last-cast (* 1000 cooldown)))
      (respond :cooldown)
      (> mana-cost (get-in game-state [:chars id :mana]))
      (respond :out-of-mana)
      (not (ccfns/id-close-enough? game-state id target cast-range))
      (respond :out-of-range)
      :else
      (do (respond :ok)
          (b/enqueue-msgs [(:player-ids game-state)
                           {:type :s-spell-cast :by id :spell spell
                            :mana-cost mana-cost :target target}])
          (-> game-state
              (update-in [:chars id :mana] - mana-cost)
              (effect-fn id target)
              (assoc-in (conj path :last-cast) curr-time))))))

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
