(ns game.server.base
  (:require (game.common [core-functions :as ccfns]))
  (:use game.utils))

(def event-queue (ref []))

(def msg-queue (ref []))

(def enqueue-events (ccfns/enqueue-fn event-queue))

(def enqueue-msgs (ccfns/enqueue-fn msg-queue))

(let [game-id-counter (atom 0)
      net->game (atom {})
      game->net (atom {})]
  (defn new-game-id
    ([] (swap! game-id-counter inc))
    ([net-id]
     (let [game-id (swap! game-id-counter inc)]
       (swap! net->game assoc net-id game-id)
       (swap! game->net assoc game-id net-id)
       game-id)))
  (defn net-id->game-id [net-id]
    (if-let [game-id (@net->game net-id)]
      game-id
      (new-game-id net-id)))
  (defn game-id->net-id [game-id]
    (@game->net game-id)))

(defmulti process-event (fn [game-state event] (:type event)))

(defmethod process-event :default [game-state event])

(defn make-preparation-fn [ks]
  (fn [char]
    (let [prepared (select-keys char ks)
          pos (:pos prepared)]
      (cond-> prepared
        pos (assoc :pos (map float pos))))))

(def char-update-keys [:max-hp :max-mana :level])

(def keys-about-others
  (concat char-update-keys [:name :speed :pos :type :hp :mana :level]))

(def prepare-char-update (make-preparation-fn char-update-keys))

(def prepare-char-for-owner
  (make-preparation-fn
    (concat keys-about-others [:damage :delay :exp :inv :gear :spells])))

(def prepare-char-for-sending
  (make-preparation-fn keys-about-others))

(def prepare-corpse-for-sending
  (make-preparation-fn [:name :pos :type]))

(defn prepare-chars-for-sending [m]
  (fmap prepare-char-for-sending m))

(defn prepare-corpses-for-sending [m]
  (fmap prepare-corpse-for-sending m))

(defn prepare-for-sending-to [id game-state]
  (let [player (prepare-char-for-owner (get-in game-state [:chars id]))]
    (-> game-state
        (update-in [:chars] prepare-chars-for-sending)
        (update-in [:corpses] prepare-corpses-for-sending)
        (select-keys [:chars :corpses :terrain])
        (assoc-in [:chars id] player))))

(defn update-player [game-state id]
  (let [ngs (update-in game-state [:chars id] ccfns/update-stats)]
    (enqueue-msgs [(:player-ids game-state)
                   {:id id
                    :type :s-char-update
                    :updated (prepare-char-update (get-in ngs [:chars id]))}])
    ngs))
