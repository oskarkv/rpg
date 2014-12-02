(ns game.server.core
  (:require [clojure.data.priority-map :as pm]
            [game.networking.core :as net]
            (game.common [core :as cc]
                         [core-functions :as ccfns]
                         [items :as items])
            (game.key-value-store [core :as kvs.core]
                                  [protocols :as kvs])
            (game [game-map :as gmap]
                  [constants :as consts])
            (game.server [ai :as ai]
                         [base :as b]
                         [mobs-and-looting :as ml]
                         [combat :as cb]
                         [movement :as move]
                         [inventory :as inv]))
  (:use game.utils))

(defn new-player [username]
  (ccfns/update-stats
    {:name username
     :speed 2
     :pos [1 1]
     :bind-spot [1 1]
     :move-dir [0 0]
     :type :player
     :attacking false
     :hp 100
     :max-hp 100
     :damage 60
     :delay 1
     :last-attack 0
     :level 1
     :exp 0
     :inv (-> (vec (repeat 10 nil))
              (assoc-in [0] {:stats {:armor 20}, :id 0}))
     :gear (zipmap items/gear-slots (repeat nil))}))

(defmethod b/process-event :c-login [game-state event]
  (let [key-value-store (:kvs game-state)
        {:keys [id username password]} event
        player (or (kvs/load key-value-store username)
                   (new-player username))
        old-players (:player-ids game-state)
        new-game-state (-> game-state
                           (assoc-in [:chars id] player)
                           (update-in [:player-ids] conj id))
        gs-for-entrant (b/prepare-for-sending-to id new-game-state)]
    (b/enqueue-msgs [[id] {:type :s-game-state :game-state gs-for-entrant}]
                    [[id] {:type :s-own-id :id id}]
                    [old-players
                     {:type :s-spawn-player :id id
                      :player (b/prepare-char-for-sending player)}])
    new-game-state))

(defn get-network-events [_ net-sys]
  (apply b/enqueue-events (cc/get-events net-sys)))

(defn make-process-and-send-fn [networking-system]
  (ccfns/make-process-and-send-fn
    (fn [game-state events]
      (reduce (fn [gs e] (or (b/process-event gs e) gs))
              game-state events))
    #(cc/update networking-system (ccfns/reset-queue b/msg-queue))
    b/event-queue))

(defn main-update [game-state net-sys]
  (let [hook (make-process-and-send-fn net-sys)]
    (ccfns/call-update-fns* game-state hook
      (get-network-events net-sys)
      (ccfns/calculate-move-time-delta)
      (move/move-players)
      (ai/decide-mob-actions)
      (ai/decide-mob-paths)
      (move/move-mobs)
      (move/check-if-moved)
      (cb/let-chars-attack)
      (cb/regen-chars)
      (ml/check-spawns)
      (ml/check-corpses))))

(defrecord Server [net-sys key-value-store game-state stop?]
  cc/Lifecycle
  (start [this]
    (cc/start net-sys)
    (cc/start key-value-store)
    (start-new-thread "server"
      ((fn [game-state]
         (if @stop?
           nil
           (recur (take-at-least-ms 100
                    (main-update game-state net-sys)))))
       game-state))
    this)
  (stop [this]
    (reset! stop? true)
    (cc/stop key-value-store)
    (cc/stop net-sys)
    this))

(defn create-to-spawn-queue [spawns]
  (apply pm/priority-map (interleave (keys spawns) (repeat 0))))

(defn create-game-state []
  (-> {:chars {} :player-ids #{} :corpses (pm/priority-map-keyfn :decay-time)
       :last-move (current-time-ms) :last-regen (current-time-ms)}
      (merge (gmap/load-game-map))
      (as-> gs
        (assoc gs :to-spawn (create-to-spawn-queue (:spawns gs))))))

(defn init-server [port]
  (let [game-state (create-game-state)
        stop? (atom false)
        networking-system (net/init-server-net-sys
                            port b/net-id->game-id b/game-id->net-id)
        key-value-store (kvs.core/construct-key-value-store)]
    (->Server networking-system key-value-store
              (assoc game-state :kvs key-value-store) stop?)))
