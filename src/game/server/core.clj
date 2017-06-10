(ns game.server.core
  (:require
   [clojure.data.priority-map :as pm]
   [game.common.core :as cc]
   [game.common.core-functions :as ccfns]
   [game.common.items :as items]
   [game.constants :as const]
   [game.dungeon-generator :as dg]
   [game.game-map :as gmap]
   [game.key-value-store.core :as kvs.core]
   [game.key-value-store.protocols :as kvs]
   [game.networking.core :as net]
   [game.server.ai :as ai]
   [game.server.base :as b]
   [game.server.combat :as cb]
   [game.server.mobs-and-looting :as ml]
   [game.server.movement :as mv]
   [game.server.spells :as sp]
   [game.utils :refer :all]))

(defn new-player [username spawn-pos]
  (->
    {:name username
     :speed 2
     :pos spawn-pos
     :bind-spot spawn-pos
     :move-dir [0 0]
     :type :player
     :attacking false
     :damage 60
     :delay 1
     :last-attack 0
     :level 1
     :exp 0
     :inv (-> (vec (repeat 10 nil))
            (assoc-in [0] {:stats {:armor 20}, :id 0}))
     :gear (zipmap items/gear-slots (repeat nil))
     :spells (-> (vec (repeat const/spell-slots nil))
               (assoc-in [0] {:spell :regrowth :last-cast 0}))
     :effects []}
    ccfns/update-stats
    (as-> c
      (assoc c :hp (:max-hp c))
      (assoc c :mana (:max-mana c)))))

(defmethod b/process-event :c-login [game-state event]
  (let [key-value-store (:kvs game-state)
        {:keys [id username password]} event
        player (or (kvs/load key-value-store username)
                   (new-player username
                               (:player-spawn game-state)))
        old-players (:player-ids game-state)
        new-game-state (-> game-state
                         (assoc-in [:chars id] player)
                         (update :player-ids conj id))
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
      (mv/move-players)
      (ai/decide-mob-actions)
      (ai/decide-mob-paths)
      (mv/move-mobs)
      (mv/check-if-moved)
      (cb/let-chars-attack)
      (cb/regen-chars)
      (sp/check-effects)
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
  (->$ {:chars {} :player-ids #{} :corpses (pm/priority-map-keyfn :decay-time)
        :last-move (current-time-ms) :last-regen (current-time-ms)
        :effects (pm/priority-map-keyfn :decay-time)}
    (merge $ (gmap/load-game-map (dg/make-round-rooms 2 10 10 0.45)))
    (assoc $ :to-spawn (create-to-spawn-queue (:spawns $)))))

(defn init-server [port]
  (let [game-state (create-game-state)
        stop? (atom false)
        networking-system (net/init-server-net-sys
                           port b/net-id->game-id b/game-id->net-id)
        key-value-store (kvs.core/construct-key-value-store)]
    (->Server networking-system key-value-store
              (assoc game-state :kvs key-value-store) stop?)))
