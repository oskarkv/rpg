(ns game.editor.main
  (:import  (java.util LinkedList))
  (:require (game [game-map :as game-map])
            (game.common [core :as cc])
            (game.editor [core :as ec]
                         [gui :as gui]
                         [editor-functions :as efns])
            (game.key-value-store [core :as kvs.core])))

(defrecord Editor [app j-frame key-value-store]
  cc/Lifecycle
  (start [this]
    (cc/start key-value-store)
    (cc/start app)
    (cc/start j-frame)
    this)
  (stop [this]
    (cc/stop app)
    this))

(defn create-editor-swing-app [app game-state-atom event-queue]
  (let [j-frame (gui/create-editor-swing-app app game-state-atom event-queue)]
    j-frame))

(defn init-editor []
  (let [game-map (game-map/load-game-map)
        key-value-store (kvs.core/construct-key-value-store)
        game-state-atom (atom {:game-map game-map
                               :key-value-store key-value-store})
        event-queue (LinkedList.)
        app (ec/create-editor-jme3-app game-state-atom
                                       event-queue key-value-store)
        j-frame (create-editor-swing-app app game-state-atom event-queue)]
    (efns/init-editor (apply max (keys (:spawns game-map))))
    (->Editor app j-frame key-value-store)))
