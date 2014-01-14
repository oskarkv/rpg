(ns game.editor.main
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

(defn create-editor-swing-app [app game-state-atom]
  (let [j-frame (gui/create-editor-swing-app app game-state-atom)]
    j-frame))

(defn init-editor []
  (let [game-map (game-map/load-game-map)
        key-value-store (kvs.core/construct-key-value-store)
        game-state-atom (atom {:game-map game-map
                               :key-value-store key-value-store})
        app (ec/create-editor-jme3-app game-state-atom key-value-store)
        j-frame (create-editor-swing-app app game-state-atom)]
    (efns/init-spawn-id-counter (apply max (keys (:spawns game-map))))
    (->Editor app j-frame key-value-store)))
