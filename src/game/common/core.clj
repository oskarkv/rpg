(ns game.common.core)

(def port 12345)

(def connect-msg {:type :connect})
(def disconnect-msg {:type :disconnect})

(def type->keys
  ;; c-msgs can't have an :id key, because the
  ;; server adds one when it receives the msg
  {:connect nil :disconnect nil
   :c-login [:username :password]
   :c-move [:pos :move-dir]
   :c-target [:target]
   :c-toggle-attack []
   :s-attack [:target :damage]
   :s-game-state [:game-state]
   :s-login [:id :player]
   :s-move [:id :positions]
   :s-own-id [:id]
   :s-spawn-mobs [:mobs]
   :s-char-death [:id]
   :s-spawn-player [:id-char]})

(let [types (-> (keys type->keys) sort)]
  (def type->int (zipmap types (range)))
  (def int->type (zipmap (range) types)))

(defprotocol Lifecycle
  (start [this])
  (stop [this]))

(defprotocol Updatable
  (update [this args]))
