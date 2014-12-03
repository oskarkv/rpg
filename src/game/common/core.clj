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
   :c-loot-corpse [:corpse-id]
   :c-loot-item [:from-path]
   :c-rearrange-inv [:paths]
   :c-move-quantity [:from-path :to-path :quantity]
   :c-quit-looting [:ids]
   :c-destroy-item [:path :quantity]
   :c-cast-spell [:number :target]
   :s-attack [:target :damage :hit]
   :s-game-state [:game-state]
   :s-move [:positions]
   :s-own-id [:id]
   :s-spawn-mobs [:mobs]
   :s-char-death [:id]
   :s-spawn-player [:id :player]
   :s-spawn-corpse [:id :corpse]
   :s-loot [:corpse-id :drops]
   :s-loot-item-ok [:from-path]
   :s-item-looted [:from-path :by :left]
   :s-decay-corpses [:ids]
   :s-hp-update [:id-hp-vecs]
   :s-heal [:target :by :amount]
   :s-char-update [:updated :id]})

(let [types (-> (keys type->keys) sort)]
  (def type->int (zipmap types (range)))
  (def int->type (zipmap (range) types)))

(defprotocol Lifecycle
  (start [this])
  (stop [this]))

(defprotocol Updatable
  (update [this args]))

(defprotocol EventsProducer
  (get-events [this]))
