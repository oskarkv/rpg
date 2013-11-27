(ns game.common.core)

(def port 12345)

(def connect-msg [:connect])
(def disconnect-msg [:disconnect])

(let [types [(connect-msg 0) (disconnect-msg 0) :login :game-state :own-id
             :move :toggle-attack :target :attack]]
  (def type->int (zipmap types (range)))
  (def int->type (zipmap (range) types)))

(letfn [(message-transformer [transformer]
          (fn [msg] (update-in msg [0] transformer)))]
  (def type->int-in-msg (message-transformer type->int))
  (def int->type-in-msg (message-transformer int->type)))

(defprotocol Lifecycle
  (start [this])
  (stop [this]))

(defprotocol Updatable
  (update [this args]))
