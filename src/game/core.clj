(ns game.core)

(def port 12345)

(def connect-msg [:connect])
(def disconnect-msg [:disconnect])

(let [types [(connect-msg 0) (disconnect-msg 0) :login]]
  (def types->ints (zipmap types (range)))
  (def ints->types (zipmap (range) types)))

(letfn [(message-transformer [transformer]
          (fn [msg] (update-in msg [0] transformer)))]
  (def type->int (message-transformer types->ints))
  (def int->type (message-transformer ints->types)))

(defprotocol Lifecycle
  (start [this])
  (stop [this]))

(defprotocol Updatable
  (update [this]))
