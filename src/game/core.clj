(ns game.core)

(def connect-msg :connect)
(def disconnect-msg :disconnect)

(let [types [connect-msg disconnect-msg :login]]
  (def types->ints (zipmap types (range)))
  (def ints->types (zipmap (range) types)))

(letfn [(message-transformer [transformer]
          (fn [msg]
            (if (vector? msg)
              (update-in [0] transformer)
              (transformer msg))))]
  (def type->int (message-transformer types->ints))
  (def int->type (message-transformer ints->types)))

(defprotocol Lifecycle
  (start [this])
  (stop [this]))

(defprotocol Updatable
  (update [this]))
