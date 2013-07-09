(ns game.utils)

(defmacro error-printing-future [& body]
  `(future (try ~@body (catch Exception ~'e (.printStackTrace ~'e)))))
