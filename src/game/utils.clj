(ns game.utils)

(defmacro error-printing-future [& body]
  ; *err* makes the stack trace print in Vim instead of in the lein repl cmd
  ; window, when running from Vim.
  `(future (try ~@body (catch Exception ~'e (.printStackTrace ~'e *err*)))))
