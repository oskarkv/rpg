(ns game.utils)

(defmacro error-printing-future [& body]
  ; *err* makes the stack trace print in Vim instead of in the lein repl cmd
  ; window, when running from Vim.
  `(future (try ~@body (catch Exception ~'e (.printStackTrace ~'e *err*)))))

(defn- private-symbol [sym]
  (with-meta sym (assoc (meta sym) :private true)))

(defmacro def-
  "Defines a private var."
  [name val]
  (list `def (private-symbol name) val))

(defmacro defmacro-
  "Same as defmacro but yields a private definition"
  [name & decls]
  (list* `defmacro (private-symbol name) decls))
