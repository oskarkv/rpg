(ns game.utils)

(defmacro assert-args [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form)
                       " requires "
                       ~(second pairs)
                       " in " ~'*ns*
                       ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defmacro while-let [bindingvec & body]
  (assert-args
    (vector? bindingvec) "a vector for its binding"
    (= 2 (count bindingvec)) "exactly 2 forms in binding vector")
  (let [form (bindingvec 0) expr (bindingvec 1)]
    `(loop [result# ~expr]
       (when result#
         (let [~form result#]
           ~@body
           (recur ~expr))))))

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
