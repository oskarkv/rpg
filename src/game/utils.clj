(ns game.utils
  (:import java.util.Random))

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
  ;; *err* makes the stack trace print in Vim instead of in the lein repl cmd
  ;; window, when running from Vim.
  `(future (try ~@body (catch Exception e# (.printStackTrace e# *err*)))))

(defmacro start-new-thread [name & body]
  `(.start (Thread. (fn [] ~@body) ~name)))

(defmacro with-gensyms [syms & body]
  (assert-args
    (vector? syms) "a vector of symbols as it's first argument"
    (every? symbol? syms) "a vector of symbols as it's first argument")
  `(let [~@(apply concat (for [sym syms]
                           [sym `(gensym ~(str sym))]))]
     ~@body))

(defn current-thread-id [msg]
  (.getId (Thread/currentThread)))

(defn current-thread-name []
  (.getName (Thread/currentThread)))

(defn current-time-ms []
  (System/currentTimeMillis))

(defn- private-symbol [sym]
  (with-meta sym (assoc (meta sym) :private true)))

;; We don't want defprivatedef to be public.
(defn- defprivatedef [name deffer]
  (eval `(defmacro ~name [inner-name# & rest#]
           (list* ~deffer (private-symbol inner-name#) rest#))))

(defprivatedef 'def- '`def)
(defprivatedef 'defmacro- '`defmacro)

(defmacro deftype- [name & decls]
  (let [constructor (symbol (str "->" name))]
    `(do (deftype ~name ~@decls)
         ;; The metadata map will be evaluated by def,
         ;; so we have to quote the arglists which can
         ;; contain arbitrary symbols.
         (let [meta# (meta #'~constructor)
               args# (:arglists meta#)
               meta# (assoc meta# :arglists `'~args#)
               sym# (with-meta '~constructor
                               (assoc meta# :private true))]
           (eval `(def ~sym# ~~constructor))))))

(defmacro debug [x]
  `(let [x# ~x]
     (do (println (with-out-str (println '~x "=") (clojure.pprint/pprint x#)))
         x#)))

(let [polymorphic-dissoc
      (fn [m k] (if (instance? clojure.lang.IPersistentMap m)
                  (dissoc m k)
                  (assoc m k nil)))]
  (defn dissoc-in [m [k & ks :as keys]]
    (if (seq keys)
      (if ks
        (let [inner-map (dissoc-in (m k) ks)]
          (if (seq inner-map)
            (assoc m k inner-map)
            (polymorphic-dissoc m k)))
        (polymorphic-dissoc m k))
      m)))

(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn call-times [n f & args]
  (let [n (int n)]
    (if (or (zero? n) (neg? n))
      (first args)
      (loop [i (dec n) result (apply f args)]
        (if (zero? i)
          result
          (recur (dec i) (f result)))))))

(defn rec==
  ([x] true)
  ([x y]
   (cond (and (number? x) (number? y)) (== x y)
         (and (coll? x) (coll? y)) (every? true? (map rec== x y))
         :else (= x y)))
  ([x y & more]
   (if (rec== x y)
     (apply rec== x (first more) (rest more))
     false)))

(def not== (complement ==))

(defn flip [f]
  (comp (partial apply f) reverse list))

(defn partial* [f & args]
  (apply partial (flip f) args))

(defn conj-some [xs x]
  (if (some? x) (conj xs x) xs))

(defmacro take-at-least-ms [ms & body]
  `(let [start# (current-time-ms)
         result# (do ~@body)
         stop# (current-time-ms)
         took# (- stop# start#)]
     (if (>= ~ms took#)
       (Thread/sleep (- ~ms took#))
       (println "WARNING:" (current-thread-name)
                "took longer than expected to execute" '~body))
     result#))

(let [r (Random.)]
  (defn rand-gaussian
    ([] (rand-gaussian 1 0))
    ([sd] (rand-gaussian sd 0))
    ([sd mean] (+ (* sd (.nextGaussian r)) mean)))
  (defn rand-binomial [n p]
    (apply + (repeatedly n #(if (> p (.nextDouble r)) 1 0))))
  (defn rand-uniform
    ([max] (* max (.nextDouble r)))
    ([min max] (+ min (* (- max min) (.nextDouble r)))))
  (defn rand-uniform-int
    ([max] (.nextInt (inc max)))
    ([min max] (+ min (.nextInt (inc (- max min)))))))

(defmacro def-let [bindings]
  (let [let-expr (macroexpand `(let ~bindings))
        vars (filter #(not (.contains (str %) "__"))
                     (map first (partition 2 (second let-expr))))
        defs (map (fn [v] `(def ~v ~v)) vars)]
    (concat let-expr defs)))

(defmacro condf [obj & pairs]
  (assert-args
    (even? (count pairs)) "an odd number of arguments")
  (when pairs
    `(if (~(first pairs) ~obj)
       ~(second pairs)
       (condf ~obj ~@(next (next pairs))))))

(defn throw-error [& msg]
  (throw (Error. (apply str msg))))

(defn move-in [m from-path to-path]
  (-> m
      (assoc-in to-path (get-in m from-path))
      (dissoc-in from-path)))

(defn swap-in [m path1 path2]
  (let [item1 (get-in m path1)
        item2 (get-in m path2)]
    (-> m (assoc-in path1 item2) (assoc-in path2 item1))))

(defn remove-map-nils [m]
  (into {} (filter #(% 1) m)))
