(ns game.utils
  (:require
   [clojure.core.matrix :as m]
   [clojure.pprint :as pp]
   [clojure.reflect :as r]
   [clojure.string :as str]
   [clojure.walk :as walk])
  (:import
   (java.util Random)))

;; Set here because it's gonna run very early
(m/set-current-implementation :vectorz)

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

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

(defmacro dotimes*
  "Like dotimes, but allows more than one binding,
   e.g. (dotimes* [i (range 10) j (range 10)] body)."
  [bindsvec & body]
  (assert-args
   (vector? bindsvec) "a vector for its binding"
   (even? (count bindsvec))
   "an even number of elements in its bindings vector")
  (if (> (count bindsvec) 2)
    `(dotimes ~(vec (take 2 bindsvec))
       (dotimes* ~(vec (drop 2 bindsvec))
         ~@body))
    `(dotimes ~bindsvec ~@body)))

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

(defn dissoc-in [m [k & ks]]
  (letfn [(polymorphic-dissoc [m k]
            (if (instance? clojure.lang.IPersistentMap m)
              (dissoc m k)
              (assoc m k nil)))]
    (if-let [inner (and ks (get m k))]
      (assoc m k (dissoc-in inner ks))
      (polymorphic-dissoc m k))))

(defn fmap [f m]
  (into {} (map (fn [[k v]] [k (f v)])) m))

(defn call-times
  "Call f on args, then again on the result, and so on, n times."
  [n f & args]
  (let [n (int n)]
    (if-not (pos? n)
      (first args)
      (loop [i (dec n) result (apply f args)]
        (if (zero? i)
          result
          (recur (dec i) (f result)))))))

(defn rec==
  ([x] true)
  ([x y]
   (cond (and (number? x) (number? y)) (== x y)
         (and (coll? x) (coll? y)) (and (== (count x) (count y))
                                        (every? true? (map rec== x y)))
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

(defn conj-some [coll x]
  (if (some? x) (conj coll x) coll))

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
    ([max] (.nextInt r (inc max)))
    ([min max] (+ min (.nextInt r (inc (- max min)))))))

(defmacro defs
  "Like def, but can take several symbol-value pairs, and can destructure like
   let."
  [& bindings]
  (let [let-expr (macroexpand `(let ~(vec bindings)))
        new-let-bindings (vec (mapcat
                               (fn [[sym expr]]
                                 (if (str/includes? (str sym) "__")
                                   [sym expr]
                                   [sym expr '_ `(def ~sym ~sym)]))
                               (partition 2 (second let-expr))))]
    `(let* ~(vec new-let-bindings))))

(defmacro condf [obj & pairs]
  (assert-args
   (even? (count pairs)) "an odd number of arguments")
  (when pairs
    `(if (~(first pairs) ~obj)
       ~(second pairs)
       (condf ~obj ~@(next (next pairs))))))

(defmacro cond-pairs [& vs]
  `(cond ~@(apply concat vs)))

(defn throw-error [& msg]
  (throw (Error. (apply str msg))))

(defn throw-ex [& msg]
  (throw (Exception. (apply str msg))))

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

(defn domap [& args]
  (doall (apply map args)))

(defn runmap [& args]
  (dorun (apply map args)))

(defmacro defmemoized [& body]
  `(do (defn ~@body)
       (alter-var-root (var ~(first body)) memoize)))

(defmacro assert-even-vector [v]
  `(assert-args
    (vector? ~v) "a vector for its binding"
    (even? (count ~v)) "an even number of forms in binding vector"))

(defmacro when-lets [bindings & body]
  (assert-even-vector bindings)
  (if (seq bindings)
    `(when-let ~(subvec bindings 0 2)
       (when-lets ~(subvec bindings 2)
         ~@body))
    `(do ~@body)))

(defmacro if-lets
  "If every binding is truthy, do the then branch, else the else branch."
  [bindings then else]
  (assert-even-vector bindings)
  (if (seq bindings)
    `(if-let ~(subvec bindings 0 2)
       (if-lets ~(subvec bindings 2)
         ~then
         ~else)
       ~else)
    then))

(defn iterate-some [f x]
  (take-while (complement nil?) (iterate #(when % (f %)) x)))

(defn ensure-vec [v]
  (if (vector? v) v [v v]))

(defn vectorize [form]
  (walk/postwalk (fn [form] (if (seq? form) (vec form) form)) form))

(defn repeat-str [n s]
  (apply str (repeat n s)))

(defn pair-cycle [coll]
  (take (count coll) (partition 2 1 (cycle coll))))

(defn print-methods [object]
  (pp/print-table
   (sort-by :name (filter :exception-types (:members (r/reflect object))))))

(defmacro log-fn-io
  "Replace the function named with a function that wraps it, and prints its
   input and output."
  [fn-sym]
  `(alter-var-root
    #'~fn-sym
    (fn [f#]
      (fn [& args#]
        (println "Input to" '~fn-sym)
        (pp/pprint args#)
        (let [r# (apply f# args#)]
          (println "Output from" '~fn-sym)
          (pp/pprint r#)
          r#)))))

(letfn [(contains-$? [form]
          (some #{'$} (tree-seq coll? seq form)))
        (insert-$-if-missing [arrow forms]
          (map (fn [form]
                 (cond (symbol? form) (list form '$)
                       (contains-$? form) form
                       :else (list arrow '$ form)))
               forms))]
  (defmacro ->$
    "Acts like (as-> x $ form) if a form contains $, otherwise acts like ->."
    [x & forms]
    `(as-> ~x ~'$
           ~@(insert-$-if-missing '-> forms)))
  (defmacro ->>$
    "Acts like (as-> x $ form) if a form contains $, otherwise acts like ->>."
    [x & forms]
    `(as-> ~x ~'$
           ~@(insert-$-if-missing '->> forms))))

(defn zip
  ([a b] (map vector a b))
  ([a b & more]
   (apply map vector a b more)))

(defn reject-indices [v indices]
  (let [iset (set indices)]
    (vec (keep-indexed (fn [i x] (if-not (iset i) x)) v))))

(defn derive-many [hierarchy coll parent]
  (reduce (fn [h elem] (derive h elem parent))
          hierarchy coll))

(defn fdefault [f default]
  (fn [& args]
    (let [r (apply f args)]
      (if (nil? r)
        default
        r))))

(defn +some [& args]
  (apply + (remove nil? args)))

(defn *some [& args]
  (apply * (remove nil? args)))
