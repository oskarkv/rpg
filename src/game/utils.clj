(ns game.utils
  (:require
   [clojure.core.matrix :as m]
   [clojure.pprint :as pp]
   [clojure.reflect :as r]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [clojure.set :as set])
  (:import
   (java.util Random)))

;; Set here because it's gonna run very early
(m/set-current-implementation :vectorz)

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn flatten-more
  "Like flatten, but also flattens sets."
  [x]
  (let [pred (some-fn sequential? set?)]
    (remove pred (tree-seq pred seq x))))

(defn flatten-all
  "Like flatten, but also flattens sets and maps."
  [x]
  (remove coll? (tree-seq coll? seq x)))

(defn x!-sym? [sym x]
  (and (symbol? sym)
       (> (count (str sym)) 2)
       (str/starts-with? (str sym) (str x "!"))))

(defn g!-sym? [sym]
  (x!-sym? sym "g"))

(defn o!-sym? [sym]
  (x!-sym? sym "o"))

(defn o!-sym-to-g!-sym [sym]
  (symbol (str "g!" (subs (str sym) 2))))

(defmacro defmacro-g!
  "Like defmacro, but symbols starting with g! in the body will be let-bound
   to gensyms."
  [name args & body]
  (let [syms (distinct (filter g!-sym? (flatten-all body)))]
    `(defmacro ~name ~args
       (let [~@(apply concat (for [s syms]
                               [s `(gensym ~(subs (str s) 2))]))]
         ~@body))))

(defmacro defmacro! [name args & body]
  "Like defmacro-g!, but symbols starting with o! in the arguments vector of the
   defined macro will be evaluated and the corresponding g!-symbols will be
   let-bound to the results, making it easy to evaluate the arguments only
   once."
  (let [os (filter o!-sym? (flatten-all args))
        gs (map o!-sym-to-g!-sym os)]
    `(defmacro-g! ~name ~args
       `(let [~~@(interleave gs os)]
          ~~@body))))

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

(let [const #(with-meta % {:const true})]
  (defmacro defconst
    ([sym value]
     (list `def (const sym) value))
    ([sym docstring value]
     (list `def (const sym) docstring value))))

(defmacro defconsts [& pairs]
  (assert-args (even? (count pairs)) "an even number of arguments")
  (let [pairs (partition 2 pairs)]
    `(do ~@(map (fn [[sym val]] `(defconst ~sym ~val))
                pairs))))

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

(defmacro make-map
  "Returns a map mapping keywords with the names of the provided symbols to
   their values in the current context."
  [& syms]
  (zipmap (map keyword syms) syms))

(defn dissoc-in [m [k & ks]]
  (letfn [(polymorphic-dissoc [m k]
            (if (instance? clojure.lang.IPersistentMap m)
              (dissoc m k)
              (assoc m k nil)))]
    (if-let [inner (and ks (get m k))]
      (assoc m k (dissoc-in inner ks))
      (polymorphic-dissoc m k))))

(defn fmap
  "Apply f to the vals in m and return a map. If colls are provided, apply f to
   the vals in m and the items from colls, as with clojure.core/map, but keep in
   mind that the order of the keys is unreliable."
  ([f m] (into {} (map (fn [[k v]] [k (f v)])) m))
  ([f m & colls] (zipmap (keys m) (apply map f (vals m) colls))))

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

(defn random-pick
  "Randomly pick a key in chance-map, where each value is the relative chance
   for the key."
  [chance-map]
  (let [total (apply + (vals chance-map))]
    (reduce (fn [left [k v]]
              (if (> left v)
                (- left v)
                (reduced k)))
            (rand-uniform total)
            chance-map)))

(defn chance
  "Returns true with the given probability."
  [probability]
   (< (rand-uniform 1) probability))

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

(defmacro condf
  "Takes an object and a set of test-fn/expr pairs. It evaluates
   (test-fn object) for each pair in order, and returns the expr of the pair if
   the test-fn returns logical true. If no pair matches, returns nil."
  [obj & pairs]
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

(defn swap [m k1 k2]
  (let [item1 (m k1)
        item2 (m k2)]
    (-> m (assoc k1 item2) (assoc k2 item1))))

(defn remove-map-nils [m]
  (into {} (filter (comp some? val) m)))

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
        (insert-$-in-one [arrow form]
          (cond (contains-$? form) form
                :else (list arrow '$ form)))
        (insert-$-in-many [arrow forms]
          (map #(insert-$-in-one arrow %) forms))
        (simple-body [x arrow forms]
          `(as-> ~x ~'$
                 ~@(insert-$-in-many arrow forms)))
        (cond-body [x arrow pairs]
          `(-> ~x
             ~@(map (fn [[tst form]]
                      `(as-> ~'$
                         (if ~tst ~(insert-$-in-one arrow form) ~'$)))
                    (partition 2 pairs))))
        (some-body [x arrow forms]
          `(some-> ~x
             ~@(map (fn [form] `(as-> ~'$ ~(insert-$-in-one arrow form)))
                    forms)))]
  (defmacro ->$
    "Acts like (as-> x $ form) if a form contains $, otherwise acts like ->."
    [x & forms]
    (simple-body x '-> forms))
  (defmacro ->>$
    "Acts like (as-> x $ form) if a form contains $, otherwise acts like ->>."
    [x & forms]
    (simple-body x '->> forms))
  (defmacro cond->$
    "Acts like cond-> except that if the symbol $ exists in a test or an
     expression, it will be replaced by the current value being threaded,
     and the normal -> rules will not apply to the expression."
    [x & pairs]
    (cond-body x '-> pairs))
  (defmacro cond->>$
    "Acts like cond->> except that if the symbol $ exists in a test or an
     expression, it will be replaced by the current value being threaded,
     and the normal ->> rules will not apply to the expression."
    [x & pairs]
    (cond-body x '->> pairs))
  (defmacro some->$
    "Acts like (some-> x (as-> $ form) ...) if a form contains $, otherwise
     acts like some->."
    [x & forms]
    (some-body x '-> forms))
  (defmacro some->>$
    "Acts like (some-> x (as-> $ form) ...) if a form contains $, otherwise
     acts like some->>."
    [x & forms]
    (some-body x '->> forms)))

(defn zip
  ([a b] (map vector a b))
  ([a b & more]
   (apply map vector a b more)))

(defn reject-indices [coll indices]
  (let [iset (set indices)]
    (vec (keep-indexed (fn [i x] (if-not (iset i) x)) coll))))

(defn reject-keys [m keyseq]
  (select-keys m (remove (set keyseq) (keys m))))

(defn select-random-keys [m num]
  (select-keys m (take num (shuffle (keys m)))))

(defn derive-many [hierarchy coll parent]
  (reduce (fn [h elem] (derive h elem parent))
          hierarchy coll))

(defn fdefault
  "Takes a function f and a default value, and returns a function that is like f
   but returns default when f returns nil."
  [f default]
  (fn [& args]
    (let [r (apply f args)]
      (if (nil? r)
        default
        r))))

(defn +some [& args]
  (apply + (remove nil? args)))

(defn *some [& args]
  (apply * (remove nil? args)))

(defn infinite-shuffle [coll]
  (lazy-cat (shuffle coll) (infinite-shuffle coll)))

(defn takes [[n & ns] coll]
  (when n
    (cons (take n coll)
          (takes ns (drop n coll)))))

(defn interleave-runs
  "Returns the lengths of runs of elements from seq-a if one were to interleave
   seq-a with seq-b as evenly as possible, given that seq-a has length a and
   seq-b has length b, a >= b. The imagined interleaving starts with a run of
   elements from seq-a."
  [a b]
  (let [ib (inc b)
        n (int (/ a ib))
        left (rem a ib)]
    (concat (repeat left (inc n))
            (repeat (- ib left) n))))

(defn uneven-interleave
  "Interleaves the two given sequences, which may be of different lengths, as
   evenly as possible."
  [s1 s2]
  (letfn [(interleave* [s1 s2 rs]
            (when (seq s1)
              (lazy-cat
               (take (first rs) s1)
               (take 1 s2)
               (interleave* (drop (first rs) s1) (rest s2) (rest rs)))))]
    (if (< (count s1) (count s2))
      (uneven-interleave s2 s1)
      (interleave* s1 s2 (interleave-runs (count s1) (count s2))))))

(letfn [(all-pairs* [f]
          (fn [coll]
            (loop [pairs #{} left coll]
              (if-let [rst (next left)]
                (recur (into pairs (map #(f (first left) %)) rst) (rest left))
                pairs))))]
  (defn all-pairs
    "Returns a set of all possible pairs (vectors) of items in coll."
    [coll]
    ((all-pairs* vector) coll))
  (defn all-set-pairs
    "Returns a set of all possible pairs (sets) of items in coll."
    [coll]
    ((all-pairs* hash-set) coll)))

(defn indexed [coll]
  (map-indexed vector coll))

(defn invert-map
  "Returns the map with the vals mapped to the keys."
  [m]
  (reduce (fn [m [k v]] (assoc m v k)) {} m))

(defn bfs-waves
  "Returns a lazy sequence of the waves of successors in a breadth-first search
   from start using the given successors fn to generate the successors of a
   node."
  [start successors]
  (lazy-seq
   (letfn [(bfs* [prevs visited]
             (lazy-seq
              (let [ss (set/difference (set (mapcat successors prevs)) visited)]
                (if (seq ss)
                  (cons ss (bfs* ss (set/union visited ss)))))))]
     (cons #{start} (bfs* [start] #{start})))))

(defn bfs
  "Returns a lazy sequence of nodes in a breadth-first search from start using
   the given successors fn to generate the successors of a node."
  [start successors]
  (apply concat (bfs-waves start successors)))

(defn dfs
  "Returns a lazy sequence of nodes in a depth-first search from start using the
   given successors fn to generate the successors of a node."
  [start successors]
  (let [visited (volatile! #{})
        dfs* (fn dfs* [curr]
               (lazy-seq
                (when-not (@visited curr)
                  (vswap! visited conj curr)
                  (cons curr (mapcat dfs* (successors curr))))))]
    (dfs* start)))
