(ns erdos.lenart.macros)

(defn- expr-branch? [x]
  (and (coll? x)
       (not (and (seq? x) (= 'quote (first x))))))

(defn- expr-deref? [x]
  (when (and (seq? x)
             (= 'clojure.core/deref (first x)))
    (second x)))

(def genkwd
  "Generate a random keyword, like gensym"
  (comp keyword name gensym))

(defmacro defatom= [n expr]
  (assert (symbol? n))
  (let [expr (clojure.walk/macroexpand-all expr)
        xs (->> (tree-seq expr-branch? seq expr)
                (keep expr-deref?)
                (filter symbol?))
        f (gensym)]
    `(let [~f (fn [] ~expr)]
       (def ~(with-meta n nil) (reagent.core/atom (~f)))
       ~@(for [x xs]
           (list 'add-watch x (genkwd)
                 (list 'fn '[_ _ _ _]
                       (list 'reset! n (list f)))))
       #'~n)))

(defmacro fnx [& bodies]
  `(fn [~'x] & bodies))

(defmacro fnxy [& bodies]
  `(fn [~'x ~'y] & bodies))

(defmacro fnxyz [& bodies]
  `(fn [~'x ~'y ~'z] & bodies))

(defmacro fn-> [& bodies]
  `(fn [x#] (-> x# ~@bodies)))

(defmacro fn->> [& bodies]
  `(fn [x#] (->> x# ~@bodies)))

(defn partial_ [f & xs] (fn [x] (apply f x xs)))

(defmacro comp_ [& fs]
  `(comp ~@(reverse fs)))

;; procedural style programming

(defn- proc-disp
  ([] ::end) ([x & _] x))

(defmulti proc proc-disp :default ::default)
(defmethod proc ::end [])
(defmethod proc ::default [x & xs] `(do ~x ~(apply proc xs)))

(defmethod proc :if [_ c & xs]
  (let [[as [_ & bs]] (split-with #{:else} xs)]
    ;; todo: find :elsif clauses in #'as
    `(if ~c ~(apply proc as) ~(apply proc bs))))

(defmethod proc :when [_ y & xs] `(if ~y ~(apply proc xs)) )
(defmethod proc :let [_ bs & xs] `(let ~bs ~(apply proc xs)))
(defmethod proc :for [_ bs & xs] `(for ~bs ~(apply proc xs)))
(defmethod proc :loop [_ bs & xs] `(loop ~bs ~(apply proc xs)))

;; todo: test :recur clause
(defmethod proc :recur [_ & bs] `(recur ~@bs))

(defn scalar? [x]
  (or (string? x) (number? x) (nil? x) (keyword? x) (true? x) (false? x)))

(defn- match-seq-1 [b case then else]
  (assert (symbol? b))
  (assert (vector? case))
  (let [ks (for [[i k] (map vector (range) case)
                 :when (not (symbol? k))]
             `(= (nth ~b ~i) ~k))
        rest? (if (-> case butlast last (= '&))
                (last case))
        ls (for [[i k] (map vector (range) case)
                 :when (symbol? k)]
             `(~k (nth ~b ~i)))
        ls (if rest? (butlast (butlast ls)) ls)

        cnt (if rest?
              `(<= ~(dec (dec (count case))) (count ~b))
              `(= ~(count case) (count ~b)))]
    (assert (or rest? cnt (seq ks)) (str "No literal in pattern: " case))
    `(if (and ~cnt ~@ks)
       (let [~@(apply concat ls)
             ~@(if rest? `(~rest? (nthrest ~b ~(- (count case) 2))))]
         ~then)
       ~else)))

(defmacro match-seq [expr & clauses]
  (let [b (gensym)
        cls (partition 2 clauses)
        default (if (odd? (count clauses))
                  (last clauses)
                  `(assert false (str "no match for " ~b)))
        ]
    `(let [~b ~expr]
       ~(reduce (fn [acc [cond then]]
                  (match-seq-1 b cond then acc))
                default (reverse cls)))))


(defmacro template [p & itms]
  ;(.log js/console (str p (type p)))
  (assert (string? p) (str "no str: " p))
  (let [ls (.split p "%")]
    (assert (= (dec (count ls)) (count itms))
            (str "arg count does not match"))
    `(str ~@(interleave ls itms))
    ) ;; idea; can parse str now
  )

(defmacro format [s & params]
  (loop [xs [] ;; output
         s  s
         [p & ps] params]
    (let [idx (.indexOf s "%")]
      (if (neg? idx)
        (do (assert (nil? p))
            `(.join (cljs.core/array ~@(remove #{""} (conj xs s))) ""))
        (let [s0 (.substring s 0 idx)
              st (.substring s (inc idx))]
          (cond (.startsWith st "d")
                (recur (conj xs s0 p)
                       (.substring st 1)
                       ps)

                (.startsWith st ".4f")
                (recur (conj xs s0 `(.toFixed ~p 4))
                       (.substring st 3)
                       ps)

                (.startsWith st ".2f")
                (recur (conj xs s0 `(.toFixed ~p 2))
                       (.substring st 3)
                       ps)

                :else (assert false "Unexpected str!")))))))

:OK
