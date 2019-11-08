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
       (def ~n (reagent.core/atom (~f)))
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

        cnt (if (not rest?) `(= ~(count case) (count ~b)) true)]
    (assert (or rest? cnt (seq ks)) (str "No literal in pattern: " case))
    `(if (and ~@ks ~cnt)
       (let [~@(apply concat ls)
             ~@(if rest? `(~rest? (nthrest ~b ~(- (count case) 2))))
             ] ~then)
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



(comment

  (match-seq "abacus" [?a] :no false)
  (match-seq "ab" [?a ?b] :no false)

  (match-seq "abacus" [?a & ?rs] :no false)



  (match-seq ["point" "at" 2 3 4 "with" "color" "red"]
             ["point" "at" x y z & rest] rest)


  (macroexpand
   (quote
    (match-seq ["point" "at" "1" "2" "3"]
               ["point" "at" ?x ?y ?z]
               [:pt ?x ?y ?z])))


  (macroexpand
   (quote
    (match-seq "abacus"
               ;[\a ?b] :a
               ;[?c \b] :c
               [?a ?b & ?bacus] ?bacus)
    ))


  (nthrest [] 3)
  (match-seq (seq nil)
             [] {} ; default style maybe??
    ["color" ?c & ?xs]
    :c
    ["visibility" ?v & xs]
    :vis)
  )

(defn- obj->1
  [obj x]
  (let []
    (if (seq? x)
      `(if-let [o# ~obj]
         (if-let [m# (aget o# ~(name (first x)))]
           (.call m# o# ~@(rest x))
           (throw ~(str "no method: " (name (first x)))))
         (throw ~(str "call on nil: " (name (first x)))))
      `(if-let [o# ~obj]
         (aget o# ~(name x))
         (throw ~(str "call on nil: " (name x)))))))


(defmacro obj-> [obj & xs]
  (reduce obj->1 obj xs))

;; (macroexpand '(obj-> 1 a b c))

;; todo: add :fn clause
;; (defmethod proc :fn [_ & xs] `(fn ~@xs))

;; todo: add :recur clasue??
;; todo: add

(defmacro template [p & itms]
  ;(.log js/console (str p (type p)))
  (assert (string? p) (str "no str: " p))
  (let [ls (.split p "%")]
    (assert (= (dec (count ls)) (count itms))
            (str "arg count does not match"))
    `(str ~@(interleave ls itms))
    ) ;; idea; can parse str now

  )


(comment

  (defn topsort
    "Creates a processing order in the map"
    [m]
  (let [deps (fn [x] (let [mx (m x)] (remove nil? [(:from mx) (:to mx) (:a mx) (:b mx)])))
        k->deps (zipmap (keys m) (map deps (keys m)))
        no-deps (map key (filter (comp empty? val) k->deps))
        others (set (remove (set no-deps) (keys k->deps)))]
    (loop [out (vec no-deps)       ;; already solved
           others (set others) ;; still to sort.
           ]
      (if (seq others)
        (when-let [kk (seq (remove  #(some others (k->deps %)) others))]
          (recur (into out kk) (reduce disj others kk)))
        out))))


  (topsort { :x {2 2} :a {1 1} :b {:from :a} :z {:from :b} :e {:from :z}})

  )



:OK
