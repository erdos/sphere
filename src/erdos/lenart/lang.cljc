(ns erdos.lenart.lang
  (:require [erdos.lenart.math :as m])
  #?(:cljs (:require-macros [erdos.lenart.macros
                            :refer [match-seq]])
     :clj (:require [erdos.lenart.macros
                     :refer [match-seq]])))


(defn topsort
  "Creates a processing order in the map"
  [m]
  (let [deps (fn [x] (let [mx (m x)]
                       (remove nil?
                               (list* (:from mx) (:to mx) (:a mx) (:b mx) (:origin mx) (:on mx) (concat (:pts mx))))))
        k->deps (zipmap (keys m) (map deps (keys m)))
        no-deps (map key (filter (comp empty? val) k->deps))
        others (set (remove (set no-deps) (keys k->deps)))]
    (loop [out (vec no-deps)
           others (set others)]
      (if (seq others)
        (when-let [kk (seq (remove  #(some others (k->deps %)) others))]
          (recur (into out kk) (reduce disj others kk)))
        out))))

(defmulti eval-geo (fn [acc x] (:type x)))

(defmethod eval-geo :default [acc x]
  (assert false (str "unexpected " x)))

(defmethod eval-geo :point [acc x] x)

(defmethod eval-geo :segment
  [acc x]
  (let [fl (-> x :from acc :loc)
        tl (-> x :to acc :loc)]
    (assoc x :from fl :to tl)))

;; TODO: handle first, second, nth intersections!
(defmethod eval-geo :intersection [acc x]
  (cond
    (= :great-circle (-> x :a acc :type) (-> x :b acc :type))
    (let [loc1 (-> x :a acc :origin)
          loc2 (-> x :b acc :origin)
          loc (m/cross loc1 loc2)]
      (.log js/console (str "intersect> " loc))
      {:type :point
       :loc  loc
       :id   (:id x)
       :hidden false})))

(defmethod eval-geo :great-circle
  [acc x]
  (let [o (-> x :origin acc :loc)]
    (assoc x :origin o)))

(defmethod eval-geo :polygon
  [acc x]
  (let [xs (->> x :pts (map acc) (map :loc))]
    (assoc x :pts xs)))

(declare parse-sentence)

(defn parse-sentence- [x]
  (try (parse-sentence x)
       #?(:cljs (catch :default e (.log js/console e))
          :clj  (catch Exception e (.printStackTrace e))) nil))


(defn parse-book [ls]
  (let [ls  (.split ls "\n")
        xs  (keep parse-sentence- ls)
        m   (zipmap (map :id xs) xs)
        top (topsort m)]
    (-> (reduce (fn [acc x]
               (let [e (eval-geo acc (m x))]
                 (assoc acc (:id e) e))) {} top)
         (mapv top))))

; (parse-book "a is point at 1 2 3\nb is point at 3 4 5\ndraw segment between a and b")


(defn tokenize-sentence [s]
  (assert (string? s))
  (seq (.split s " ")))

(defn tokenize-sentences [s]
  (assert (string? s))
  (map tokenize-sentence (seq (.split s "\n"))))

(declare parse-construction)

(defn parse-sentence [s]
  (let [s (if (vector? s) s (tokenize-sentence s))]
    (match-seq s
      [?id "is" "hidden" & ?xs]
      (-> ?xs (parse-construction) (assoc :id ?id :hidden true))
      [?id "is" & ?xs]
      (-> ?xs (parse-construction) (assoc :id ?id :hidden false))
      ["draw" & ?xs]
      (-> ?xs (parse-construction) (assoc :id (gensym) :hidden true)))))

(comment
  (parse-sentence "x is hidden segment from 1 to 2"
   )




  )

(defn parse-style-item [s]
  (assert (sequential? s))
  (match-seq s
    [] {} ; default style maybe??
    ["color" ?c & ?xs]
    (-> ?xs parse-style-item (assoc :color ?c))
    ["stroke" ?s & xs]
    (-> xs parse-style-item (assoc :stroke ?s))
    ["visibility" ?v & xs]
    (-> xs parse-style-item (assoc :hidden ?v))))

(defn parse-style [s]
  (match-seq s
             [] {}
             ["with" & ?style] (parse-style-item ?style)))

(defn str->num [x]
  #?(:cljs (.parseFloat js/Number x)
     :clj  (Double/parseDouble x)))

(defn parse-construction [s]
  (assert (sequential? s))
  (match-seq s

    ["polygon" "of" & ?rest]
    (let [[a1 a2] (split-with (partial not= "with") ?rest)]
      (-> a2 parse-style
          (assoc :type :polygon :pts (vec a1))))

    ["great" "circle" "of" ?x & rest]
    (-> rest parse-style (assoc :type :great-circle :origin ?x))

    ["point" "at" ?x ?y ?z & rest]
    (-> rest parse-style (assoc :type :point
                                :loc [(str->num ?x)
                                      (str->num ?y)
                                      (str->num ?z)]))

    ["intersection" "of" ?name1 "and" ?name2 & rest]
    (-> rest parse-style (assoc :type :intersection :a ?name1 :b ?name2))

    ["segment" "between" ?from "and" ?to & rest]
    (-> rest parse-style (assoc :type :segment :from ?from :to ?to))
    ;;,,{:type :segment :from ?name1 :to ?name2}
    (assert false (str "Not a construction: " s))))


(comment



  ;; sentence:
  [OBJECTID is CONSTRUCTION]
  [OBJECTID is hidden CONSTUCTION]
  [draw CONSTRUCTION]

  ;; OBJECTID: a-zA-Z0-9

  ;; CONSTRUCTION
  [free point] ;; can be moved by mouse ??
  [point at F F F] ;; can be moved by mouse???
  [intersection of OBJECTID and OBJECTID]
  [segment between OBJECTID and OBJECTID]
  ;;[great circle for OBJECTID]
  ;;[midpoint of OBJECTID and OBJECTID]
  ;;[polygon for OBJECTID ... OBJECTID]

  ;; idea: optional suffix: with color red
  ;; IDEA: C is A bang B with color red


  a is point at 0.2 0.3 -0.4
  b is point at 2 43 4
  s1 is segment between a and b with color silver

  c is point at 2 3 3
  d is point at 4 3 2
  s2 is segment between c d with color silver

  pt is intersection of s1 and s2 with color red

  ptline is great circle of pt

  draw great circle of c
  draw great circle of d


)
