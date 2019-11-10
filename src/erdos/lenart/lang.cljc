(ns erdos.lenart.lang
  (:require [erdos.lenart.geo :as geo])
  #?(:cljs (:require-macros [erdos.lenart.macros
                            :refer [match-seq]])
     :clj (:require [erdos.lenart.macros
                     :refer [match-seq]])))

(defn- deps [m x]
  (let [mx (m x)]
    (remove nil?
            (list* (:from mx)
                   (:to mx)
                   (:a mx)
                   (:b mx)
                   (:origin mx)
                   (:on mx)
                   (concat (:pts mx))))))

(defn topsort
  "Creates a processing order in the map"
  [deps m]
  (let [k->deps (zipmap (keys m) (map (partial deps m) (keys m)))
        no-deps (map key (filter (comp empty? val) k->deps))
        others (set (remove (set no-deps) (keys k->deps)))]
    (loop [out (vec no-deps)
           others (set others)]
      (if (seq others)
        (when-let [kk (seq (remove  #(some others (k->deps %)) others))]
          (recur (into out kk) (reduce disj others kk)))
        out))))

(declare parse-sentence)

(defn parse-sentence- [x]
  (try (parse-sentence x)
       #?(:cljs (catch :default e (.log js/console e))
          :clj  (catch Exception e (.printStackTrace e))) nil))


(defn parse-book [ls]
  (let [ls  (map #(.trim %) (.split ls "\n"))
        xs  (keep parse-sentence- ls)
        m   (zipmap (map :id xs) xs)
        top (topsort deps m)]
    (-> (reduce (fn [acc x]
               (let [e (geo/eval-geo acc (m x))]
                 (assoc acc (:id e) e))) {} top)
         (mapv top))))

(defn tokenize-sentence [s]
  (assert (string? s))
  (seq (.split s " ")))

(declare parse-construction)

(defn parse-sentence [s]
  (let [s (if (vector? s) s (tokenize-sentence s))]
    (match-seq s
               [?id "is" "hidden" & ?xs]
               (-> ?xs (parse-construction) (assoc :id ?id :hidden true))
               [?id "is" & ?xs]
               (-> ?xs (parse-construction) (assoc :id ?id :hidden false))
               ["draw" & ?xs]
               (-> ?xs (parse-construction) (assoc :id (gensym) :hidden false)))))

(defn parse-style-item [s]
  (assert (sequential? s))
  (match-seq s
    [] {} ; default style maybe??
    ["color" ?c & ?xs]
    (-> ?xs parse-style-item (assoc :color ?c)) ;; color
    ["size" ?s & ?xs]
    (-> ?xs parse-style-item (assoc :size ?s)) ;; stroke width
    ))

(defn- parse-style [s]
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

    ["midpoint" "of" ?name1 "and" ?name2 & rest]
    (-> rest parse-style (assoc :type :midpoint :a ?name1 :b ?name2))

    ["intersection" "of" ?name1 "and" ?name2 & rest]
    (-> rest parse-style (assoc :type :intersection :a ?name1 :b ?name2))

    ["segment" "between" ?from "and" ?to & rest]
    (-> rest parse-style (assoc :type :segment :from ?from :to ?to))
    ;;,,{:type :segment :from ?name1 :to ?name2}
    (assert false (str "Not a construction: " s))))
