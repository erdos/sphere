(ns erdos.lenart.lang
  (:require [erdos.lenart.geo :as geo]
            [clojure.string :refer [join]])
  #?(:cljs (:require-macros [erdos.lenart.macros :refer [match-seq]])
     :clj  (:require [erdos.lenart.macros :refer [match-seq]])))


(defn topsort
  "Creates a processing order in the map"
  [deps m]
  (let [k->deps (zipmap (keys m) (map (partial deps m) (keys m)))
        no-deps (map key (filter (comp empty? val) k->deps))
        others (set (remove (set no-deps) (keys k->deps)))]
    (loop [out (vec no-deps)
           others (set others)]
      (if (seq others)
        (when-let [kk (seq (remove #(some others (k->deps %)) others))]
          (recur (into out kk) (reduce disj others kk)))
        out))))

(defn tokenize-sentence [s]
  (assert (string? s))
  (seq (.split s " ")))

(defn parse-style-item [s]
  (assert (sequential? s))
  (match-seq s
    [] {} ; default style maybe??
    ["color" ?c & ?xs]
    (-> ?xs parse-style-item (assoc :color ?c)) ;; color
    ["size" ?s & ?xs]
    (-> ?xs parse-style-item (assoc :size ?s)) ;; stroke width

    ["stroke" "dashed" & ?xs]
    (-> ?xs parse-style-item (assoc :stroke-style :dashed))

    ["stroke" "dotted" & ?xs]
    (-> ?xs parse-style-item (assoc :stroke-style :dotted))

    {:error "Unexpected style definition!"}))

(defn- parse-style [s]
  (match-seq s
     [] {}
     ["with" & ?style] (parse-style-item ?style)
     (throw (ex-info "Wrong style definitions!"
                     {:error (str "Unexpected: " (join " " s))}))))

(defn str->num [x]
  #?(:cljs (let [n (.parseFloat js/Number x)]
             (if (js/isNaN n)
               (throw (ex-info "Not a number!" {:error "Could not parse number!"}))
               n))
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

    ["antipode" "of" ?name & rest]
    (-> rest parse-style (assoc :type :antipode :origin ?name))

    ["midpoint" "of" ?name1 "and" ?name2 & rest]
    (-> rest parse-style (assoc :type :midpoint :a ?name1 :b ?name2))

    ["intersection" "of" ?name1 "and" ?name2 & rest]
    (-> rest parse-style (assoc :type :intersection :a ?name1 :b ?name2))

    ["segment" "between" ?from "and" ?to & rest]
    (-> rest parse-style (assoc :type :segment :from ?from :to ?to))

    (throw (ex-info "Not a construction!" {:error "Not a construction!"}))))

(defn parse-sentence [s-]
  (let [s (if (vector? s-) s- (tokenize-sentence s-))]
    (match-seq s
               [?id "is" "hidden" & ?xs]
               (-> ?xs (parse-construction) (assoc :id ?id :hidden true))
               [?id "is" & ?xs]
               (-> ?xs (parse-construction) (assoc :id ?id :hidden false))
               ["draw" & ?xs]
               (-> ?xs (parse-construction) (assoc :id (gensym) :hidden false))
               (throw (ex-info "Unexpected line!"
                               {:error "Unexpected line!"})))))

(defn parse-sentence- [x]
  (-> (try (parse-sentence x)
           (catch #?(:cljs :default :clj Exception) e
               (if (:error (ex-data e))
                 (ex-data e)
                 {:error (str "Unexpected exception: " e)})))
      (assoc :line x)))

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

(defn parse-book [ls]
  (let [ls    (filter seq (map #(.trim %) (.split ls "\n")))
        xs    (keep parse-sentence- ls)
        error (some #(when (:error %) %) xs)
        xs    (take-while (complement :error) xs)
        m     (zipmap (map :id xs) xs)]
    (if-let [top (topsort deps m)]
      (let [reduced (reduce (fn [acc x]
                              (let [e (geo/eval-geo acc (m x))]
                                (if (:error e)
                                  (reduced (assoc acc :err e))
                                  (assoc acc (:id e) e))))
                            {} top)]
        {:construction (doall (keep reduced top))
         :err          (:err reduced error)})
      {:err {:error "Error in construction: circular dependency!"}})))
