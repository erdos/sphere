(ns erdos.lenart.geo
  "Geometric construction related data structures"
  (:require [erdos.lenart.math :as m]))


(defmulti eval-geo (fn [acc x] (:type x)))

(defmethod eval-geo :default [acc x]
  (assert false (str "unexpected " x)))

(defmethod eval-geo :point [acc x] x)

(defmethod eval-geo :segment
  [acc x]
  (let [fl (-> x :from acc :loc)
        tl (-> x :to acc :loc)]
    (assoc x :from fl :to tl)))

(defmulti intersection (fn [acc x] [(-> x :a acc :type) (-> x :b acc :type)]))
;; TODO: handle first, second, nth intersections!
(defmethod eval-geo :intersection [acc x] (intersection acc x))

(defmethod eval-geo :midpoint [acc x]
  (let [loc1 (-> x :a acc :loc)
        loc2 (-> x :b acc :loc)
        loc  (m/mean loc1 loc2)]
    (assoc x :type :point :loc loc)))

(defmethod eval-geo :great-circle [acc x]
  (let [o (-> x :origin acc :loc)]
    (assoc x :origin o)))

(defmethod eval-geo :polygon [acc x]
  (let [xs (->> x :pts (map acc) (map :loc))]
    (assoc x :pts xs)))

(defmethod intersection [:great-circle :great-circle] [acc x]
  (let [loc1 (-> x :a acc :origin)
        loc2 (-> x :b acc :origin)
        loc  (m/cross loc1 loc2)]
    (assoc x :type :point :loc  loc)))
