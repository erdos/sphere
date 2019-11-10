(ns erdos.lenart.geo
  "Geometric construction related data structures"
  (:require [erdos.lenart.math :as m]))


(defmulti eval-geo (fn [acc x] (:type x)))

(defmethod eval-geo :default [acc x]
  (assert false (str "unexpected " x)))

(defmethod eval-geo :point [acc x]
  (if (m/zero-vec? (:loc x))
    (assoc x :error "Point is not on the surface of the sphere!")
    x))

(defmethod eval-geo :segment
  [acc x]
  (let [fl (-> x :from acc :loc)
        tl (-> x :to acc :loc)]
    (cond
      (nil? fl)            (assoc x :error (str "Not a point: " (:from x)))
      (nil? tl)            (assoc x :error (str "Not a point: " (:to x)))
      (m/antipodal? fl tl) (assoc x :error (str "Points are antipodal!"))
      :else                (assoc x :from fl :to tl))))

(defmulti point-dist (fn [obj [x y z] lookup] (:type obj)))

(defmethod point-dist :point [point-obj pt lookup]
  (m/dist-angle (:loc point-obj) pt))

(defmethod point-dist :great-circle [great-circle pt lookup]
  (m/abs (- (m/dist-angle (:origin great-circle) pt) m/half-pi)))

(defn colinear-points? [a mid b]
  (assert a)
  (assert mid)
  (assert b)
  (or (m/close? (m/unit (m/cross a mid)) (m/unit (m/cross mid b)) (m/unit (m/cross a b)))
      (m/close? (m/unit a) (m/unit mid))
      (m/close? (m/unit b) (m/unit mid))))

(defmethod point-dist :segment [segment pt lookup]
  (let [p1 (-> segment :from)
        p2 (-> segment :to)]
    (assert p1) (assert p2)
    (if (colinear-points? p1 pt p2)
      0.0
      m/half-pi ;; TODO: implement this branch!
      )))

(defmulti intersection (fn [acc x] [(-> x :a acc :type) (-> x :b acc :type)]))

;; TODO: handle first, second, nth intersections!
(defmethod eval-geo :intersection [acc x]
  (cond (-> x :a acc nil?) (assoc x :error (str "Did not find object: " (:a x)))
        (-> x :b acc nil?) (assoc x :error (str "Did not find object: " (:b x)))
        :else              (intersection acc x)))

(defmethod eval-geo :midpoint [acc x]
  (let [loc1 (-> x :a acc :loc)
        loc2 (-> x :b acc :loc)]
    (cond
      (nil? loc1)              (assoc x :error (str "Not a point: " (:a x)))
      (nil? loc2)              (assoc x :error (str "Not a point: " (:b x)))
      (m/antipodal? loc1 loc2) (assoc x :error (str "Points are antipodal!"))
      :else                    (assoc x :type :point :loc (m/mean loc1 loc2)))))

(defmethod eval-geo :great-circle [acc x]
  (if-let [o (-> x :origin acc :loc)]
    (assoc x :origin o)
    (assoc x :error "Origin of great circle does not exist!")))

(defmethod eval-geo :polygon [acc x]
  (assoc x :pts (->> x :pts (map acc) (map :loc) (doall))))

(defmethod intersection [:great-circle :great-circle] [acc x]
  (let [loc1 (-> x :a acc :origin)
        loc2 (-> x :b acc :origin)
        loc  (m/cross loc1 loc2)]

    (if (m/zero-vec? loc)
      {:error "Could not take intersection of object with itself!"
       :line (:line x)}
      (assoc x :type :point :loc loc))))

(defmethod intersection [:segment :segment] [acc x]
  (let [s1o (m/cross (-> x :a acc :from) (-> x :a acc :to))
        s2o (m/cross (-> x :b acc :from) (-> x :b acc :to))

        loc   (m/cross s1o s2o)
        loc-1 (m/antipode loc)]
    (cond
      (and (zero? (point-dist (-> x :a acc) loc acc))
           (zero? (point-dist (-> x :b acc) loc acc)))
      (assoc x :type :point :loc loc)

      (and (zero? (point-dist (-> x :a acc) loc-1 acc))
           (zero? (point-dist (-> x :b acc) loc-1 acc)))
      (assoc x :type :point :loc loc-1)

      :else
      (assoc x :type :point :exists false :loc nil))))

(defmethod intersection [:great-circle :segment] [acc x]
  (let [s1o (-> x :a acc :origin)
        s2o (m/cross (-> x :b acc :from) (-> x :b acc :to))

        loc   (m/cross s1o s2o)
        loc-1 (m/antipode loc)]
    (cond
      (and (m/small? (point-dist (-> x :a acc) loc acc))
           (m/small? (point-dist (-> x :b acc) loc acc)))
      (assoc x :type :point :loc loc)

      (and (m/small? (point-dist (-> x :a acc) loc-1 acc))
           (m/small? (point-dist (-> x :b acc) loc-1 acc)))
      (assoc x :type :point :loc loc-1)

      :else
      (assoc x :type :point :exists false :loc nil))))

(defmethod intersection [:segment :great-circle] [acc x]
  (intersection acc (assoc x :a (:b x) :b (:a x))))

(defmethod intersection :default [acc x]
  {:error (str "Can not intersect these objects: " (:a x) " and " (:b x))})
