(ns erdos.lenart.math
  #_(:require [cljs.test :as t]))

(def pi 3.14159265359)

(def up [0.0 0.0 1.0])

(defn rad->deg [x] (-> x (/ pi) (* 180.0)))
(defn deg->rad [x] (-> x (/ 180.0) (* pi)))

(defn sin [x] #?(:cljs (js/Math.sin x) :clj (Math/sin x)))
(defn cos [x] #?(:cljs (js/Math.cos x) :clj (Math/cos x)))
(defn sqrt [x] #?(:cljs (js/Math.sqrt x) :clj (Math/sqrt x)))

(defn asin [x] (#?(:cljs js/Math.asin :clj Math/asin) x))
(defn acos [x] (#?(:cljs js/Math.acos :clj Math/acos) x))
(defn atan2 [dy dx] (#?(:cljs js/Math.atan2 :clj Math/atan2) dy dx))

(defn unit [[x y z]]
  (let [d (#?(:cljs js/Math.sqrt :clj Math/sqrt) (+ (* x x) (* y y) (* z z)))]
    [(/ x d) (/ y d) (/ z d)]))

(defn dot [[x y z]]
  (+ (* x x) (* y y) (* z z)))

(defn distsq [[a b c] [x y z]]
  (let [ax (- a x) by (- b y) cz (- c z)]
    (+ (* ax ax) (* by by) (* cz cz))))

(def dist (comp sqrt distsq))

;; todo: test for precision
(defn dist-angle [a b]
  (let [d (dist (unit a) (unit b))]
    (if (>= d 2.0) ;; for precisionb
      pi (-> d (/ 2.0) (asin) (* 2.0)))))

#_
(t/deftest test-dist-angle
  (t/is (= 0 (dist-angle up up)))
  (t/is (= (/ pi 2) (dist-angle [1 0 0] [0 1 0])))
  (t/is (= (* pi) (dist-angle [1 0 0] [-0.9999 0 0]))))

(defn antipode [[x y z]]
  [(- x) (- y) (- z)])

(defn mirror [[x y z]]
  [x y (- z)])

(defn cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn zero-vec? [[a b c]]
  (and (zero? a) (zero? b) (zero? c)))

(defn mean [[ax ay az] [bx by bz]]
  [(/ (+ ax bx) 2)
   (/ (+ ay by) 2)
   (/ (+ az bz) 2)])

(defn det [[ax ay az] [bx by bz] [cx cy cz]]
  (- (+ (* ax by cz)
        (* ay bz cx)
        (* az bx cy))
     (+ (* az by cx)
        (* ay bx cz)
        (* ax bz cy))))

(defn clockwise? [[ax ay] [bx by] [cx cy]]
  (pos? (det [ax ay 1.0] [bx by 1.0] [cx cy 1.0])))

(defprotocol Projection
  (euclidean->spherical [_ x y])
  (spherical->euclidean [_ x y z]))

(defrecord Orthographic []
  Projection
  (euclidean->spherical [_ x y]
    (let [d (+ (* x x) (* y y))]
      (when (<= d 1.0)
        [x y (sqrt (- 1.0 d))])))

  (spherical->euclidean [_ x y z] [x y z]))

(def orthographic-projection (->Orthographic))

(defrecord Quaternion [x y z w])

(defn conjugate [{:keys [x y z w]}]
  (->Quaternion (- x) (- y) (- z) w))

(defn invert [{:keys [x y z w]}]
  (->Quaternion (- x) (- y) (- z) (- w)))

(defn multiply
  [{:keys [x y z w]}
   {qx :x qy :y qz :z qw :w}]
  (->Quaternion
   (+ (* w qx) (* x qw) (* y qz) (- (* z qy)))
   (+ (* w qy) (* y qw) (* z qx) (- (* x qz)))
   (+ (* w qz) (* z qw) (* x qy) (- (* y qx)))
   (- (* w qw) (* x qx) (* y qy) (* z qz))))

(defn quaternion->vec [{:keys [x y z]}]
  [x y z])

(defn rotate
  "Rotate vector by quaternion"
  [[x y z] q]
  (-> q
      (multiply (->Quaternion x y z 0))
      (multiply (conjugate q))
      (quaternion->vec)
      (unit)))

(defn normalize [{:keys [x y z w]}]
  (let [l (sqrt (+ (* x x) (* y y) (* z z) (* w w)))]
    (->Quaternion (/ x l) (/ y l) (/ z l) (/ w l))))

(defn rotate-quaternion [q axis angle]
  (if (zero? angle)
    q
    (let [[a b c] (unit axis)
          rs (sin (/ angle 2.0 ))
          rc (cos (/ angle 2.0))
          qs (->Quaternion (* rs a) (* rs b) (* rs c) rc)]
      (assert (pos? angle))
      (multiply (-> qs normalize) q)))) ;;; XXX changed order

(defn axis
  "Return rotation axis as unit vector"
  ([q] (-> q quaternion->vec unit)))

;; XXX: impl this!
(defn scale
  "Return scale ratio of quaternion"
  ([q] 1.0))
