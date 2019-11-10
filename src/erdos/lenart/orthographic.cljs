(ns erdos.lenart.orthographic
  (:require [erdos.lenart.common :as c
             :refer [*style* *zoom* format arc]]
            [erdos.lenart.state :as state]
            [erdos.lenart.math :as m
             :refer [atan2 sin cos sqrt rad->deg unit cross up clockwise?]]))

(defn create-great-circle [aa]
  (let [[ax ay az :as aa] (m/unit aa)
        [x0 y0 z0 :as a0] (m/unit (m/cross m/up aa))
        [x1 y1] [(- x0) (- y0)]]
    (if (pos? az)
      (recur (m/antipode aa))
      (when-not (== 0 ax ay)
        (arc (* x0 *zoom*) (* y0 *zoom*)
             (* *zoom* (js/Math.abs az)) *zoom*
             (-> (m/atan2 ay ax) m/rad->deg)
             0 0
             (* x1 *zoom*) (* y1 *zoom*)
             )))))

(defn create-great-circle-backface [aa]
  (create-great-circle (m/mirror aa)))


(defn create-point [id pt]
  (let [[x y z] (m/unit pt)
        r  (:point-size *style* 6)]
    (when (pos? z)
      [:rect {:x (- (* x *zoom*) r) :y (- (* y *zoom*) r)
              :width (* 2 r) :height (* 2 r)
              :on-mouse-over #(reset! state/hover id)
              :on-mouse-out #(reset! state/hover nil)
              :fill (or (:point-color *style*) (:color *style*) "red")
              :stroke :red}])))

(defn- create-segment-path-goto-str [aa bb]
  (assert (not= aa bb))
  (assert (and (-> aa (nth 2) neg? not)
               (-> bb (nth 2) neg? not)))
  (let [[ax ay az :as aa] (unit aa)
        [bx by bz :as bb] (unit bb)
        [ox oy oz :as oo]        (unit (m/cross aa bb))
        cw (clockwise? aa bb [0.0 0.0 1.0])]
    (assert (every? some? aa))
    (assert (every? some? bb))
    (assert (every? some? oo))
    (cond
     (> (/ 1 64) (js/Math.abs oz))
     (format "L%.4f,%.4f" (* *zoom* bx) (* *zoom* by))
     #_(< 0.99  (js/Math.abs oz))
     #_(format "A%.4f,%.4f %.4f %d,%f %.4f,%.4f"
               *zoom* *zoom*
               (-> (m/atan2 oy oz) m/rad->deg (+ 180))
               0 0 (* bx *zoom*) (* by *zoom*))
     :arc
     (format "A%.4f,%.4f %.4f %d,%d %.4f,%.4f"
             (* *zoom*  (js/Math.abs oz)) *zoom*
             (-> (m/atan2 oy ox)  m/rad->deg  (+ 180.0))
             0 (if (pos? oz)  1 0)
             (* bx *zoom*) (* by *zoom*)))))

(defn- create-segment-path-str [aa bb]
  (assert (and (-> aa (nth 2) neg? not)
               (-> bb (nth 2) neg? not)))
  (when (not= aa bb)
    (str (format "M%.4f %.4f " (* *zoom* (first aa)) (* *zoom* (second aa)))
         (create-segment-path-goto-str aa bb))))


(defn- create-segment-path-part
  ;; returns [aa bb str]
  [aa bb]
  (let [aa (m/unit aa), bb (m/unit bb)
        oo (m/unit (m/cross (m/unit aa) (m/unit bb)))
        a? (-> aa (nth 2) neg? not)
        b? (-> bb (nth 2) neg? not)]
    (if a?
      (if b?
        [aa bb (create-segment-path-str aa bb)]
        [aa
         (m/unit (m/cross oo m/up))
         (create-segment-path-str
          aa
          (m/unit (m/cross oo m/up)))])
      (if b?
        [(m/unit (m/cross m/up oo)) bb
         (create-segment-path-str
          (m/unit (m/cross m/up oo)) bb)]))))

(defn- create-segment-path-part-1 [aa bb]
  (create-segment-path-part aa bb))


(defn create-segment [aa bb]
  (let [[[x y] _ s] (create-segment-path-part aa bb)
        m (str "M" (* x *zoom*) "," (* y *zoom*))]
    (c/path (str m " " s))))

(defn create-segment-backface [[ax ay az] [bx by bz]]
  (create-segment [ax ay (- az)] [bx by (- bz)]))


;; IDEA
(defn create-poly-1 [p-q-s]
  ; arg: [[p q s]]
  (let [m (str "M"
               (* *zoom* (first (first (last p-q-s)))) ","
               (* *zoom* (second (first (last p-q-s)))))]
    (->
     (map (fn [[_ q1, s1]
              [p2 _, s2]]
            (if (= q1 p2)
              (str s1 " ")
              (let [[_ _ s12] (create-segment-path-part-1 q1 p2)]
                (str s1 " " s12 " "))))
          (cons (last p-q-s) p-q-s) p-q-s)
     (->> (apply str m " "))
     (str " z"))))

;; IDEA
(defn create-poly-1+
  [pts]
  (let [p-q-s (map create-segment-path-part
                   (cons (last pts) pts) pts)
        p-q-s (vec (remove nil? p-q-s))]
    (-> (create-poly-1 p-q-s))))



;; IDEA ONLY
(defn islands [f xs]
  ;; creates islands where [nf f ... f nf] hold.
  (when (seq xs)
    (let [[a b] (split-with (complement f) xs)
          [b c] (split-with f b)
          m (concat (some-> (last a) vector)
                    b
                    (some-> (first c) vector))]
      (cons m (lazy-seq (islands f c))))))

(defn- triangle-pts [a b c]
  (let [[ax ay az] a
        [bx by bz] b
        [cx cy cz] c]
    (cond-> ()
            (pos? bz) (conj b)
            (and (neg? bz) (pos? az)) (conj (m/unit (m/cross (m/cross a b) m/up)))
            (and (pos? bz) (neg? az)) (conj (m/unit (m/cross (m/cross b a) m/up)))
            (pos? az) (conj a)
            (and (neg? az) (pos? cz)) (conj (m/unit (m/cross (m/cross c a) m/up)))
            (and (pos? az) (neg? cz)) (conj (m/unit (m/cross (m/cross a c) m/up)))
            (pos? cz) (conj c)
            (and (neg? cz) (pos? bz)) (conj (m/unit (m/cross (m/cross b c) m/up)))
            (and (pos? cz) (neg? bz)) (conj (m/unit (m/cross (m/cross c b) m/up))))))

(defn- points->triangles [pts]
  ; triangle strip
  (mapv vector pts (next pts) (nnext pts)))

(defn- points->path [pts]
  (let [p-q-s (apply str (map create-segment-path-goto-str (cons (last pts) pts) pts))
        ;;
        p-q-s
        (str "M" (* *zoom* (first (last pts)))
             "," (* *zoom* (second (last pts)))
             " " p-q-s " z ")]
    p-q-s))

(defn- points->line-path [pts]
  (str
   "M" (* *zoom* (first (last pts))) "," (* *zoom* (second (last pts))) " "
   (apply str
          (for [[x y] pts]
            (str "L" (* *zoom* x) "," (* *zoom* y) " ")))
   " z "))


(defn create-poly [pts]
  (let [ts (points->triangles pts)
        ps (map (partial apply triangle-pts) ts)]
    ;;(println " - ") (doseq [p ts] (println (clj->js p)))
    (->
     (apply str (map points->path ps))
     (c/poly))))

(defn create-poly-backface [pts]
  (create-poly (map m/mirror pts)))


:OK
