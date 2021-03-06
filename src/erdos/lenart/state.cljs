(ns erdos.lenart.state
  (:require [erdos.lenart.math :as m]
            [erdos.lenart.lang :as lang]
            [reagent.core :as reagent :refer [atom]])
  (:require-macros [erdos.lenart.macros :refer [defatom=]]))


(->> ["X is point at 12 13 14"
      "Y is point at -13 3 13"
      ""
      "x is great circle of X with stroke dashed"
      "y is great circle of Y with stroke dotted"
      "Z is intersection of x and y"
      "z is great circle of Z with color olive"
      ""
      "draw polygon of X Y Z with color wheat"
      ""
      "Z' is antipode of Z"
      "draw segment between X and Z'"]
     (clojure.string/join \newline)
     (atom)
     (defonce editor-text))


(def arcball0 (m/->Quaternion 0 0 0 1.0))

(defonce arcball ; "Contains rotation information"
  (atom arcball0))

(defatom= construction-raw
  (lang/parse-book @editor-text))

(defatom= construction
  (:construction @construction-raw))

(defatom= error-msg
  (:err @construction-raw))

(def pressed? (atom false))
(def hover    (atom nil)) ;; id of hovered object(s)
(def selected (atom nil)) ;; like hover

(def screen-cursor "Contains screen [x y] of cursor or nil" (atom nil))

(defatom= ^:once rotation (m/quaternion->vec @arcball))
(def zoom 190)
; (defatom= zoom 0)

(def canvas-size 400)

(def projection "Current projection object" (atom m/orthographic-projection))


(defn drag-object! "Evt handler. should modify state." [id location] nil)

(defatom= cursor
  (when-let [[x y] @screen-cursor]
    (m/euclidean->spherical @projection x y)))

(add-watch cursor :drag
 (fn [_ _ x' x]
   (when (and @pressed? x x')
     (if-let [h @selected]
       (drag-object! h x)
       (swap! arcball m/rotate-quaternion
              (m/unit (m/cross x' x))
              (m/dist-angle (m/unit x') (m/unit x)))))))

(add-watch pressed? :click
 (fn [_ _ _ x]
   (if x
     (when (and (not @selected) @hover)
       (reset! selected @hover))
     (do (reset! selected nil)
         (reset! screen-cursor nil)))))
