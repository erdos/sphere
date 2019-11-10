(ns erdos.lenart.state
  (:require [erdos.lenart.math :as m]
            [erdos.lenart.lang :as lang]
            [reagent.core :as reagent :refer [atom]])
  (:require-macros [erdos.lenart.macros :refer [defatom=]]))

(defonce editor-text
  (atom (str
         "X is point at 1 2 3" \newline
         "Y is point at -1 2 3" \newline
         \newline
         "x is great circle of X with stroke dashed" \newline
         "y is great circle of Y with stroke dotted" \newline

         "Z is intersection of x and y" \newline
         "z is great circle of Z with color lime"
         )))

(defonce arcball ; "Contains rotation information"
  (atom (m/->Quaternion 0 0 0 1.0)))

(defatom= construction-raw
  (lang/parse-book @editor-text))

(defatom= construction
  (when-not (:error @construction-raw)
    @construction-raw))

(defatom= error-msg
  (cond (:error @construction-raw)
        @construction-raw
       ))

(def pressed? (atom false))
(def hover    (atom nil)) ;; id of hovered object(s)
(def selected (atom nil)) ;; like hover

(def screen-cursor "Contains screen [x y] of cursor or nil" (atom nil))

(defatom= ^:once rotation (m/quaternion->vec @arcball))
(defatom= zoom 0)


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
