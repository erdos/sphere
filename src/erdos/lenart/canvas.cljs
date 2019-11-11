(ns erdos.lenart.canvas
  (:require [reagent.core :as reagent :refer [atom]]
            [erdos.lenart.common :as c :refer [*style* *zoom* format]]
            [erdos.lenart.state :as state :refer [editor-text error-msg arcball construction pressed? hover selected screen-cursor]]
            [erdos.lenart.math :as m]
            [erdos.lenart.lang :as lang]
            [erdos.lenart.orthographic
             :refer [create-segment create-segment-backface create-point
                     create-great-circle create-great-circle-backface
                     create-poly create-poly-backface]])
  (:require-macros [erdos.lenart.macros :refer [defatom=]]))

(defn on-mouse-move "evt handler" [^js/MouseEvent e]
  (let [e-target ^js/SVGElement (.-target e)
        svg (or (.-ownerSVGElement e-target) e-target)
        q (doto (.createSVGPoint svg)
            (-> .-x (set! (.-clientX e)))
            (-> .-y (set! (.-clientY e))))
        p (.matrixTransform q (-> e-target (.getScreenCTM) (.inverse)))]
    (reset! state/screen-cursor [(/ (.-x p) *zoom*) (/ (.-y p) *zoom*)])
    nil))

(def defs
  [:defs
   #_[:pattern {:id "diag" :patternUnits "userSpaceOnUse" :width 4 :heigt 4}
    [:path {:d "M-1,1 l2,-2
           M0,4 l4,-4
           M3,5 l2,-2"}]
    ]

   [:radialGradient { :id "grad1" :cx "50%" :cy "50%" :r "50%"
                     :fx "50%" :fy "50%"}
    [:stop {:offset "70%" :style {:stop-color "white"}}]
    [:stop {:offset "90%" :style {:stop-color "#fafafa"}}]
    [:stop {:offset "100%" :style {:stop-color "#eeeeee"}}]]])

(defn- backface [pt]
  (binding [*style* (assoc *style*
                           :stroke      (c/color->backface (:stroke *style*))
                           :point-color (c/color->backface (:point-color *style*))
                           :fill (c/color->backface (:fill *style*)))]
    [:g
     (doall
      (for [x (reverse @state/construction)
            :when (not (:hidden x true))]
        (binding [*style* (assoc *style*
                                 :fill (c/color->backface (:color x "blue"))
                                 :stroke (c/color->backface (:color x "black"))
                                 :stroke-style (:stroke-style x :normal)
                                 :stroke-width (:size x 2))]
          (-> (case (:type x)
                :segment (create-segment-backface  (pt (:from x)) (pt (:to x)))
                :point   (create-point (:id x) (m/mirror (pt (:loc x))))
                :great-circle (create-great-circle-backface (pt (:origin x)))
                :polygon (create-poly-backface (map pt (:pts x)))
                nil)
              (with-meta {:key (:id x)})))))]))

(defn- frontface [pt]
  [:g
   (doall
    (for [t [:polygon :great-circle :segment :point]
          x @state/construction
          :when (= t (:type x))
          :when (not (:hidden x true))]
      (binding [*style* (assoc *style*
                               :fill (:color x "blue")
                               :stroke (:color x "black")
                               :stroke-style (:stroke-style x :normal)
                               :stroke-width (:size x 2))]
        (-> (case (:type x)
              :segment (create-segment (pt (:from x)) (pt (:to x)))
              :point   (create-point (:id x) (pt (:loc x)))
              :great-circle (create-great-circle (pt (:origin x)))
              :polygon (create-poly (map pt (:pts x)))
              nil)
            (with-meta {:key (:id x)})))))])

(defn gr []
  (let [size 400
        pt (memoize (fn [x] (m/rotate x @state/arcball)))]
    [:svg {:width size :height size :style {:touch-action "none"}}
     defs
     [:g {:transform (format "translate(%d,%d)" (/ size 2) (/ size 2))
          ;:on-mouse-down #(do (reset! pressed? true) nil)
          :on-pointer-down #(do (reset! pressed? true) nil)
          ;:on-mouse-up   #(do (reset! pressed? false) nil)
          :on-pointer-up #(do (reset! pressed? false) nil)
          ;;:on-mouse-click (fn [e] (println e))
          :on-pointer-move on-mouse-move}
      [:circle {:cx 0 :cy 0 :r *zoom* :fill "url(#grad1)" :stroke "#aaaaaa"}]
      (backface pt)
      (frontface pt)]]))

:ok
