(ns ^:figwheel-always erdos.lenart.core
    (:require [reagent.core :as reagent
               :refer [atom]]
              [erdos.lenart.common :as c
               :refer [*style* *zoom* format
                       pressed? hover selected construction editor-text error-msg]]
              [erdos.lenart.math :as m :refer
               [sin atan2 rad->deg
                deg->rad unit clockwise?]]
              [erdos.lenart.lang :as lang]
              [erdos.lenart.orthographic
               :refer [create-segment create-segment-backface create-point
                       create-great-circle create-great-circle-backface
                       create-poly create-poly-backface]])
    (:require-macros [erdos.lenart.macros
                      :refer [defatom= obj->]]))

(enable-console-print!)

(defn on-js-reload [])

(defonce arcball ; "Contains rotation information"
  (atom (m/->Quaternion 0 0 0 1.0)))

(let [a (atom 0)]
  (defn genint [] (swap! a inc)))

(defatom= ^:once rotation (m/quaternion->vec @arcball))
(defatom= zoom 0)


(def projection "Current projection object" (atom m/orthographic-projection))

(def screen-cursor "Contains screen [x y] of cursor or nil" (atom nil))

;((fn-> println) 5)

(defatom= cursor
  (when-let [[x y] @screen-cursor]
    (m/euclidean->spherical @projection x y)))

;; TODO: impl this
(defn drag-object! "Evt handler. should modify state." [id location] nil)

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


(defn on-mouse-move "evt handler" [e]
  (let [svg (or (obj-> e target ownerSVGElement) (.-target e))
        q (doto (.createSVGPoint svg)
            (-> .-x (set! (.-clientX e)))
            (-> .-y (set! (.-clientY e))))
        p (obj-> q (matrixTransform (obj-> e target (getScreenCTM) (inverse))))]
    (reset! screen-cursor
            [(/ (.-x p) *zoom*) (/ (.-y p) *zoom*)])

    nil
    ))

;;[:b [:h1 "asdsad" tyty]]


(defn gr
  "Sphere display component"
  []
  (let [size 420
        pt (memoize (fn [x] (m/rotate x @arcball)))]
    [:svg {:width size :height size
           :style {:touch-action "none"}
           }
     [:defs

      [:pattern {:id "diag" :patternUnits "userSpaceOnUse" :width 4 :heigt 4}
       [:path {:d "M-1,1 l2,-2
           M0,4 l4,-4
           M3,5 l2,-2"}]
       ]

      [:radialGradient { :id "grad1" :cx "50%" :cy "50%" :r "50%"
                        :fx "50%" :fy "50%"}
       [:stop {:offset "70%" :style {:stop-color "white"}}]
       [:stop {:offset "90%" :style {:stop-color "#fafafa"}}]
       [:stop {:offset "100%" :style {:stop-color "#eeeeee"}}]]]
     [:g {:transform (format "translate(%d,%d)" (/ size 2) (/ size 2))
          ;:on-mouse-down #(do (reset! pressed? true) nil)
          :on-pointer-down #(do (reset! pressed? true) nil)
          ;:on-mouse-up   #(do (reset! pressed? false) nil)
          :on-pointer-up #(do (reset! pressed? false) nil)
          ;;:on-mouse-click (fn [e] (println e))
          :on-pointer-move on-mouse-move
          }
      [:circle {:cx 0 :cy 0 :r *zoom* :fill "url(#grad1)" :stroke "#aaaaaa"}]

      [:g
       ;; backface
       (binding [*style* (assoc *style*
                           :stroke      (c/color->backface (:stroke *style*))
                           :point-color (c/color->backface (:point-color *style*))
                           :fill (c/color->backface (:fill *style*)))]
        (doall
         (for [x (reverse @construction)]
           (-> (case (:type x)
              :segment (create-segment-backface  (pt (:from x)) (pt (:to x)))
              :point   (create-point (m/mirror (pt  (:loc x))))
              :great-circle (create-great-circle-backface (pt (:origin x)))
              :polygon (create-poly-backface (map pt (:pts x)))
              nil)
               (with-meta {:key (genint)})))))

       ]
      [:g
            ;; frontface
      (doall (for [x @construction]
         (-> (case (:type x)
               :segment (create-segment (pt (:from x)) (pt (:to x)))
               :point   (create-point (pt (:loc x)))
               :great-circle (create-great-circle (pt (:origin x)))
               :polygon (create-poly (map pt (:pts x)))
               nil)
             (with-meta {:key (genint)}))))

       ]

      ]]))



(defn editor
  "Code editor component"
  []
  ;; HELP INFO
  (let []
    [:div {}
     [:textarea
      {:on-change  (fn [x]
                     (reset! editor-text (-> x .-target .-value)))
       :rows (count (seq (.split @editor-text "\n")))
       :style {:font-family "Times"
               :font-size :1.2em
               :min-width :300px
               :width "auto" :resize :none :border "1px solid silver" :outline :none}
       :value @editor-text}]
     ;; error messages are here
     [:div @error-msg]]))


(defn container []
  [:div {:style {:display :flex
                 :flex-direction :row
                 :flex-wrap :wrap
                 :justify-content :center
                 :align-items :center
                 :align-content :stretch}}
   [:div {:style {:display :inline-block :flex-basis "440px"}} [gr]]
   [:div {:style {:display :inline-block}}[editor]]])


(reagent/render [container] (js/document.getElementById "app"))

; (.addEventListener js/document "touchmove" (fn [e] (.preventDefault e)))

'''''''((fn f [e]
   (swap! arcball m/rotate-quaternion m/up 0.02)
   (.setTimeout js/window f 50)) nil)
