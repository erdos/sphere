(ns erdos.lenart.common
  (:require-macros [erdos.lenart.macros :refer [template format]])
  (:require [erdos.lenart.lang :as lang]
            [erdos.lenart.math :as m]))

(def default-style
     {:stroke "black"
      :fill "rgba(0,0,255,0.8)"
      :stroke-width 4
      :point-size 4
      :point-color "orange"})

(def ^:dynamic *style* default-style)

(defn color->rgba-
  [code]
  (let [e (.createElement js/document "div")]
    (aset e "style" "color" code)
    (.appendChild (.-body js/document) e)
    (let [c (.-color (.getComputedStyle js/window e))
          [r g b a] (mapv #(.parseFloat js/Number %)
                          (re-seq #"\d+(?:\.\d+)?" c))] [r g b (or a 1.0)])))

(defonce color->rgba (memoize color->rgba-))

(defn mean [a b] (/ (+ a b) 2))

(defn mean2 [a b] (/ (+ a b b b b b) 6))
(defn color->backface [c]
  (let [[r g b a] (color->rgba c)
        r (int (mean2 r 255))
        g (int (mean2 g 255))
        b (int (mean2 b 255))]
    (str "rgba(" r ","g ","b ","a ")")))

(defn color-lighter [c]
  (let [[r g b a] (color->rgba c)
        t 2
        ls (fn [x] (int (/ (+ (* x t) 255) (inc t))))
        r (ls r)
        g (ls g)
        b (ls b)]
    (str "rgba(" r ","g ","b ","a ")")))

(defn color-darker- [c]
  (let [[r g b a] (color->rgba c)
        t 0.4
        ls (fn [x] (int (/ (+ (* x t) 0) (inc t))))
        r (ls r)
        g (ls g)
        b (ls b)]
    (str "rgba(" r ","g ","b ","a ")")))

(def color-darker (memoize color-darker-))

(defn poly [d]
  (assert (string? d))
  [:path {:d d
          :fill (:fill *style* "blue")
          :stroke (color-darker (:fill *style* "blue"))
          :stroke-width 1}])

(defn path [d]
  (assert (string? d))
  [:path {:d d
          :fill :none
          :stroke (:stroke *style* "black")
          :stroke-width (:stroke-width *style* 3)}])

(defn arc [x0 y0, w h, ang, f0 f1, x1 y1]
  [:path
   {:d
    #_
    (template "M%d,%d A%d,%d %d %d,%d %d,%d "
              x0 y0 w h ang f0 f1 x1 y1)
    (format "M%.4f,%.4f A%.4f,%.4f %.4f %d,%d %.4f,%.4f" x0 y0 w h ang f0 f1 x1 y1)
    :fill   "none";(:fill *style* "none"),
    :stroke       (:stroke *style* "black")
    :stroke-dasharray (case (:stroke-style *style* :normal)
                        :dotted (str (:stroke-width *style* 2))
                        :dashed (str (* 4 (:stroke-width *style* 2))
                                     " "
                                     (* 2 (:stroke-width *style* 2)))
                        :normal nil)
    :stroke-width (:stroke-width *style* 1)}])

(defn line [x0 y0 x1 y1]
  [:path {:fill "none";(:fill *style* "yellow")
          :stroke (:stroke *style* "red")
          :stroke-width (:stroke-width *style* 1)
          :d
          ;;(template "M%,% L%,%" x0 y0 x1 y1)
          (format "M%.4f,%.4f L%.4f,%.4f" x0 y0 x1 y1)
          }])
