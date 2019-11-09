(ns ^:figwheel-always erdos.lenart.core
    (:require [reagent.core :as reagent
               :refer [atom]]
              [erdos.lenart.state :as state :refer [editor-text error-msg]]
              [erdos.lenart.common :as c :refer [*style* *zoom* format]]
              [erdos.lenart.lang :as lang]
              [erdos.lenart.canvas :refer [gr]])
    (:require-macros [erdos.lenart.macros
                      :refer [obj->]]))

(enable-console-print!)

(defn on-js-reload [])

(defn- on-editor-text-change [x] (reset! editor-text (obj-> x target value)))

(defn editor []
  [:div {}
   [:div {:style {:position "absolute" :top 0}} (str @state/hover)]
   ;; [:pre (str @construction)]
   [:textarea
    {:on-change  on-editor-text-change
     :rows (count (seq (.split @editor-text "\n")))
     :style {:font-family "Times"
             :font-size :1.2em
             :min-width :300px
             :width "auto"
             :resize :none
             :border "1px solid silver"
             :outline :none}
     :value @editor-text}]
   ;; error messages are here
   [:div @error-msg]])


(defn- container []
  [:div#Container
   [:div#Sphere [gr]]
   [:div#Editor [editor]]])


(reagent/render [container] (js/document.getElementById "app"))

; (.addEventListener js/document "touchmove" (fn [e] (.preventDefault e)))

'''''''((fn f [e]
   (swap! arcball m/rotate-quaternion m/up 0.02)
   (.setTimeout js/window f 50)) nil)
