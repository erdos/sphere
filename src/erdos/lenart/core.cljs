(ns ^:figwheel-always erdos.lenart.core
    (:require [reagent.core :as reagent
               :refer [atom]]
              [erdos.lenart.state :as state :refer [editor-text error-msg]]
              [erdos.lenart.common :as c :refer [*style* *zoom* format]]
              [erdos.lenart.lang :as lang]
              [erdos.lenart.canvas :refer [gr]]))

#_ (enable-console-print!)

(defn on-js-reload [])

(defn- on-editor-text-change [^js/InputEvent x]
  (reset! editor-text (.. x -target -value)))

(defn editor []
  [:div#Editor
   [:div {:style {:position "absolute" :top 0}} (str @state/hover)]
   ;; [:pre (str @construction)]
   [:textarea
    {:on-change on-editor-text-change
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
   [:div @error-msg]
   #_[:div [:label.ui [:input {:type "checkbox" :value false}] [:span "Rotate sphere"]] ]])


(defn- container [] [:div#Container [:div#Sphere [gr]]  [editor]])

(reagent/render [container] (js/document.getElementById "app"))
