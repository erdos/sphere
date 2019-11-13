(ns ^:figwheel-always erdos.lenart.core
  (:require [reagent.core :as reagent]
            [erdos.lenart.state :as state :refer [editor-text error-msg]]
            [erdos.lenart.common :as c :refer [*style* *zoom*]]
            [erdos.lenart.lang :as lang]
            [erdos.lenart.canvas :refer [gr]]))

(enable-console-print!)

(defn on-js-reload [])

(defn- on-editor-text-change [^js/InputEvent x]
  (reset! editor-text (.. x -target -value)))

(def ^:private hand-path
  [:svg {:width 32
         :height 32
         :viewBox "0 0 32 32"}
   [:path
    {:d "m 16,1 c -2,0 -2,3 -1.7,4.8 0,2.6 0,4 0,7 -1,-2 -1.8,-5.6 -3,-8.4 -0.6,-1.6 -3.3,-1.7 -3.4,0 0.0,2 1,4 1.5,6 l 1,5 c 0,1 -0.3,2.5 -1.5,1.4 -1.4,-1 -2,-3 -4.3,-2.6 -1.1,0.2 -2.2,0.4 -1.7,2.5 1.6,1.5 3.2,2.9 4.4,4.8 1.2,1.7 2.4,3.4 4.2,4.5 0.8,0.4 1.3,1 1,2 v 1.5 c 2.7,1 5.8,1.3 8.5,0 -0.6,-1.7 1,-2.5 1.7,-4 0,-1.4 1,-2.8 1.4,-4.3 0.3,-1.3 0,-2.5 0,-3.8 0.8,-3 2.2,-4.7 4.1,-8 0.5,-1.5 -1.3,-3 -2.6,-2 -1.2,1.1 -1.8,2.6 -2.7,4 l -1.4,2.4 c 0.7,-3 0.0,-5.7 2.3,-9.2 0.3,-1.7 -2,-2.8 -3.1,-1.4 -1.4,2.5 -1.3,3.3 -1.8,5 l -1.2,4.4 c -0,-3 0,-6.4 -0,-9.7 0,-1.4 -1,-1.8 -1.9,-1.7 z"
     :style {:fill "white"
             :stroke "black"
             :stroke-width 1}}]])

(defn editor []
  [:div#Editor
   [:div {:style {:position "absolute" :top 0 :right "50%"}} (str @state/hover)]
   ;; [:pre (str @construction)]
   [:textarea
    {:on-change on-editor-text-change
     :rows (count (seq (.split (str @editor-text) "\n")))
     :value @editor-text}]
   ;; error messages are here
   (when-let [e (:error @error-msg)]
     [:div#Error
      [:h3 (str e)]
      (when-let [line (:line @error-msg)]
        [:p "Line: " [:code line]])])
   #_[:div [:label.ui [:input {:type "checkbox" :value false}] [:span "Rotate sphere"]] ]])

(defn hand []
  (when (identical? @state/arcball state/arcball0)
    [:div#Hand {:on-mouse-over #(swap! state/arcball assoc :changed true)} hand-path]))

(defn- container []
  [:div#Container
   [:div#Sphere [hand] [gr]]
   [editor]])

(reagent/render [container] (js/document.getElementById "app"))

(println "Loaded!")
