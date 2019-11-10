(ns ^:figwheel-always erdos.lenart.core
  (:require [reagent.core :as reagent]
            [erdos.lenart.state :as state :refer [editor-text error-msg]]
            [erdos.lenart.common :as c :refer [*style* *zoom* format]]
            [erdos.lenart.lang :as lang]
            [erdos.lenart.canvas :refer [gr]]))

(enable-console-print!)

(defn on-js-reload [])

(defn- on-editor-text-change [^js/InputEvent x]
  (reset! editor-text (.. x -target -value)))

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


(defn- container [] [:div#Container [:div#Sphere [gr]]  [editor]])

(reagent/render [container] (js/document.getElementById "app"))

(println "Loaded!")
