(set-env!
 :resource-paths #{"src" "html"}
 :dependencies '[[adzerk/boot-cljs            "2.1.5"          :scope "test"]
                 [adzerk/boot-cljs-repl       "0.4.0"          :scope "test"]
                 [adzerk/boot-reload          "0.6.0"          :scope "test"]
                 [pandeiro/boot-http          "0.8.3"          :scope "test"]
                 [crisptrutski/boot-cljs-test "0.3.4"          :scope "test"]
                 [org.clojure/clojure         "1.10.0"]
                 [org.clojure/clojurescript   "1.10.520"]
                 [cljsjs/react "16.11.0-0"]
                 [reagent "0.9.0-rc2" :exclusions [cljsjs/react]]

                 [cider/piggieback            "0.4.2"          :scope "test"]
                 [weasel                      "0.7.0"          :scope "test"]
                 [org.clojure/tools.nrepl     "0.2.13"         :scope "test"]])

(require
  '[adzerk.boot-cljs      :refer [cljs]]
  '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
  '[adzerk.boot-reload    :refer [reload]]
  '[crisptrutski.boot-cljs-test  :refer [test-cljs]]
  '[pandeiro.boot-http    :refer [serve]])

(deftask auto-test []
  (merge-env! :resource-paths #{"test"})
  (comp (watch)
        (speak)
        (test-cljs)))

(deftask dev []
  (comp (serve :dir "resources/public" :port 8000)
        (watch)
        (speak)
        (reload :on-jsload 'erdos.lenart.core/on-js-reload)
        (cljs-repl)
        (cljs :source-map true
              :optimizations :none)))

(deftask production []
  (comp (cljs :optimizations :advanced
              :compiler-options {:externs ["externs.js"]})
        (target)))
