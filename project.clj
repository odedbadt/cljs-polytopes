(defproject cljs-polytopes "0.1.1-SNAPSHOT"
  :description "Polytopes demo"
  
  :url "http://rigsomelight.com/2014/05/01/interactive-programming-flappy-bird-clojurescript.html"

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [org.clojure/core.async "0.2.371"]
                 [cljsjs/react "0.13.3-1"]
                 [sablono "0.4.0"]
                 [rm-hull/monet "0.3.0"]
                 [thinktopic/aljabr "0.1.1"]
                 [net.mikera/core.matrix "0.56.0"]]

  :plugins [[lein-cljsbuild "1.1.0"]
            [lein-figwheel "0.5.0-SNAPSHOT"]]

  :clean-targets ^{:protect false} ["resources/public/js/out"
                                    "resources/public/js/cljs-polytopes.js"
                                    :target-path]  
  
  :source-paths ["src"]

  :cljsbuild { 
    :builds [{:id "cljs-polytopes"
              :source-paths ["src/cljs"]
              :figwheel true
              :compiler {
                         :main cljs-polytopes.core
                         :asset-path "js/out"
                         :output-to "resources/public/js/cljs-polytopes.js"
                         :output-dir "resources/public/js/out"
                         :source-map-timestamp true}}]}
  
  :figwheel { :css-dirs ["resources/public/css"]
              :open-file-command "emacsclient"
             })
