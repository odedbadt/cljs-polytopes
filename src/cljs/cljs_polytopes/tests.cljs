(ns cljs-polytopes.tests
   (:require [cljs.test :refer-macros [deftest is testing run-tests]]
             [cljs-polytopes.graph :as graph]))



(enable-console-print!)
(defn go[] 
  (run-tests))

(go)
(print "AAA")