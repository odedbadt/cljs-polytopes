(ns ^:figwheel-always cljs-polytopes.graph-test
  (:require
    [cljs.test :refer-macros [deftest testing is run-tests]]
    [cljs-polytopes.graph :as graph]
    [figwheel.client :as fw]))

(enable-console-print!)

(deftest test-duplicate-and-transform-graph
  (is (= 
        {
          0 ["A" true [1 2]]
          1 ["B" true [0 2]]
          20 ["C" false [0 1]]
          21 ["tA" true [22 23]]
          22 ["tB" true [21 23]]
          41 ["tC" false [21 22]]
        }

        (graph/duplicate-and-transform-graph
          {
            0 ["A" true [1 2]]
            1 ["B" true [0 2]]
            20 ["C" false [0 1]]
          } 
          "t")
    )))

 (deftest test-apply-word-rules
   (is (= 
          (graph/apply-word-rules "AA" [["AA" "B"]])
         "B"
       ))
   (is (= 
          (graph/apply-word-rules "AAA" [["AA" "A"]])
         "A"
       ))

   )

 (deftest test-transform-and-redue-graph
   (is (= {
              21 20
              41 0
          }
          (get (:new-graph (graph/transform-and-reduce-graph
            [{
              0 ["A" true [20]]
              20 ["BA" false [0]]
            } {}]
            "B"
           [["BBA" "A"] ["AA" "A"]])) 1)
            
       )
   )
   (is
       (= {
              0  ["A" true [20]]
              20 ["BA" false [0]]
              21 ["BA" true [41]]
              41 ["BBA" false [21]]
            }
          (get (:new-graph (graph/transform-and-reduce-graph
            [{
              0 ["A" true [20]]
              20 ["BA" false [0]]
            } {}]
            "B"
           [["BBA" "A"] ["AA" "A"]])) 0)
            
       ))   
   )

(deftest test-pairinduction
 (is (= [2 2] (graph/pair-induction [nil 2])))
 (is (= [2 2] (graph/pair-induction [2 nil])))
 (is (= [2 3] (graph/pair-induction [2 3]))))

(deftest test-flatten-merging-table
  (is (= {0 1, 2, 3}
    (graph/flatten-merging-table {0 1, 2 3})))
  (is (= {0 2, 1 2}
    (graph/flatten-merging-table {0 1, 1 2})))
  (is (= {0 2, 1 2, 3 5}
    (graph/flatten-merging-table {0 1, 1 2, 3 5})))
  (is (= {0 7, 1 7, 2, 7}
    (graph/flatten-merging-table {0 1, 1 2, 2, 7}))))

(defn cyclic-equals [l1 l2]
  (if (= (count l1) (count l2))
    (let [c (count l1)
          l2_0 (first l2)
          value-matches (comp (partial = l2_0) second)
          indexed-l1 (map-indexed vector l1)
          [offset _] (first (filter value-matches indexed-l1))
          ]
          (if offset
            (every? identity (take c 
              (map = (drop offset (cycle l1)) l2))))
          )))

(deftest test-common-cycle
  (is (cyclic-equals [[1 3] [2 2] [4 5] [6 6]]
         (second 
           (graph/common-cycle [true [1 2 4 6]] [true [3 2 5 6]]))))
  (is (cyclic-equals [[6 6] [1 7] [2 8] [4 4]]
         (second 
           (graph/common-cycle [true [1 2 4 6]] [true [6 7 8 4]]))))
  (is (= nil
         (graph/common-cycle [true [1 2 4 6 9]] [true [6 7 8 4]])))
  (is (= nil
         (graph/common-cycle [false [1 2 4 6]] [true [6 7 8 4]])))
  (is (= nil
         (graph/common-cycle [true [8 2 4 6]] [true [6 7 8 4]])))
)

(run-tests)

;; FW connection is optional in order to simply run tests,
;; but is needed to connect to the FW repl and to allow
;; auto-reloading on file-save
(fw/start {
           :websocket-url "ws://localhost:3449/figwheel-ws"
           ;; :autoload false
           :build-id "test"
           })