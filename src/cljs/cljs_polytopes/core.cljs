(ns cljs-polytopes.core
  (:require
   [cljsjs.react]
   [sablono.core :as sab :include-macros true]
   [monet.canvas :as canvas]
   [clojure.core.matrix :as mtrx :include-macros]
   [clojure.core.matrix.random :as rnd :include-macros]
   [cljs-polytopes.shapes :as shapes]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)
(def cube-vertices [
    [-1 -1 -1]
    [-1 -1 +1]
    [-1 +1 -1]
    [-1 +1 +1]
    [+1 -1 -1]
    [+1 -1 +1]
    [+1 +1 -1]
    [+1 +1 +1]
  ])
(def cube-edges [
    [0 4]
    [1 5]
    [2 6]
    [3 7]
    [0 2]
    [1 3]
    [4 6]
    [5 7]
    [0 1]
    [2 3]
    [4 5]
    [6 7]
  ])

; (defn floor [x] (.floor js/Math x))

; (defn translate [start-pos vel time]
;   (floor (+ start-pos (* time vel))))

(defn x-rotator-mat [alpha]
  [[1 0 0]
   [0 (mtrx/cos alpha) (mtrx/sin alpha)]
   [0 (- (mtrx/sin alpha)) (mtrx/cos alpha)]])

(defn y-rotator-mat [alpha]
  [
   [(mtrx/cos alpha) 0 (mtrx/sin alpha)]
   [0 1 0]
   [(- (mtrx/sin alpha)) 0 (mtrx/cos alpha)]])
(defn rotator-mat [alpha] (mtrx/mmul (x-rotator-mat alpha) (y-rotator-mat alpha)))

(defn random-vertices [n]
  (rnd/sample-normal [n 3]))

(defn generate-projection-func [camera-zoom camera-z offset-x offset-y]
  (fn [[x y z :as location]]
    (let [z-factor 1 ]
      [(+ offset-x (* camera-zoom z-factor x))
       (+ offset-y (* camera-zoom z-factor y))]
      )))
(def project (generate-projection-func 100 1 300 300))

(defn apply-forces-to-vertex-in-graph [graph vertices idx]
  (let
    [neighbors (graph idx)
     vertex (vertices idx)
     sum-of-forces 
      (concat 
              (for [neighbor neighbors]
                (mtrx/mul -0.1 (mtrx/sub (vertices neighbor) vertex)))
              (for [other vertices]
                (mtrx/mul 0.33 (mtrx/sub other vertex)))
      )
    ]
  (mtrx/normalise 
      (reduce mtrx/add (vertices idx) sum-of-forces)
   )
))

(defn apply-forces-to-graph [graph vertices]
  (vec (map (partial apply-forces-to-vertex-in-graph graph vertices)
    (range (count vertices)))))

(defn detect-edges [graph]
  (set (for [[a neighbours] graph
              b neighbours]
          (if (< a b) [a b] [b a]))))


(def starting-state {
  :cur-time 0
  :graph (:graph shapes/dodecahedron)
  :vertices (:coordinates shapes/dodecahedron)
  :timer-running true
  })

(defn go [state]
  (-> state
      (assoc
          :timer-running true)))

(defn reset-vertices [randomize state]
  (-> state
    (assoc
      :vertices (if randomize
                   (random-vertices (count (:vertices state) ))
                   (:vertices starting-state)))))


(defn time-update [timestamp state]
  (-> state
      (update :vertices (partial apply-forces-to-graph (:graph state)))
      (assoc :cur-time timestamp)))

(defonce poly-state (atom starting-state))

(defn time-loop [time]
  (let [
    new-state (swap! poly-state (partial time-update time))    
    ]
    (when (:timer-running new-state)
      (go
       (<! (timeout 30))
       (.requestAnimationFrame js/window time-loop)
       ))))

(defn start-game []
  (.requestAnimationFrame
   js/window
   (fn [time]      
     (time-loop time))))


(defn world [state]
  (assoc state
    :plotted-vertices (:vertices state)));)

(defn reset []
  (swap! poly-state (partial reset-vertices false)
  ))
(defn step []
  (swap! poly-state (partial time-update 0)
  ))
(defn debug-print []
  (print (:vertices @poly-state))
  )
(defn main-template [_]
  (sab/html [:div.board
    [:canvas#main-canvas {:width 600 :height 600}]
    [:button {:onClick reset} "Reset"]
    [:button {:onClick #(swap! poly-state (partial reset-vertices true))} "Random"]
    [:button {:onClick step} "Step"]
    [:button {:onClick debug-print} "Debug"]
    [:button {:onClick go} "Go"]]))

(let [node (.getElementById js/document "main-area")]
  (defn renderer [full-state]
    (.render js/React (main-template full-state) node)
    (let [dom (.getElementById js/document "main-canvas")
          ctx (.getContext dom "2d")]
      (set! (.-fillStyle ctx) "white")
      (.fillRect ctx 0 0 600 600)
      (doseq [[a b] (detect-edges (:graph full-state)) ]
        (let [[xa ya] (project ((:plotted-vertices full-state) a))
              [xb yb] (project ((:plotted-vertices full-state) b))]
          (.beginPath ctx)
          (.moveTo ctx xa ya)
          (.lineTo ctx xb yb)
          (.stroke ctx))
      ))))



(add-watch poly-state :renderer (fn [_ _ _ state]
                                  (renderer (world state))))

(start-game)