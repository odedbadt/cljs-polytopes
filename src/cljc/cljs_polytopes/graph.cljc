(ns cljs-polytopes.graph
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn duplicate-and-transform-graph [graph transformation]
  (let [
    l (count graph)
    offset (inc (- (reduce max (keys graph)) (reduce min (keys graph))))
    to-offset-id (fn [x] (if x (+ x offset)))
    ]
  (into graph
      (for [[id [formula cyclic neighbors]] graph] 
        [(to-offset-id id) [(str transformation formula) cyclic (vec (map to-offset-id neighbors))]])      
    )))

(defn apply-word-rules [word rule-book]
  (loop [word word
         [[from to] & rest-of-rules :as rule-book] rule-book]
      (if (and rule-book (not (empty? word)))
        (if (string/starts-with? word from)
          (recur (string/replace word from to) rule-book)
          (recur word rest-of-rules)
        )
        word
      )
  )
)

(defn flatten-merging-table [merging-table]
  (loop [merging-table merging-table]
    (let [      
      mapped-entries (for [[k v] merging-table]
               [(contains? merging-table v) k (or (get merging-table v) v)])]
      (if (some first mapped-entries)
        (recur (into {} (for [[a b c] mapped-entries] [b c])))
        merging-table))))

(defn pair-induction [[a b]]
  (cond 
    (and (not a) (not b)) [nil nil]
    (not a) [b b]
    (not b) [a a]
    :else [a b]))

(defn transform-and-reduce-graph [[graph _] transformation rule-book]
  (let [
    new-graph-structure (duplicate-and-transform-graph graph transformation)
    keyed-common-formula-groups (group-by
       (fn [id]        
          (let [[formula cyclic neighbors] (get new-graph-structure id)]
            (apply-word-rules formula rule-book)))
       (sort > (keys new-graph-structure)))
    common-formula-groups (vals keyed-common-formula-groups)
    equivalence-table (into {} common-formula-groups)
    ]
    (when (some (partial < 2) (map count common-formula-groups))
      (throw "Group larger than 2"))
    {
     :new-graph [new-graph-structure equivalence-table]
     :common-formula-groups-dbg common-formula-groups     
    }))

#?(:clj
   (defn -index-of [l x] (.indexOf l x)))

#?(:cljs
   (defn -index-of [l x] (.indexOf (apply array l) x)))


(defn common-cycle [[cyclic1 list1] [cyclic2 list2]]
  (if (and cyclic1 cyclic2 (= (count list1) (count list2)))
    (let [l (count list1)
          common-members (set/intersection (set list1) (set list2))
          first-common-member (first common-members)
          offset1 (-index-of list1 first-common-member)
          offset2 (-index-of list2 first-common-member)
          candidate (map
           (fn [idx]
             (pair-induction [(get list1 (mod (+ idx offset1) l))
                              (get list2 (mod (+ idx offset2) l))]))
             (range l))
          ]
      (if (every? identity (for [[a b] candidate]
            (or (= a b)
                (not (contains? common-members a))
                (not (contains? common-members b)))))
          [true candidate]))))

(defn -get-member-or-index [coll idx] (or (coll idx) idx))

(defn -replace-with-equivalent [graph-structure equivalence-table]
  (for [[idx [formula cyclic neighbors]] graph-structure]
                 [(-get-member-or-index equivalence-table idx)
                  [formula cyclic (map (partial -get-member-or-index equivalence-table) neighbors)]]))

(defn merge-vertices [[graph-structure equivalence-table :as graph] src dst]
  (if (or (= src dst)
          (not (and (contains? graph src) (contains? graph dst))))
    graph
    (let [
       [f1 cyclic1 neighbors1] (get graph-structure src)
       [f2 cyclic2 neighbors2] (get graph-structure dst)
       equivalnce-mapper (partial map (partial -get-member-or-index equivalence-table))
       [cyclic equivalent-neigbors] (common-cycle [cyclic1 (equivalnce-mapper neighbors1)]
                                                  [cyclic2 (equivalnce-mapper neighbors2)])
       extended-equivalence-table (merge equivalence-table (into {} equivalent-neigbors) [src dst])
       [new-graph-structure returned-equivalence-table] (reduce [graph-structure extended-equivalence-table]
                                                                 merge-vertices
                                                                 equivalent-neigbors)
       returned-graph-structure (dissoc (-replace-with-equivalent new-graph-structure returned-equivalence-table) src)
       ]    
    [returned-graph-structure returned-equivalence-table])))
