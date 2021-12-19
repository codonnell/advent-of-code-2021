(ns aoc2021.day18
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.zip :as zip]))

(defn parse-input [input]
  (->> input
    str/split-lines
    (map edn/read-string)))

(defn explodes? [z]
  (and (vector? (zip/node z)) (> (count (zip/path z)) 3)))

(defn snailfish-left [z]
  (or (->> z
        (iterate zip/prev)
        (drop 1)
        (drop-while #(and % (vector? (zip/node %))))
        first)
    (zip/vector-zip (zip/root z))))

(defn snailfish-right [z]
  (->> z
    (iterate zip/next)
    (drop 1)
    (drop-while #(and (not (zip/end? %)) (vector? (zip/node %))))
    first))

(defn explode [z]
  (let [[left right] (zip/node z)
        before-right (-> z
                       (zip/replace 0)
                       snailfish-left
                       (zip/edit (fn [v] (if (vector? v) v (+ v left))))
                       snailfish-right ; Takes us to the zero
                       snailfish-right ; Takes us to the right value from the zero
                       )]
    (if (zip/end? before-right)
      before-right
      (zip/edit before-right (fn [v] (+ v right))))))

(comment
  (def input (-> [[[[[9,8],1],2],3],4] zip/vector-zip zip/down zip/down zip/down zip/down))
  (def input (->> [7 [6 [5 [4 [3 2]]]]] zip/vector-zip (iterate zip/next) (drop 8) first))
  (def input (->> [[6,[5,[4,[3,2]]]],1] zip/vector-zip (iterate zip/next) (drop 7) first))
  (def input (->> [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] zip/vector-zip (iterate zip/next) (drop 7) first))
  (def input (->> [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] zip/vector-zip (iterate zip/next) (drop 14) first))
  (zip/root (explode input))
  )

(defn splits? [z]
  (let [v (zip/node z)]
    (and (number? v) (>= v 10))))

(defn split [z]
  (zip/edit z (fn [v] [(quot v 2) (+ (quot v 2) (rem v 2))])))

(comment
  (-> [10] zip/vector-zip zip/next split)
  (-> [11] zip/vector-zip zip/next split)
  )

(defn reduce-explode [z]
  (->> z
    (iterate (fn [z]
               (if (explodes? z)
                 (-> z explode zip/root zip/vector-zip)
                 (zip/next z))))
    (drop-while #(not (zip/end? %)))
    first
    zip/root
    zip/vector-zip))

(defn reduce-split [z]
  (->> z
    (iterate (fn [z]
               (if (splits? z)
                 [(-> z split zip/root) :split]
                 (zip/next z))))
    (drop-while (fn [z]
                  (and (not= :split (second z))
                    (not (zip/end? z)))))
    first))

(defn snailfish-reduce [z]
  (->> z
    (iterate (fn [[v status]]
               (-> v zip/vector-zip reduce-explode reduce-split)))
    (drop-while #(not (zip/end? %)))
    first
    zip/root))

(defn snailfish-add [v1 v2]
  (-> [v1 v2]
    zip/vector-zip
    snailfish-reduce))

(comment
  (snailfish-add [[[[4,3],4],4],[7,[[8,4],9]]] [1,1])
  )

(defn magnitude [v]
  (walk/postwalk
    (fn [v]
      (if (vector? v)
        (+ (* 3 (first v)) (* 2 (second v)))
        v))
    v))

(defn task1 [input]
  (->> input
    parse-input
    (reduce snailfish-add)
    magnitude))

(defn task2 [input]
  (let [nums (parse-input input)
        pairs (for [n nums
                    m nums
                    :when (not= n m)]
                [n m])]
    (->> pairs
      (map (fn [[v1 v2]] (snailfish-add v1 v2)))
      (map magnitude)
      (apply max))))

(comment
  (def input (slurp "inputs/day18-practice5.txt"))
  (def v (parse-input input))
  (snailfish-add (first v) (second v))
  (magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])
  (task1 input)
  (parse-input input)
  (-> (zip/vector-zip [[1 2] [3 4]])
    zip/next
    zip/next
    zip/next
    zip/next
    zip/node)
  (task2 input)
  (task1 (slurp "inputs/day18-input.txt"))
  (task2 (slurp "inputs/day18-input.txt"))
  )
