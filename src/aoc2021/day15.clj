(ns aoc2021.day15
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.set :as set]))

(defn compute-neighbors [risks [x y]]
  (into #{}
    (filter #(contains? risks %))
    [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))

(defn parse-input [input]
  (let [lines (vec (str/split-lines input))]
    {:risks (into {}
              (for [y (range (count lines))
                    x (range (count (first lines)))]
                [[x y] (-> lines (nth y) (subs x (inc x)) parse-long)]))
     :destination [(dec (count lines)) (dec (count (first lines)))]}))

(defn step [{:keys [risks visited q costs] :as env}]
  (let [[path cost :as v] (peek q)
        node (peek path)
        unvisited-neighbors (set/difference (compute-neighbors risks node) visited)]
    (cond-> env
      true (update :q pop)
      (not (visited node))
      (->
        (update :q into (map (juxt #(conj path %) #(+ cost (risks %)))) unvisited-neighbors)
        (update :visited conj node)
        (update :costs assoc node cost)))))

(defn task1 [input]
  (let [{:keys [risks destination]} (parse-input input)
        costs (->> {:risks risks :visited #{} :q (priority-map [[0 0]] 0) :costs {}}
                (iterate step)
                (drop-while #(not (contains? (:costs %) destination)))
                first
                :costs)]
    (get costs destination)))

(defn parse-tiled-input [input]
  (let [lines (vec (str/split-lines input))
        tile-height (count lines)
        tile-width (count (first lines))
        tile-costs (into [0] (take 40 (cycle (range 1 10))))]
    {:risks (into {}
              (for [y (range (* 5 tile-height))
                    x (range (* 5 tile-width))]
                (let [base-risk (-> lines
                                  (nth (rem y tile-height))
                                  (subs (rem x tile-width) (inc (rem x tile-width)))
                                  parse-long)]
                  [[x y] (nth tile-costs (+ base-risk (quot x tile-width) (quot y tile-height)))])))
     :destination [(dec (* 5 tile-width)) (dec (* 5 tile-width))]}))

(defn task2 [input]
  (let [{:keys [risks destination]} (parse-tiled-input input)
        costs (->> {:risks risks :visited #{} :q (priority-map [[0 0]] 0) :costs {}}
                (iterate step)
                (drop-while #(not (contains? (:costs %) destination)))
                first
                :costs)]
    (get costs destination)))

(comment
  (def input (slurp "inputs/day15-practice.txt"))
  (get-in (parse-tiled-input input) [:risks [2 13]])
  (def x 2)
  (def y 13)
  (def tile-height 10)
  (def tile-width 10)
  (def lines (vec (str/split-lines input)))
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day15-input.txt"))
  (task2 (slurp "inputs/day15-input.txt"))
  )
