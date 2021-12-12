(ns aoc2021.day09
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.set :as set]))

(defn parse-input [input]
  (let [lines (vec (str/split-lines input))]
    (into {}
      (for [y (range (count lines))
            x (range (count (first lines)))]
        [[x y] (-> lines (nth y) (subs x (inc x)) parse-long)]))))

(defn low-point? [heights [x y]]
  (let [h (get heights [x y])]
    (and
      ;; 10 is greater than the tallest height in the grid, 9
      (< h (get heights [(inc x) y] 10))
      (< h (get heights [(dec x) y] 10))
      (< h (get heights [x (inc y)] 10))
      (< h (get heights [x (dec y)] 10)))))

(defn task1 [input]
  (let [heights (parse-input input)]
    (->> heights
      keys
      (filter (partial low-point? heights))
      (map (fn [point] (inc (get heights point))))
      (reduce + 0))))

(defn neighbors [[x y]]
  [[x y] [(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn grow-basin [heights basin]
  (into #{}
    (comp (mapcat neighbors)
      (distinct)
      (remove (comp #(contains? #{9 nil} %) heights)))
    basin))

(defn step [{:keys [heights] :as env}]
  (update env :basins update-vals (partial grow-basin heights)))

(defn task2 [input]
  (let [heights (parse-input input)
        basins (->> heights keys (filter (partial low-point? heights)) (into {} (map (juxt identity hash-set))))]
    (->> {:heights heights :basins basins}
      (iterate step)
      (partition 2 1)
      (drop-while (fn [[a b]] (not= a b)))
      ffirst
      :basins
      vals
      (map count)
      (sort (comp - compare))
      (take 3)
      (reduce * 1))))

(comment
  (def input (slurp "inputs/day09-practice.txt"))
  (get (parse-input input) [0 1])
  (sort (comp - compare) (range 10))
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day09-input.txt"))
  (task2 (slurp "inputs/day09-input.txt"))
  )
