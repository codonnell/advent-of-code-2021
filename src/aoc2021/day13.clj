(ns aoc2021.day13
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.set :as set]))

(defn parse-input [input]
  (let [[points [_ & instructions]]
        (->> input
          str/split-lines
          (split-with (complement str/blank?)))]
    {:points (->> points
               (map #(str/split % #","))
               (into #{} (map #(mapv parse-long %))))
     :instructions (map (fn [instruction]
                          (let [[_ x-or-y n] (re-matches #"fold along (x|y)=(\d+)" instruction)]
                            [x-or-y (parse-long n)]))
                     instructions)}))

(defn fold-x [[x y] n]
  (if (> x n)
    [(- (* 2 n) x) y]
    [x y]))

(defn fold-y [[x y] n]
  (if (> y n)
    [x (- (* 2 n) y)]
    [x y]))

(defn task1 [input]
  (let [{:keys [points] [[x-or-y n]] :instructions} (parse-input input)]
    (->> points
      (into #{} (map #(case x-or-y
                        "x" (fold-x % n)
                        "y" (fold-y % n))))
      count)))

(defn draw-points [points]
  (let [[max-x max-y] (reduce (fn [[x y] [x' y']]
                                [(max x x') (max y y')])
                        [0 0] points)]
    (doseq [y (range 0 (inc max-y))]
      (doseq [x (range 0 (inc max-x))]
        (if (contains? points [x y])
          (print "#")
          (print ".")))
      (print "\n"))))

(defn task2 [input]
  (let [{:keys [points instructions]} (parse-input input)]
    (->> instructions
      (reduce (fn [points [x-or-y n]]
                (into #{}
                  (map #(case x-or-y
                          "x" (fold-x % n)
                          "y" (fold-y % n)))
                  points))
        points)
      draw-points)))

(comment
  (def input (slurp "inputs/day13-practice.txt"))
  (parse-input input)
  (def s "fold along y=7" )
  (re-matches #"fold along (x|y)=(\d+)" s)
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day13-input.txt"))
  (task2 (slurp "inputs/day13-input.txt"))
  )
