(ns aoc2021.day05
  (:require [clojure.string :as str]
            [clojure.java.math :as math]))

(defn parse-point [s]
  (mapv parse-long (str/split s #",")))

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv #(str/split % #" -> "))
       (mapv #(mapv parse-point %))))

(defn intersected-points [[[x1 y1] [x2 y2]]]
  (cond
    (and (= x1 x2) (= y1 y2))
    [[x1 y1]]

    (or (= x1 x2) (= y1 y2))
    (for [x (range (min x1 x2) (inc (max x1 x2)))
          y (range (min y1 y2) (inc (max y1 y2)))]
      [x y])

    :diagonal
    (map (fn [n]
           [((if (> x2 x1) + -) x1 n)
            ((if (> y2 y1) + -) y1 n)])
         (range (inc (math/abs (- x1 x2)))))))

(defn task1 [input]
  (->> input
       parse-input
       (filter (fn [[[x1 y1] [x2 y2]]]
                 (or (= x1 x2) (= y1 y2))))
       (mapcat intersected-points)
       (reduce (fn [points point]
                 (update points point (fnil inc 0)))
               {})
       (filter (fn [[_ n]] (> n 1)))
       count))

(defn task2 [input]
  (->> input
       parse-input
       (mapcat intersected-points)
       (reduce (fn [points point]
                 (update points point (fnil inc 0)))
               {})
       (filter (fn [[_ n]] (> n 1)))
       count))

(comment
  (def input (slurp "inputs/day05-practice.txt"))
  (parse-input input)
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day05-input.txt"))
  (task2 (slurp "inputs/day05-input.txt"))

  (str/split "0,9 -> 5,9" #" -> ")
  (range 5 6)
  (intersected-points [[1 1] [1 3]])
  (intersected-points [[1 1] [3 3]])
  (math/abs (- 5 2))
  )
