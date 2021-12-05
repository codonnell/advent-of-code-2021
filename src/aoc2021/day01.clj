(ns aoc2021.day01
  (:require [clojure.string :as str]))

(defn task1 [input]
  (->> input
       str/split-lines
       (mapv parse-long)
       (partition 2 1)
       (filter (fn [[left right]] (< left right)))
       (count))
  )

(defn task2 [input]
  (->> input
       str/split-lines
       (mapv parse-long)
       (partition 3 1)
       (partition 2 1)
       (filter (fn [[left right]] (< (reduce + 0 left) (reduce + 0 right))))
       (count))
  )

(comment
  (def input (slurp "inputs/day01-practice.txt"))
  (task1 (slurp "inputs/day01-input.txt"))
  (task2 (slurp "inputs/day01-input.txt"))
  )
