(ns aoc2021.day07
  (:require [clojure.string :as str]
            [clojure.java.math :as math]))

(defn parse-input [input]
  (mapv parse-long (str/split (str/trim input) #",")))

(defn fuel-cost [positions destination]
  (reduce + 0 (map #(math/abs (- % destination)) positions)))

(defn task1 [input]
  (let [positions (parse-input input)]
    (apply min (map (partial fuel-cost positions)
                 (range (apply min positions) (inc (apply max positions)))))))

(defn fuel-cost2 [positions destination]
  (->> positions
    (map #(long (* (math/abs (- % destination)) (inc (math/abs (- % destination))) 1/2)))
    (reduce + 0)))

(defn task2 [input]
  (let [positions (parse-input input)]
    (apply min (map (partial fuel-cost2 positions)
                 (range (apply min positions) (inc (apply max positions)))))))

(comment
  (def input (slurp "inputs/day07-practice.txt"))
  (def positions (parse-input input))
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day07-input.txt"))
  (task2 (slurp "inputs/day07-input.txt"))
  )
