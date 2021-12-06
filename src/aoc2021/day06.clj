(ns aoc2021.day06
  (:require [clojure.string :as str]
            [clojure.java.math :as math]))

(defn parse-input [input]
  (mapv parse-long (str/split (str/trim input) #",")))

(defn step [timer-freqs]
  (->> timer-freqs
    (map (fn [[timer population]]
           (if (zero? timer)
             {6 population 8 population}
             {(dec timer) population})))
    (apply merge-with +')))

(defn task1 [input]
  (->> input
    parse-input
    frequencies
    (iterate step)
    (drop 80)
    first
    (map val)
    (reduce + 0)))

(defn task2 [input]
  (->> input
    parse-input
    frequencies
    (iterate step)
    (drop 256)
    first
    (map val)
    (reduce + 0)))

(comment
  (def input (slurp "inputs/day06-practice.txt"))
  (->> input
    parse-input
    frequencies
    (iterate step)
    (drop 80)
    first
    (map val)
    (reduce + 0))
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day06-input.txt"))
  (task2 (slurp "inputs/day06-input.txt"))
  )
