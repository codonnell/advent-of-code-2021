(ns aoc2021.day08
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[inputs outputs] (str/split line #" \| ")]
    {:inputs (mapv set (str/split inputs #"\s"))
     :outputs (mapv set (str/split outputs #"\s"))}))

(defn parse-input [input]
  (mapv parse-line (str/split-lines input)))

(defn task1 [input]
  (->> input
    parse-input
    (mapcat :outputs)
    (filter (comp #{2 3 4 7} count))
    count))

(def digit->segments
  {0 #{\a \b \c \e \f \g}
   1 #{\c \f}
   2 #{\a \c \d \e \g}
   3 #{\a \c \d \f \g}
   4 #{\b \c \d \f}
   5 #{\a \b \d \f \g}
   6 #{\a \b \d \e \f \g}
   7 #{\a \c \f}
   8 #{\a \b \c \d \e \f \g}
   9 #{\a \b \c \d \f \g}
   })

(def segments->digit (set/map-invert digit->segments))

(defn decode [{:keys [inputs outputs]}]
  (let [freqs (->> inputs (apply concat) frequencies)
        {b 6 e 4 f 9} (set/map-invert freqs)
        a+c (->> freqs (filter (comp #{8} val)) (map key) set)
        d+g (->> freqs (filter (comp #{7} val)) (map key) set)
        c (->> inputs (filter (comp #{2} count)) first (set/intersection a+c) first)
        a (first (disj a+c c))
        d (->> inputs (filter (comp #{4} count)) first (set/intersection d+g) first)
        g (first (disj d+g d))
        segment-mapping {a \a b \b c \c d \d e \e f \f g \g}]
    (->> outputs
      (map #(into #{} (map segment-mapping) %))
      (map segments->digit)
      (apply str)
      parse-long)))

(defn task2 [input]
  (->> input
    parse-input
    (map decode)
    (reduce + 0)))

(comment
  (def input (slurp "inputs/day08-practice.txt"))
  (def line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
  (->> line parse-line decode)
  (parse-input input)
  (->> digit->segments vals (apply concat) frequencies)
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day08-input.txt"))
  (task2 (slurp "inputs/day08-input.txt"))
  )
