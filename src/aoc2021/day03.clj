(ns aoc2021.day03
  (:require [clojure.string :as str]
            [clojure.java.math :as math]))

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv vec)))

(defn transpose [colls]
  (apply mapv vector colls))

(defn zero-count [digits]
  (count (filter #{\0} digits)))

(defn common-digit [digits]
  (if (> (zero-count digits)
         (quot (count digits) 2))
    \0 \1))

(defn long-pow [a b]
  (reduce * 1 (repeat b a)))

(defn binary->long [digits]
  (Long/parseLong (apply str digits) 2)
  #_(->> digits
       (map-indexed (fn [idx digit]
                      (* digit (long-pow 2 idx))))
       (reduce + 0)))

(defn task1 [input]
  (let [common-digits (->> input
                           parse-input
                           transpose
                           (mapv common-digit))
        uncommon-digits (mapv {\0 \1 \1 \0} common-digits)]
    (* (binary->long common-digits) (binary->long uncommon-digits)))
  )

(defn rating [nums to-keep-fn]
  (loop [nums nums pos 0]
    (let [to-keep (to-keep-fn nums pos)
          rem-nums (filterv #(= to-keep (nth % pos)) nums)]
      (if (= 1 (count rem-nums))
        (first rem-nums)
        (recur rem-nums (rem (inc pos) (count (first nums))))))))

(defn o2 [nums]
  (rating nums (fn [nums pos]
                 (let [digits (mapv #(nth % pos) nums)]
                   (if (> (zero-count digits)
                          (quot (count digits) 2))
                     \0 \1)))))

(defn co2 [nums]
  (rating nums (fn [nums pos]
                 (let [digits (mapv #(nth % pos) nums)]
                   (if (<= (zero-count digits)
                           (quot (count digits) 2))
                     \0 \1)))))

(defn task2 [input]
  (let [parsed (parse-input input)]
    (* (binary->long (o2 parsed)) (binary->long (co2 parsed)))))

(comment
  (def input (slurp "inputs/day03-practice.txt"))
  (parse-input input)
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day03-input.txt"))
  (task2 (slurp "inputs/day03-input.txt"))

  (apply mapv vector (repeat 3 (range 5)))
  (vec "1234")
  (o2 (parse-input input))
  (co2 (parse-input input))

  )
