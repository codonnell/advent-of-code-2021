(ns aoc2021.day02
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv #(str/split % #"\s"))
       (mapv (fn [[action value]] [action (parse-long value)]))))

(defn task1 [input]
  (->> input
       parse-input
       (reduce (fn [positions [action value]]
                 (case action
                   "forward" (update positions :horizontal + value)
                   "down" (update positions :depth + value)
                   "up" (update positions :depth - value)))
               {:horizontal 0 :depth 0})
       vals
       (reduce * 1))
  )

(defn task2 [input]
  (let [{:keys [horizontal depth]}
        (->> input
             parse-input
             (reduce (fn [{:keys [aim] :as positions} [action value]]
                       (case action
                         "forward" (-> positions
                                       (update :horizontal + value)
                                       (update :depth + (* aim value)))
                         "down" (update positions :aim + value)
                         "up" (update positions :aim - value)))
                     {:horizontal 0 :depth 0 :aim 0}))]
    (* horizontal depth))
  )

(comment
  (def input (slurp "inputs/day02-practice.txt"))
  (parse-input input)
  (task1 (slurp "inputs/day02-input.txt"))
  (task2 (slurp "inputs/day02-input.txt"))
  )
