(ns aoc2021.day04
  (:require [clojure.string :as str]
            [clojure.java.math :as math]))

(defn transpose [colls]
  (apply mapv vector colls))

(defn parse-board [lines]
  (mapv (fn [line]
          (mapv parse-long (str/split (str/trim line) #"\s+")))
        lines))

(defn parse-input [input]
  (let [[[numbers] & raw-boards]
        (->> input
             str/split-lines
             (partition-by empty?)
             (remove #{[""]}))]
    {:numbers (mapv parse-long (str/split numbers #","))
     :boards (mapv parse-board raw-boards)}))

(defn winning-board? [marks board]
  (or (some #(every? marks %) board)
      (some #(every? marks %) (transpose board))))

(defn winning-board?' [{:keys [marks]} board]
  (let [marks (set marks)]
    (or (some #(every? marks %) board)
        (some #(every? marks %) (transpose board)))))

(defn step [{:keys [numbers boards winning-boards] :as state}]
  (let [updated-state (-> state
                          (update :numbers rest)
                          (update :marks conj (first numbers))
                          (update :winning-boards into (comp (remove winning-boards)
                                                             (filter (partial winning-board?' state))) boards))]
    (update updated-state
            :winning-boards into (comp (remove winning-boards)
                                       (filter (partial winning-board?' updated-state))) boards)))

(defn score [board marks]
  (->> board
       (apply concat)
       (remove (set marks))
       (reduce + 0)
       (* (peek marks))))

(defn task1 [input]
  (let [{:keys [numbers boards]} (parse-input input)]
    (loop [numbers numbers marks []]
      (if-let [winner (first (filter (partial winning-board? (set marks)) boards))]
        (->> winner
             (apply concat)
             (remove (set marks))
             (reduce + 0)
             (* (peek marks)))
        (recur (rest numbers) (conj marks (first numbers)))))))

(defn task1' [input]
  (let [{:keys [winning-boards marks]}
        (->> (assoc (parse-input input)
                    :marks []
                    :winning-boards #{})
             (iterate step)
             (drop-while (comp empty? :winning-boards))
             first)]
    (score (first winning-boards) marks)))

(defn task2 [input]
  (let [{:keys [numbers boards]} (parse-input input)]
    (loop [numbers numbers marks [] boards boards]
      (let [new-numbers (rest numbers)
            new-marks (conj marks (first numbers))
            new-boards (remove (partial winning-board? (set new-marks)) boards)]
        (if (empty? new-boards)
          (->> (first boards)
               (apply concat)
               (remove (set new-marks))
               (reduce + 0)
               (* (peek new-marks)))
          (recur new-numbers new-marks new-boards))))))

(defn task2' [input]
  (let [[{:keys [boards winning-boards]} {:keys [marks]}]
        (->> (assoc (parse-input input)
                    :marks []
                    :winning-boards #{})
             (iterate step)
             (partition 2 1)
             (drop-while (comp #(not= (:winning-boards %)
                                      (set (:boards %)))
                               second))
             first)
        last-winning-board (first (remove winning-boards boards))]
    (score last-winning-board marks)))

(comment
  (def input (slurp "inputs/day04-practice.txt"))
  (parse-input input)
  (task1 input)
  (task1' input)
  (task2 input)
  (task2' input)
  (task1 (slurp "inputs/day04-input.txt"))
  (task1' (slurp "inputs/day04-input.txt"))
  (task2 (slurp "inputs/day04-input.txt"))
  (task2' (slurp "inputs/day04-input.txt"))

  (remove #{[""]} (partition-by seq ["foo" "" "bar" "" "baz"]))

  )
