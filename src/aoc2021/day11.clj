(ns aoc2021.day11
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.set :as set]))

(defn parse-input [input]
  (let [lines (vec (str/split-lines input))]
    (into {}
      (for [y (range (count lines))
            x (range (count (first lines)))]
        [[x y] (-> lines (nth y) (subs x (inc x)) parse-long)]))))

(defn inc-energy [n]
  (if (= :flash n)
    :flash (inc n)))

(defn neighbors [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= [0 0] [dx dy])]
    [(+ x dx) (+ y dy)]))

(defn flash-step [energies]
  (let [flashes (->> energies
                  (filter (fn [[_ e]]
                            (and (not= :flash e)
                              (> e 9))))
                  (map key))
        flash-neighbors (mapcat neighbors flashes)]
    (reduce (fn [energies* flash-neighbor]
              (if (contains? energies* flash-neighbor)
                (update energies* flash-neighbor inc-energy)
                energies*))
      (merge energies (zipmap flashes (repeat :flash)))
      flash-neighbors)))

(defn step [{:keys [flashes energies]}]
  (let [new-energies (update-vals energies inc)
        stepped-energies (->> new-energies
                           (iterate flash-step)
                           (partition 2 1)
                           (drop-while (fn [[a b]] (not= a b)))
                           ffirst)]
    {:energies (update-vals stepped-energies #(if (= :flash %) 0 %))
     :flashes (+ flashes (->> stepped-energies vals (filter #{:flash}) count))}))

(defn task1 [input]
  (->> input
    parse-input
    (hash-map :flashes 0 :energies)
    (iterate step)
    (drop 100)
    first
    :flashes))

(defn synchronized? [energies]
  (every? zero? (vals energies)))

(defn task2 [input]
  (->> input
    parse-input
    (hash-map :flashes 0 :energies)
    (iterate step)
    (map-indexed vector)
    (drop-while (fn [[_ {:keys [energies]}]]
                   (not (synchronized? energies))))
    ffirst))

(comment
  (def input (slurp "inputs/day11-practice.txt"))
  (def input2 (slurp "inputs/day11-practice2.txt"))
  (def energies (parse-input input2))
  (step (step {:energies energies :flashes 0}))
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day11-input.txt"))
  (task2 (slurp "inputs/day11-input.txt"))
  )
