(ns aoc2021.day12
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.set :as set]))

(defn parse-input [input]
  (->> input
    str/split-lines
    (map #(str/split % #"-"))
    (reduce (fn [g [a b]]
              (-> g
                (update a (fnil conj []) b)
                (update b (fnil conj []) a))) {})))

(defn small-cave? [cave]
  (= cave (str/lower-case cave)))

(defn valid-path? [path]
  (not (some (fn [[cave visits]]
               (and (> visits 1) (small-cave? cave)))
         (frequencies path))))

(defn grow-path [g path]
  (let [last-cave (peek path)]
    (map (fn [next-cave] (conj path next-cave))
      (get g last-cave))))

(defn finished? [path]
  (= "end" (peek path)))

(defn grow-paths [g paths]
  (into paths
    (comp
      (remove finished?)
      (mapcat (partial grow-path g))
      (filter valid-path?))
    paths))

(defn task1 [input]
  (let [g (parse-input input)]
    (->> #{["start"]}
      (iterate (partial grow-paths g))
      (partition 2 1)
      (drop-while (fn [[a b]] (not= a b)))
      ffirst
      (filter finished?)
      count)))

(defn valid-path2? [path]
  (let [path-freqs (frequencies path)
        small-cave-freqs (into {} (filter (comp small-cave? key)) path-freqs)]
    (and
      (= 1 (get path-freqs "start"))
      (< (count (filter (comp #(> % 1) val) small-cave-freqs)) 2)
      (<= (apply max (vals small-cave-freqs)) 2))))

(defn compute-next-paths [g paths]
  (into #{}
    (comp
      (mapcat (partial grow-path g))
      (filter valid-path2?))
    paths))

(defn step [{:keys [g unfinished-paths] :as env}]
  (let [next-paths (compute-next-paths g unfinished-paths)
        {unfinished false finished true} (group-by finished? next-paths)]
    (-> env
      (update :finished-paths into finished)
      (assoc :unfinished-paths unfinished))))

(defn task2 [input]
  (let [g (parse-input input)]
    (->> {:g g :unfinished-paths #{["start"]} :finished-paths #{}}
      (iterate step)
      ;; (drop 5)
      ;; first
      (take 100)
      (partition 2 1)
      (drop-while (fn [[a b]] (not= a b)))
      ffirst
      :finished-paths
      count
      )))

(comment
  (def input (slurp "inputs/day12-practice.txt"))
  (def input2 (slurp "inputs/day12-practice2.txt"))
  (def input3 (slurp "inputs/day12-practice3.txt"))
  (grow-paths (parse-input input) #{["start"]})
  (task1 input)
  (task1 input2)
  (task1 input3)
  (task2 input)
  (task2 input2)
  (task2 input3)
  (task1 (slurp "inputs/day12-input.txt"))
  (task2 (slurp "inputs/day12-input.txt"))
  )
