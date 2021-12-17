(ns aoc2021.day17
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

(defn parse-input [input]
  (->> input
    str/trim
    (re-matches #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)")
    rest
    (map parse-long)
    (zipmap [:min-x :x-max :min-y :max-y])))

(defn step-path [{:keys [velocity location] :as env}]
  (-> env
    (update :location (partial mapv +) velocity)
    (update :velocity (fn [[dx dy]]
                        [(cond (pos? dx) (dec dx)
                               (neg? dx) (inc dx)
                               :else dx)
                         (dec dy)]))))

;; For min-dx, want minimum dx that gets us to min-x
;; x + (x-1) + ... + 1  >= min-x
;; (x-1) + (x-2) + ... + 1  < min-x
;; For min-dy, want minimum y that gets us to min-y
;; ((y - (abs-min-dx - 1)) + y) * abs-min-dx / 2
;; (2y - abs-min-dx + 1) * abs-min-dx / 2
;; For max-dy... the y coord will always come back down to the starting point
;; So dy cannot be larger than the y-distance from the starting point to the max absolute value of min-y and max-y

(defn compute-min-dx [min-x]
  (let [abs-min-x (math/abs min-x)]
    (cond-> (reduce (fn [farthest-x dx]
                      (if (>= (+ farthest-x dx) abs-min-x)
                        (reduced dx)
                        (+ farthest-x dx)))
              0 (range 1 (inc abs-min-x)))
      (neg? min-x) -)))

(defn compute-min-dy [min-y min-dx]
  (if (neg? min-y)
    min-y
    (let [abs-min-dx (math/abs min-dx)]
      (long (* (- (inc (* 2 min-y)) abs-min-dx) abs-min-dx 1/2)))))

(defn compute-max-dy [min-y max-y]
  (max (math/abs min-y) (math/abs max-y)))

(defn within-bounds? [{:keys [min-x x-max min-y max-y]} [x y]]
  (and (<= min-x x x-max) (<= min-y y max-y)))

(defn paths-within-bounds [{:keys [min-x x-max min-y max-y] :as bounds}]
  (let [min-dx (compute-min-dx min-x)
        max-dx x-max
        min-dy (compute-min-dy min-y min-dx)
        max-dy (compute-max-dy min-y max-y)]
    (->> (for [dx (range min-dx (inc max-dx))
               dy (range min-dy (inc max-dy))]
           (->> {:velocity [dx dy] :location [0 0]}
             (iterate step-path)
             (take-while #(-> % :location second (>= min-y)))))
      (filter (fn [steps]
                (some #(within-bounds? bounds %) (map :location steps)))))))

(defn task1 [input]
  (->> input
    parse-input
    paths-within-bounds
    (mapcat (fn [steps]
              (map (comp second :location) steps)))
    (apply max)))

(defn task2 [input]
  (->> input
    parse-input
    paths-within-bounds
    count))

(comment
  (def input (slurp "inputs/day17-practice.txt"))
  (parse-input input)
  (compute-min-dx 4)
  (math/signum 1)
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day17-input.txt"))
  (task2 (slurp "inputs/day17-input.txt"))
  )
