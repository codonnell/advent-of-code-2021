(ns aoc2021.day14
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.set :as set]))

(defn parse-input [input]
  (let [[[template] [_ & rules]]
        (->> input
          str/split-lines
          (split-with (complement str/blank?)))]
    {:template (seq template)
     :rules (->> rules
              (map #(str/split % #" -> "))
              (map (fn [[i o]] [(seq i) (first o)]))
              (into {}))}))

(defn step [{:keys [rules template] :as env}]
  (let [insertions (->> template
                     (partition 2 1)
                     (map rules))]
    (assoc env :template
      (cons (first template) (interleave insertions (rest template))))))

(defn task1 [input]
  (let [element-freqs (->> input
                        parse-input
                        (iterate step)
                        (drop 10)
                        first
                        :template
                        frequencies)
        max-element-freq (apply max (vals element-freqs))
        min-element-freq (apply min (vals element-freqs))]
    (- max-element-freq min-element-freq)))

(defn step-pairs [{:keys [rules pair-freqs] :as env}]
  (assoc env :pair-freqs
    (reduce (fn [pair-freqs* [[a b :as pair] n]]
              (let [middle (get rules pair)]
                (-> pair-freqs*
                  (update [a middle] (fnil + 0) n)
                  (update [middle b] (fnil + 0) n))))
      {} pair-freqs)))

(defn pair-freqs->element-freqs [pair-freqs]
  (update-vals
    (reduce (fn [element-freqs [[a b] n]]
              (-> element-freqs
                (update a (fnil + 0) n)
                (update b (fnil + 0) n)))
      {}
      pair-freqs)
    #(+ (quot % 2) (rem % 2))))

(defn task2 [input]
  (let [{:keys [rules template]} (parse-input input)
        element-freqs (->> {:rules rules
                            :pair-freqs (->> template
                                          (partition 2 1)
                                          frequencies)}
                        (iterate step-pairs)
                        (drop 40)
                        first
                        :pair-freqs
                        pair-freqs->element-freqs)
        max-element-freq (apply max (vals element-freqs))
        min-element-freq (apply min (vals element-freqs))]
    (- max-element-freq min-element-freq)))

(comment
  (def input (slurp "inputs/day14-practice.txt"))
  (parse-input input)
  (->> input
    parse-input
    (iterate step)
    (take 2))
  (seq "AB")
  (first "A")
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day14-input.txt"))
  (task2 (slurp "inputs/day14-input.txt"))
  )
