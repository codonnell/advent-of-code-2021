(ns aoc2021.day10
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.set :as set]))

(def open->close
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(defn first-illegal-char [line]
  (let [result (reduce (fn [stack c]
                         (cond
                           (contains? open->close c) (conj stack c)
                           (= c (open->close (peek stack))) (pop stack)
                           :illegal-char (reduced c)))
                 '() line)]
    (when (char? result)
      result)))

(def illegal-score
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn task1 [input]
  (->> input
    str/split-lines
    (keep first-illegal-char)
    (map illegal-score)
    (reduce + 0)))

(defn completion [line]
  (->> line
    (reduce (fn [stack c]
              (cond
                (contains? open->close c) (conj stack c)
                (= c (open->close (peek stack))) (pop stack)
                :illegal-char (throw (ex-info "Unexpected illegal char" {:stack stack :char c}))))
      '())
    (map open->close)))

(def legal-score
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn score-completion [cs]
  (reduce (fn [score c]
            (+ (* 5 score) (legal-score c)))
    0 cs))

(defn middle-score [scores]
  (let [n (count scores)]
    (nth (vec (sort scores)) (quot n 2))))

(defn task2 [input]
  (->> input
    str/split-lines
    (remove first-illegal-char)
    (map completion)
    (map score-completion)
    middle-score))

(comment
  (def input (slurp "inputs/day10-practice.txt"))
  (->> input str/split-lines (map first-illegal-char))
  (task1 input)
  (task2 input)
  (task1 (slurp "inputs/day10-input.txt"))
  (task2 (slurp "inputs/day10-input.txt"))
  )
