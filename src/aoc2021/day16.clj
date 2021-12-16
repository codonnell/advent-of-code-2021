(ns aoc2021.day16
  (:require [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

(def debug false)

(defn type-id->type [type-id]
  (get {4 :literal} type-id :operator))

(def digit->binary
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(defn hex->binary-str [s]
  (apply str (map digit->binary s)))

(declare parse-packet)

(defn parse-header [{:keys [transmission]}]
  (let [type-id (Long/parseLong (subs transmission 3 6) 2)]
    {:version (Long/parseLong (subs transmission 0 3) 2)
     :type-id type-id
     :type (type-id->type type-id)
     :transmission (subs transmission 6)}))

(comment
  (def transmission "110100101111111000101000")
  (parse-header {:transmission "110100101111111000101000"})
  )

(defn parse-literal-byte [{:keys [transmission]}]
  {:value (Long/parseLong (subs transmission 1 5) 2)
   :continue (= \1 (first transmission))
   :transmission (subs transmission 5)})

(comment
  (parse-literal-byte {:transmission "101111111000101"})
  )

(defn parse-literal [{:keys [transmission]}]
  (let [[bs [last-b]] (->> {:transmission transmission}
                          (iterate parse-literal-byte)
                          (drop 1)
                          (split-with :continue))]
    {:value (->> (conj (vec bs) last-b)
              (mapv :value)
              (reduce (fn [v b] (+ (* v (* 2 2 2 2)) b)) 0))
     :transmission (:transmission last-b)}))

(comment
  (def transmission "101111111000101")
  (parse-literal {:transmission "101111111000101"})
  )

(defn parse-fixed-length-operator [{:keys [length transmission]}]
  (when debug (println {:length length :transmission transmission}))
  (let [[ps [last-p]] (->> {:transmission (subs transmission 0 length)}
                        (iterate parse-packet)
                        (drop 1)
                        (split-with (comp seq :transmission)))]
    {:subpackets (conj (vec ps) last-p)
     :transmission (subs transmission length)}))

(defn parse-fixed-subpackets-operator [{:keys [subpackets transmission]}]
  (when debug (println {:subpackets subpackets :transmission transmission}))
  (let [packets (->> {:transmission transmission}
                  (iterate parse-packet)
                  (drop 1)
                  (take subpackets)
                  vec)]
    {:subpackets packets
     :transmission (:transmission (peek packets))}))

(defn parse-operator [{:keys [transmission]}]
  (when debug (println "parse-operator" (subs transmission 0 16)))
  (case (first transmission)
    \0 (parse-fixed-length-operator {:transmission (subs transmission 16)
                                     :length (Long/parseLong (subs transmission 1 16) 2)})
    \1 (parse-fixed-subpackets-operator {:transmission (subs transmission 12)
                                         :subpackets (Long/parseLong (subs transmission 1 12) 2)})))

(comment
  ;; Fixed length
  (def transmission "00111000000000000110111101000101001010010001001000000000")
  (def length 27)
  (->> {:transmission (subs "1101000101001010010001001000000000" 0 length)}
    (iterate parse-packet)
    (drop 1)
    (drop-while (comp seq :transmission))
    (take 1))
  (parse-packet {:transmission "00111000000000000110111101000101001010010001001000000000"})

  ;; Fixed packets
  (def transmission "11101110000000001101010000001100100000100011000001100000")
  (parse-packet {:transmission "11101110000000001101010000001100100000100011000001100000"})
  )

(defn parse-packet [{:keys [transmission]}]
  (let [{body-transmission :transmission :as header}
        (parse-header {:transmission transmission})]
    (case (:type header)
      :literal (merge header (parse-literal {:transmission body-transmission}))
      :operator (merge header (parse-operator {:transmission body-transmission})))))

(comment
  (def transmission "110100101111111000101000")
  (parse-packet {:transmission "110100101111111000101000"})
  )

(defn parse-bits-transmission [input]
  (->> input
    hex->binary-str
    (hash-map :transmission)
    parse-packet))

(defn task1 [input]
  (->> input
    parse-bits-transmission
    (tree-seq :subpackets :subpackets)
    (map :version)
    (reduce + 0)))

(defn eval-packet [{:keys [type-id value subpackets] :as packet}]
  (let [values (map eval-packet subpackets)]
    (case type-id
      0 (reduce + 0 values)
      1 (reduce * 1 values)
      2 (apply min values)
      3 (apply max values)
      4 value
      5 (if (> (first values) (second values)) 1 0)
      6 (if (< (first values) (second values)) 1 0)
      7 (if (= (first values) (second values)) 1 0))))

(defn task2 [input]
  (->> input
    parse-bits-transmission
    eval-packet))

(comment
  (def input (slurp "inputs/day16-practice.txt"))
  (Long/parseLong "100" 2)
  (parse-bits-transmission "8A004A801A8002F478")
  (task1 "8A004A801A8002F478")
  (def transmission (hex->binary-str "620080001611562C8802118E34"))
  (parse-bits-transmission "620080001611562C8802118E34")
  (task1 "620080001611562C8802118E34")
  (parse-bits-transmission "C0015000016115A2E0802F182340")
  (task1 "C0015000016115A2E0802F182340")
  (parse-bits-transmission "A0016C880162017C3686B18A3D4780")
  (task1 "A0016C880162017C3686B18A3D4780")
  (task1 input)
  (task2 "C200B40A82")
  (task2 "04005AC33890")
  (task2 "880086C3E88112")
  (task2 "CE00C43D881120")
  (task2 "D8005AC2A8F0")
  (task2 "F600BC2D8F")
  (task2 "9C005AC2F8F0")
  (task2 "9C0141080250320F1802104A08")
  (task1 (slurp "inputs/day16-input.txt"))
  (task2 (slurp "inputs/day16-input.txt"))
  )
