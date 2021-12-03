(ns advent-of-code-2021.day3
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]))

(def input (str/split-lines (core/get-input 3)))

(defn bit-counts
  [lines]
  (apply (partial map (comp frequencies str)) lines))

(defn bits->int
  [bitvector]
  (->> bitvector
       (apply str)
       (#(Integer/parseInt % 2))))

(defn choose-bit
  [dir {zeroes \0 ones \1, :or {zeroes 0 ones 0}}]
  (if (= zeroes ones)
    (case dir
      :max \1
      :min \0)
    (case dir
      :max (if (> zeroes ones) \0 \1)
      :min (if (> zeroes ones) \1 \0))))

(defn choose-bits
  [dir bit-counts]
  (map (partial choose-bit dir) bit-counts))

(defn filter-by-index
  ([lines dir index]
   (let [selected-bit (->> lines
                           bit-counts
                           (choose-bits dir)
                           (#(nth % index)))
         next-lines (filter #(= selected-bit (nth % index)) lines)]
     (if (= 1 (count next-lines))
       (first next-lines)
       (filter-by-index next-lines dir (+ 1 index))))))

(defn do-1
  ([]
   (do-1 input))
  ([lines]
   (let [counts (bit-counts lines)
         gamma (->> counts
                    (choose-bits :max)
                    bits->int)
         eps (->> counts
                  (choose-bits :min)
                  bits->int)]
     (* gamma eps))))

(defn do-2
  ([]
   (do-2 input))
  ([lines]
   (let [oxygen-rating (Integer/parseInt (filter-by-index lines :max 0) 2)
         co2-rating (Integer/parseInt (filter-by-index lines :min 0) 2)]
     (* oxygen-rating co2-rating))))

(comment
  
(def sample '["00100"
              "11110"
              "10110"
              "10111"
              "10101"
              "01111"
              "00111"
              "11100"
              "10000"
              "11001"
              "00010"
              "01010"])
  (take 4 input)
  (filter-by-index sample :max 0)
  )