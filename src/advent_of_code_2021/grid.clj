(ns advent-of-code-2021.grid
   (:require [clojure.string :as str]))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (mapv str/trim)
       (mapv (partial mapv #(Character/digit % 10)))))

(defn in-bounds
  [input [r c]]
  (and (<= 0 r)
       (<= 0 c)
       (< r (count input))
       (< c (count (first input)))))

(defn lookup
  [input [r c]]
  (when (in-bounds input [r c])
    (-> input
        (nth r)
        (nth c))))

(defn neighbors
  [input [r c]]
  (filterv #(in-bounds input %)
           [[(dec r) c]
            [(inc r) c]
            [r (dec c)]
            [r (inc c)]]))

(defn dneighbors
  [input [r c]]
  (filterv #(in-bounds input %)
           [[(dec r) (dec c)]
            [(dec r) c]
            [(dec r) (inc c)]
            [(inc r) (dec c)]
            [(inc r) c]
            [(inc r) (inc c)]
            [r (dec c)]
            [r (inc c)]]))

(defn lookup-neighbors
  [input pt]
  (->> (neighbors input pt)
       (map #(lookup input %))))

(defn update-grid
  [input [r c] f]
  (update input r #(update % c f)))
