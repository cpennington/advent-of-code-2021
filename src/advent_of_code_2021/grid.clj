(ns advent-of-code-2021.grid
   (:require [clojure.string :as str]))

(defn- bounds
  [grid]
  [(count grid)
   (count (first grid))])

(defn parse-input
  ([input]
   (parse-input input #(Character/digit % 10)))
  ([input parse-fn]
   (->> input
        str/split-lines
        (mapv (partial mapv parse-fn))
        ((fn [grid]
           {:bounds (bounds grid)
            :grid grid})))))

(defn in-bounds
  [{:keys [bounds]} [r c]]
  (let [[rows cols] bounds]
    (and (<= 0 r)
       (<= 0 c)
       (< r rows)
       (< c cols))))

(defn lookup
  ([grid [r c]]
   (lookup grid [r c] nil))
  ([grid [r c] default]
   (if (in-bounds grid [r c])
     (-> grid
         :grid
         (nth r)
         (nth c))
     default)))

(defn neighbors
  [grid [r c]]
  (filter #(in-bounds grid %)
          [[(dec r) c]
           [(inc r) c]
           [r (dec c)]
           [r (inc c)]]))

(defn dneighbors
  [grid [r c]]
  (filter #(in-bounds grid %)
           [[(dec r) (dec c)]
            [(dec r) c]
            [(dec r) (inc c)]
            [(inc r) (dec c)]
            [(inc r) c]
            [(inc r) (inc c)]
            [r (dec c)]
            [r (inc c)]]))

(defn lookup-neighbors
  [grid pt]
  (->> (neighbors grid pt)
       (map #(lookup grid %))))

(defn update-grid
  [grid [r c] f]
  (update-in grid [:grid r] #(update % c f)))
