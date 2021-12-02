(ns advent-of-code-2021.day1
  (:require
   [advent-of-code-2021.core :as core]))

(defn window
  [n coll]
  (partition n 1 coll))

(defn count-increasing
  [numbers]
  (->> numbers
       (window 2)
       (map (partial apply <))
       (filter identity)
       (count)))

(def input (core/get-input 1))

(defn do-1
  ([]
   (do-1 input))
  ([numbers]
   (count-increasing numbers)))

(defn do-2
  ([]
  (do-2 input))
  ([numbers]
   (->> numbers
        (window 3)
        (map (partial apply +))
        (map #(/ % 3))
        (count-increasing))))

(comment
  (prn (do-1))
  (do-2 [199
         200
         208
         210
         200
         207
         240
         269
         260
         263])
  (do-2)

  (->> [0 1 2]
       (window 2)
       (map (partial apply <))))

