(ns advent-of-code-2021.day1
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]))

(defn count-increasing
  [numbers]
  (reduce (fn [[last count] next]
            [next (if (and (some? last ) (> next last)) (+ count 1) count)]) [nil 0] numbers))

(defn do-1
  []
  (let [input (core/get-input 1 "1.txt")
        numbers (->> (str/split-lines input)
                     (map #(Integer/parseInt %)))]
    (count-increasing numbers)))

(comment
  (prn (do-1)))

