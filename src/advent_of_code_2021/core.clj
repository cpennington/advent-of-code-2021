(ns advent-of-code-2021.core
   (:require
    [clojure.string :as str]))

(defn get-input
  [day]
  (slurp (str "resources/inputs/day" day ".txt")))

(defn input->numbers
  [input]
  (->> input
       (str/split-lines)
       (map #(Integer/parseInt %))))