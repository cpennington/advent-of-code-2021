(ns advent-of-code-2021.core)

(defn get-input
  [day name]
  (slurp (str "resources/inputs/day" day "/" name)))
