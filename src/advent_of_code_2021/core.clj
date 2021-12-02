(ns advent-of-code-2021.core
   (:require
    [clojure.edn :as edn]))

(defn get-input
  [day]
  (->> (str "resources/inputs/day" day ".txt")
      slurp
      (format "[%s]")
      edn/read-string))