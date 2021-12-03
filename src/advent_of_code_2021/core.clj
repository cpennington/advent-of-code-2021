(ns advent-of-code-2021.core
   (:require
    [clojure.edn :as edn]))

(defn get-input
  [day]
  (->> (str "resources/inputs/day" day ".txt")
       slurp))

(defn get-input-as-edn
  [day]
  (->> (get-input day)
       (format "[%s]")
       edn/read-string))