(ns advent-of-code-2021.core
   (:require
    [clojure.edn :as edn]))

(defn get-input
  [day]
  (->> (str "resources/inputs/day" day ".txt")
       slurp))

(defn string->edn
  [str]
  (->> str
       (format "[%s]")
       edn/read-string))

(defn get-input-as-edn
  [day]
  (string->edn (get-input day)))