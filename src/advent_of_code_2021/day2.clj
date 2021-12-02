(ns advent-of-code-2021.day2
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]))


(defn input->command
  [input-line]
  (let [[command value] (str/split input-line #" ")]
    [(keyword command) (Integer/parseInt value)]))

(defn input->commands
  [input]
  (let [lines (str/split-lines input)]
    (map input->command lines)))

(def input (input->commands (core/get-input 2)))

(defn process-command-1
  [[pos depth] [command value]]
  (case command
    :forward [(+ pos value) depth]
    :up [pos (- depth value)]
    :down [pos (+ depth value)]))

(defn process-command-2
  [[pos depth aim] [command value]]
  (case command
    :forward [(+ pos value) (+ depth (* aim value)) aim]
    :up [pos depth (- aim value)]
    :down [pos depth (+ aim value)]))

(defn do-1
  ([]
   (do-1 input))
  ([commands]
   (->> commands
       (reduce process-command-1 [0 0])
       (apply *))))

(defn do-2
  ([]
   (do-2 input))
  ([commands]
   (->> commands
        (reduce process-command-2 [0 0 0])
        (take 2)
        (apply *))))

(comment
  (->> [[:forward 5] [:down 5] [:forward 8] [:up 3] [:down 8] [:forward 2]] (reduce process-command-2 [0 0 0]))
  (do-1 [[:forward 4]])
  )