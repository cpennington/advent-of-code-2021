(ns advent-of-code-2021.day6
  (:require
   [advent-of-code-2021.core :as core]))

(def sample [3,4,3,1,2])
(def input (core/get-input-as-edn 6))

(defn step-population
  [pop]
  (-> (->> pop
           (map #(update % 0 dec))
           (into {}))
       (dissoc -1)
      (update 6 #((fnil + 0) % (get pop 0 0)))
      (update 8 #((fnil + 0) % (get pop 0 0)))))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> (frequencies input)
       (iterate step-population)
       (drop 80)
       first
       vals
       (apply +))))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (->> (frequencies input)
        (iterate step-population)
        (drop 256)
        first
        vals
        (apply +))))

(comment
  (->> (frequencies sample)
       (iterate step-population)
       (drop 80)
       first
       vals
       (apply +))
  )

