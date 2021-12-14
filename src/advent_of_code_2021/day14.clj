(ns advent-of-code-2021.day14
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]))

(defn parse-input
  [input]
  (let [[seed _ & insertions] (str/split-lines input)]
    {:seeds (->> seed (partition 2 1) (map (partial apply str)) frequencies),
     :insertions (->> insertions
                      (map #(str/split % #" -> "))
                      (map (fn [[seed ins]] [seed (first ins)]))
                      (into {}))
     :counts (frequencies seed)}))

(def sample (parse-input "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"))
(def input (parse-input (core/get-input 14)))

(defn expand-seed
  [insertions [seed count]]
  (let [insertion (get insertions seed)
        [fst snd] seed]
    {:seeds {(str fst insertion) count
             (str insertion snd) count}
     :counts {insertion count}}))

(defn expand-seeds
  [{:keys [insertions seeds counts]}]
  (let [{new-seeds :seeds new-counts :counts}
        (->> seeds
             (map #(expand-seed insertions %))
             (apply merge-with #(merge-with + %1 %2)))]
    {:insertions insertions
     :seeds new-seeds
     :counts (merge-with + counts new-counts)}))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        (iterate expand-seeds)
        (drop 10)
        first
        :counts
        vals
        ((fn [vs] (- (apply max vs) (apply min vs)))))))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (->> input
        (iterate expand-seeds)
        (drop 40)
        first
        :counts
        vals
        ((fn [vs] (- (apply max vs) (apply min vs)))))))

(comment
  sample
  (get (:seeds sample) "NN")
  (let [[fst snd] "NB"]
    [fst snd])
  (->> (:seeds sample)
       (map #(expand-seed (:insertions sample) %))
       (apply merge-with #(merge-with + %1 %2)))
  (->> sample
       expand-seeds)
  (->> sample
       (iterate expand-seeds)
       (drop 10)
       first
       :counts
       vals
       ((fn [vs] (- (apply max vs) (apply min vs)))))
  (frequencies "NNC")
  (first "N")
  )

