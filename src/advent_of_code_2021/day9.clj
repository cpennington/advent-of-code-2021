(ns advent-of-code-2021.day9
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]
   [clojure.set :as set]))

(defn parse-input
  [input]
  (->> input
      str/split-lines
      (mapv (partial mapv #(Character/digit % 10)))))

(def sample (parse-input (core/get-sample 9)))
(def input (parse-input (core/get-input 9)))

(defn in-bounds
  [input [r c]]
  (and (<= 0 r)
       (<= 0 c)
       (< r (count input))
       (< c (count (first input)))))

(defn lookup
  [input [r c]]
  (when (in-bounds input [r c])
    (-> input
        (nth r)
        (nth c))))


(defn neighbors
  [input [r c]]
  (filterv #(in-bounds input %)
           [[(dec r) c]
            [(inc r) c]
            [r (dec c)]
            [r (inc c)]]))

(defn lookup-neighbors
  [input pt]
  (->> (neighbors input pt)
       (map #(lookup input %))))

(defn low-points
  [input]
  (for [r (-> input count range)
        c (-> input first count range)
        :let [v (lookup input [r c])
              ns (lookup-neighbors input [r c])]
        :when (every? #(< v %) ns)]
    [r c]))

(defn expand-basin
  [{:keys [input basin boundary] :or {basin #{}}}]
  (let [new-boundary (->> boundary
                          (map #(neighbors input %))
                          (apply concat)
                          set
                          (#(set/difference % basin))
                          (filter #(->> %
                                        (lookup input)
                                        (> 9)))
                          set
                          )
        new-basin (set/union basin boundary)]
    {:input input
     :basin new-basin
     :boundary new-boundary}))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        low-points
        (map #(lookup input %))
        (map inc)
        (reduce +))))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (->> input
       low-points
       (map #(->> (iterate expand-basin {:input input :boundary #{%}})
                  (drop-while (comp seq :boundary))
                  first
                  :basin))
        (map count)
        sort
        (take-last 3)
        (reduce * 1)
        )))

(comment
  (neighbors input [0 9])
  (apply set [])
  (set [[1 2] [2 3]])
  (expand-basin {:input sample :boundary #{[0 9]}})
  (expand-basin (expand-basin {:input sample :boundary #{[0 9]}}))
  (->> (iterate expand-basin {:input sample :boundary #{[0 9]}})
       (drop-while (comp seq :boundary))
       first))
