(ns advent-of-code-2021.day9
  (:require
   [advent-of-code-2021.core :as core]
   [advent-of-code-2021.grid :as grid]
   [clojure.set :as set]))

(def sample (grid/parse-input (core/get-sample 9)))
(def input (grid/parse-input (core/get-input 9)))

(defn low-points
  [input]
  (for [r (-> input count range)
        c (-> input first count range)
        :let [v (grid/lookup input [r c])
              ns (grid/lookup-neighbors input [r c])]
        :when (every? #(< v %) ns)]
    [r c]))

(defn expand-basin
  [{:keys [input basin boundary] :or {basin #{}}}]
  (let [new-boundary (->> boundary
                          (map #(grid/neighbors input %))
                          (apply concat)
                          set
                          (#(set/difference % basin))
                          (filter #(->> %
                                        (grid/lookup input)
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
        (map #(grid/lookup input %))
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
  (grid/neighbors input [0 9])
  (apply set [])
  (set [[1 2] [2 3]])
  (expand-basin {:input sample :boundary #{[0 9]}})
  (expand-basin (expand-basin {:input sample :boundary #{[0 9]}}))
  (->> (iterate expand-basin {:input sample :boundary #{[0 9]}})
       (drop-while (comp seq :boundary))
       first))

