(ns advent-of-code-2021.day11
  (:require
   [advent-of-code-2021.core :as core]
   [advent-of-code-2021.grid :as grid]
   [clojure.set :as set]))

(def sample (grid/parse-input
             "5483143223
              2745854711
              5264556173
              6141336146
              6357385478
              4167524645
              2176841721
              6882881134
              4846848554
              5283751526"))
(def input  (grid/parse-input (core/get-input 11)))

(defn resolve-flashes
  [{:keys [input flashed]}]
  (let [new-flashes (->> (for [r (-> input :bounds first range)
                               c (-> input :bounds peek range)
                               :let [v (grid/lookup input [r c])]
                               :when (< 9 v)]
                           [r c])
                         (remove flashed)
                         set)
        inc-neighbors (->> new-flashes
                           (mapv (partial grid/dneighbors input))
                           (#(apply concat %)))
        new-input (reduce (fn [input pt] (grid/update-grid input pt inc))
                          input
                          inc-neighbors)]
    {:input new-input
     :flashed (set/union flashed new-flashes)
     :done (empty? new-flashes)}))

(defn inc-map
  [input]
  (update-in input [:input :grid] (partial mapv (partial mapv inc))))

(defn cap-map
  [input]
  (update-in input [:input :grid] (partial mapv (partial mapv #(if (> % 9) 0 %)))))

(defn step
  [input]
  (-> input
      inc-map
      (assoc :flashed #{})
      (->> (iterate resolve-flashes)
           (drop-while (comp not :done))
           first)
      (dissoc :done)
      cap-map))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> (iterate step {:input input
                       :flashed #{}})
        (drop 1)
        (take 100)
        (map (comp count :flashed))
        (reduce + 0))))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (->> (iterate step {:input input
                       :flashed #{}})
        (take-while #(< (-> % :flashed count)
                        (->> % :input :grid concat (apply concat) count)))
        count)))

(comment
  sample
  (step {:input (grid/parse-input "11111
19991
19191
19991
11111") :flashed #{}})
  (inc-map {:input sample})
  (do-1 sample)
  (->> (iterate step {:input sample
                      :flashed #{}})
       (drop 1)
       (take 10)
       (map (comp count :flashed))
       (reduce + 0))
  (->> (iterate step {:input sample
                      :flashed #{}})
       (take-while #(< (-> % :flashed count)
                       (->> % :input concat (apply concat) count))))
  (->> {:input sample} :input concat (apply concat) count)
  )


