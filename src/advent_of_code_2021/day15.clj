(ns advent-of-code-2021.day15
  (:require
   [advent-of-code-2021.core :as core]
   [advent-of-code-2021.grid :as grid]
   [advent-of-code-2021.search :as search]))

(def sample (grid/parse-input "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"))
(def input (grid/parse-input (core/get-input 15)))

(defn setup
  [input]
  (let [target (mapv dec (:bounds input))]
    (search/setup 
    {:initial-states [[0 0]]
     :est-fn (fn [next-state]
               (apply + (map - target next-state)))
     :neighbor-fn (fn [state] (map #(vector % (grid/lookup input %))
                                   (grid/neighbors input state)))})))

(defn riskier
  [costs n]
  (mapv (fn [row] (mapv #(inc (mod (+ n (dec %)) 9)) row)) costs))

(defn riskier-x
  [costs count]
  (let [dups (for [n (range count)] (riskier (:grid costs) n))
        [rows cols] (:bounds costs)]
    {:grid (into [] (for [r (range rows)]
                      (->> dups
                           (mapv #(nth % r))
                           (apply concat)
                           (into []))))
     :bounds [rows (* cols count)]}))

(defn riskier-y
  [costs count]
  (let [dups (for [n (range count)] (riskier (:grid costs) n))
        [rows cols] (:bounds costs)]
    {:grid (into [] (apply concat dups))
     :bounds [(* rows count) cols]}))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        setup
        search/search
        peek
        :actual)))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (-> input
       (riskier-x 5)
       (riskier-y 5)
       do-1)))

(comment
  sample
  (riskier sample 5)
  (grid/neighbors sample [0 0])
  (->> sample
       setup
       explore-next)
  (->> sample
       setup
       (iterate explore-next)
       (take 200)
       last)
  (do-1 sample)
  (-> sample
      (riskier-x 5)
      (riskier-y 5))
  (time (do-2 input))
  (require '[clj-async-profiler.core :as prof])
  (prof/profile (dotimes [i 10] (time (do-2 input))))
  (prof/serve-files 8080)
  )
  
  ;11637517422274862853338597396444961841755517295286)
  ;11637517422274862853338597396444960840755507195186

