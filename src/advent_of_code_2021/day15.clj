(ns advent-of-code-2021.day15
  (:require
   [advent-of-code-2021.core :as core]
   [advent-of-code-2021.grid :as grid]
   [clojure.data.priority-map :refer [priority-map-keyfn]]
   [clojure.data.int-map :as i]))

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

(defn index
  [costs [r c]]
  (let [[_ cols] (:bounds costs)]
    (+ (* r cols) c)))

(defn path-cost*
  [costs frontier [tr tc] {:keys [actual]} [r c]]
  (let [new-actual (+ actual (grid/lookup costs [r c]))
        prior-cost (get frontier [r c])]
    [[r c]
     (if (or (nil? prior-cost)
             (< new-actual (:actual prior-cost)))
       {:actual new-actual
        :index (index costs [r c])
        :est (+ (- tr r) (- tc c) new-actual)}
       prior-cost)]))

(defn explore-next
  [{:keys [costs target frontier visited found] :as state}]
  (let [[pt pt-cost] (peek frontier)
        rest-frontier (when (< 0 (count frontier)) (pop frontier))
        neighbors (remove #(visited (index costs %)) (grid/neighbors costs pt))
        next-pts (map #(path-cost* costs frontier target pt-cost %) neighbors)
        next-frontier (into rest-frontier next-pts)]
    ;; (prn path end-pt)
    (assoc state
           :frontier next-frontier
           :visited (conj visited (:index pt-cost))
           :found (into found (filter (fn [[_ cost]] (= (:actual cost) (:est cost))) next-pts)))))

(defn setup
  [input]
  {:costs input
   :frontier (priority-map-keyfn :est [0 0] {:actual 0 :index 0})
   :visited (i/dense-int-set)
   :found []
   :target (mapv dec (:bounds input))})

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
        (iterate explore-next)
        (drop-while (comp empty? :found))
        first
        :found
        first
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

