(ns advent-of-code-2021.day15
  (:require
   [advent-of-code-2021.core :as core]
   [advent-of-code-2021.grid :as grid]
   [clojure.data.priority-map :refer [priority-map-keyfn]]))

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

(defn path-cost*
  [costs path {:keys [actual]} [r c]]
  (let [{:keys [rows cols]} (grid/bounds costs)]
    [(conj path [r c])
     {:actual (+ actual (grid/lookup costs [r c]))
      :est (+ (- (dec rows) r) (- (dec cols) c))}]))

(defn keyfn*
  [cost]
  (apply + (vals cost)))

(defn explore-next
  [{:keys [costs frontier visited found]}]
  (let [[path path-cost] (peek frontier)
        rest-frontier (when (seq frontier) (pop frontier))
        end-pt (peek path)
        next-pts (remove visited (grid/neighbors costs end-pt))
        next-paths (mapv #(path-cost* costs path path-cost %) next-pts)
        next-frontier (into rest-frontier next-paths)
        ]
    ;; (prn path end-pt)
    {:costs costs
     :frontier next-frontier
     :visited (conj visited end-pt)
     :found (into found (filter (fn [[path cost]] (= 0 (:est cost))) next-paths))}
    ))

(defn setup
  [input]
  {:costs input :frontier (priority-map-keyfn keyfn* [[0 0]] {:actual 0}) :visited #{} :found []})

(defn riskier
  [costs n]
  (mapv (fn [row] (mapv #(inc (mod (+ n (dec %)) 9)) row)) costs))

(defn riskier-x
  [costs count]
  (let [dups (for [n (range count)] (riskier costs n))
        {:keys [rows]} (grid/bounds costs)]
    (into [] (for [r (range rows)]
               (->> dups
                    (mapv #(nth % r))
                    (apply concat)
                    (into []))))))

(defn riskier-y
  [costs count]
  (let [dups (for [n (range count)] (riskier costs n))]
    (into [] (apply concat dups))))

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
       (iterate explore-next)
       (take 200)
       (drop-while (comp empty? :found))
       first)
  (do-2 sample)
  (-> sample
       (riskier-x 5)
       (riskier-y 5)))
  
  ;11637517422274862853338597396444961841755517295286)
  ;11637517422274862853338597396444960840755507195186

