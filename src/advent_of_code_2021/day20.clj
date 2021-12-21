(ns advent-of-code-2021.day20
  (:require
   [advent-of-code-2021.core :as core]
   [advent-of-code-2021.grid :as grid]
   [clojure.string :as str]))

(def bits {\. 0
           \# 1})

(defn parse-input
  [input]
  (let [[key grid-str] (-> input
                           (str/split #"\n\n"))
        grid (grid/parse-input grid-str bits)]
    {:key (map bits key)
     :grid grid
     :default 0}))

(def sample (parse-input (core/get-sample 20)))
(def input (parse-input (core/get-input 20)))

(defn neighborhood
  [{:keys [grid default]} [r c]]
  (mapv #(grid/lookup grid % default)
        [[(dec r) (dec c)] [(dec r) c] [(dec r) (inc c)]
         [r       (dec c)] [r       c] [r       (inc c)]
         [(inc r) (dec c)] [(inc r) c] [(inc r) (inc c)]]))

(defn index
  [vals]
  (reduce + (map * vals [256 128 64 32 16 8 4 2 1])))

(defn expand-pt
  [{:keys [key] :as state} [r c]]
  (let [ns (neighborhood state [r c])
        ix (index ns)
        new-val (nth key ix)]
    new-val))

(defn expand
  [{:keys [grid] :as state}]
  (let [[br bc] (:bounds grid)
        nbr (+ br 2)
        nbc (+ bc 2)
        new-grid (->>
                  (for [r (range -1 (inc br))
                        c (range -1 (inc bc))]
                    (expand-pt state [r c]))
                  (partition nbc)
                  (mapv #(into [] %))
                  (#(hash-map :grid % :bounds [nbr nbc])))
        new-default (expand-pt state [-2 -2])]
    (-> state
        (assoc :grid new-grid)
        (assoc :default new-default))))

(defn do-1
  ([]
   (do-1 input))
  ([input]
  (->> input
       (iterate expand)
       (drop 2)
       first
       :grid
       :grid
       (apply concat)
       (reduce +)
       )))

(defn do-2
  ([]
   (do-2 input))
  ([input]
   (->> input
        (iterate expand)
        (drop 50)
        first
        :grid
        :grid
        (apply concat)
        (reduce +))))

(comment
  sample
  
  (do-2 input)
  (range -1 5)
  (->> sample
       parse-input
       expand
       expand
       :grid
       :grid
       (apply concat)
       (reduce +))
  )

