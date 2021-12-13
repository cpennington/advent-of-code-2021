(ns advent-of-code-2021.day12
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map str/trim)
       (map #(str/split % #"-"))
       (reduce (fn [graph [a b]] (-> (merge {a [] b []} graph)
                                     (update a #(conj % b))
                                     (update b #(conj % a)))) {})
       (#(assoc % "end" []))))

(defn small-cave?
  [cave]
  (= cave (str/lower-case cave)))

(defn extend-path
  [graph {:keys [path visited] :or {visited #{}}}]
  (let [next-caves (get graph (last path))]
    (if (seq next-caves)
      (->> next-caves
           (remove visited)
           (map (fn [next]
                  {:path (conj path next)
                   :visited (if (small-cave? next)
                              (conj visited next)
                              visited)})))
      [{:path path
        :visited visited}])))

(defn path-complete?
  [{:keys [path]}]
  (-> path last (= "end")))

(defn extend-paths
  [{:keys [graph paths complete-paths] :or {paths [{:path ["start"] :visited #{"start"}}]}}]
  (let [new-paths (->> paths
                       (mapv (partial extend-path graph))
                       (apply concat)
                       (into []))]
    {:graph graph
     :paths (into [] (remove path-complete? new-paths))
     :complete-paths (into complete-paths (filter path-complete? new-paths))}))

(def tiny-sample (parse-input "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))
(def med-sample (parse-input "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"))
(def sample (parse-input "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"))
(def input (parse-input (core/get-input 12)))

(defn all-paths
  [graph]
  (->> {:graph graph}
       (iterate extend-paths)
       (drop 1)
       (take 100)
       (drop-while #(->> %
                         :paths
                         seq))
       first
       :complete-paths
       (mapv :path)
       (mapv #(str/join "," %))
       sort))

(defn duplicate-cave
  [graph cave]
  (let [cave' (str cave \')
        connections (get graph cave)
        incoming (into {} (->> connections
                               (remove #{"end"})
                               (map #(vector % cave'))))]
    (merge-with conj
                (assoc graph cave' connections)
                incoming)))

(defn double-small-caves
  [graph]
  (->> graph
       keys
       (remove #{"start" "end"})
       (remove  (comp not small-cave?))
       (map (partial duplicate-cave graph))))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (-> input
       all-paths
       count)))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (->> input
        double-small-caves
        (map all-paths)
        (apply concat)
        (map #(str/replace % "'" ""))
        set
        count)))

(comment
  (-> sample
      all-paths)
  (->> {:graph sample}
       (iterate extend-paths)
       (drop 1)
       (take 100)
       (drop-while #(->> %
                         :paths
                         (not-every? (comp (partial = "end") last :path))))
       (map :paths)
       first
       (map :path)
       count)
  (small-cave? "a'")
  (duplicate-cave sample "b")
  (keys sample)
  (count  (double-small-caves sample))  (->> sample
                                             keys
                                             (remove #{"start" "end"})
                                             (remove  (comp not small-cave?))
                                             (map #(assoc sample (str % \') (get sample %))))
  (->> input
       double-small-caves
       (map all-paths)
      ;;  (apply concat)
      ;;  (map #(str/replace % "'" ""))
      ;;  set
      ;;  count
       )

  (all-paths {:graph med-sample})
  (-> {:graph med-sample}
      extend-paths
      extend-paths
      extend-paths
      extend-paths
      extend-paths
      extend-paths
      :complete-paths)
  (do-2 med-sample)
  (path-complete? {:path ["start" "dc"], :visited #{"dc" "start"}})
  )

