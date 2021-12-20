(ns advent-of-code-2021.day19
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]
   [clojure.set :as set]))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (remove #{""})
       (partition-by #(str/starts-with? % "---"))
       (partition 2)
       (map (fn [[name positions]]
              [(-> name first (str/replace #"\s?---\s?" "") (str/replace " " "-") keyword)
               (map core/string->edn positions)]))
       (into {})))

(def sample (parse-input (core/get-sample 19)))
(def input (parse-input (core/get-input 19)))

(defn vec-between
  [p1 p2]
  (mapv - p1 p2))

(defn vec-add
  [p v]
  (mapv + p v))

(defn dist-squared
  [p1 p2]
  (apply + (map #(* % %) (vec-between p1 p2))))

(defn scanner-dists
  [pts]
  (apply merge-with into (let [ixs (range (count pts))]
                           (for [[ia a] (map vector ixs pts)
                                 [ib b] (map vector ixs pts)
                                 :when (not= ia ib)]
                             {(dist-squared a b) [#{a b}]}))))

(defn rot-x
  [[x y z]]
  [x (- z) y])

(defn rot-y
  [[x y z]]
  [(- z) y x])

(defn rot-z
  [[x y z]]
  [(- y) x z])

(def rotations
  (for [x-rots (range 4)
        y-rots (range 4)
        z-rots (range 4)]
    (comp (apply comp (repeat x-rots rot-x))
          (apply comp (repeat y-rots rot-y))
          (apply comp (repeat z-rots rot-z)))))

(defn get-all
  [m ks]
  (mapv #(get m %) ks))

(defn fix-orientation-and-offset
  [origin-dists to-fix-dists to-fix-pts matching-dists]
  (letfn [(matching-points
           [dists]
           (->> (get-all dists matching-dists)
                (apply concat)
                (apply concat)
                frequencies
                (map first)
                sort))]
    (let [origin-matching-pts (matching-points origin-dists)
          to-fix-matching-pts (matching-points to-fix-dists)]
      (when (and (seq origin-matching-pts) (seq to-fix-matching-pts))
        (letfn [(comparison-vecs
                  [pts]
                  (->> pts
                       sort
                       (partition 2)
                       (map #(apply vec-between %))))]
          (let [origin-comp-vecs (comparison-vecs origin-matching-pts)
                valid-rotations
                (for [rot rotations
                      :let [rotated-matching-pts (sort (map rot to-fix-matching-pts))
                            to-fix-comp-vecs (comparison-vecs rotated-matching-pts)]
                      :when (= origin-comp-vecs to-fix-comp-vecs)]
                  [rot (mapv - (first origin-matching-pts) (first rotated-matching-pts))])]
            (when (seq valid-rotations)
              (let [[rot offset] (first valid-rotations)]
                (mapv #(mapv + offset (rot %)) to-fix-pts)))))))))

(defn merge-scanners
  [{:keys [re-oriented scanners dists] :or {re-oriented [] dists {}} :as state}]
  (let [[next-scanner next-pts next-dists] (first scanners)
        rest-scanners (into [] (rest scanners))
        next-dists (scanner-dists next-pts)]
    (if (empty? re-oriented)
      (-> state
          (assoc :scanners rest-scanners)
          (assoc :re-oriented [[next-scanner next-pts]])
          (assoc :dists next-dists))
      (let [matching-dists (set/intersection (set (keys dists)) (set (keys next-dists)))]
        (let [fixed-pts (fix-orientation-and-offset dists next-dists next-pts matching-dists)]
          (if (seq fixed-pts)
            (-> state
                (assoc :scanners rest-scanners)
                (update :re-oriented #(conj % [next-scanner fixed-pts]))
                (update :dists #(merge-with into % (scanner-dists fixed-pts))))
            (-> state
                (assoc :scanners (conj rest-scanners (first scanners))))))))))

(defn beacons-match?
  [left right]
  (if (seq (set/intersection (:fingerprint left) (:fingerprint right)))
    (let [adjusted-points
          (for [l-pt (:pts left)
                r-pt (:pts right)
                rot rotations
                :let [offset (vec-between l-pt (rot r-pt))
                      adjusted (->> (:pts right)
                                    (map #(vec-add (rot %) offset))
                                    set)]
                :when (-> (set/intersection (:pts left) adjusted)
                          count
                          (>= 12))]
            {:fingerprint (:fingerprint right)
             :pts adjusted
             :offset offset})]
      (first adjusted-points))))

(defn match-all-beacons
  [{:keys [adjusted input] :or {adjusted []}}]
  (prn (count adjusted) (count input))
  (let [next-input (first input)
        rest-input (into [] (rest input))]
    (if (empty? adjusted)
      {:adjusted [next-input]
       :input rest-input}
      (let [next-adjusted (->> adjusted
                               (map #(beacons-match? % next-input))
                               (remove nil?))]
        (if (seq next-adjusted)
          {:adjusted (conj adjusted (first next-adjusted))
           :input rest-input}
          {:adjusted adjusted
           :input (conj rest-input next-input)})))))

(defn prep-scanner
  [pts]
  {:pts (set pts)
   :fingerprint (-> pts
                    scanner-dists
                    keys
                    set)
   :offset [0 0 0]})

(defn manhattan-dists
  [pts]
  (for [a pts
        b pts
        :let [vec (vec-between a b)]]
    (reduce + (map #(Math/abs %) vec))))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        vals
        (mapv prep-scanner)
        (#(hash-map :input %))
        (iterate match-all-beacons)
        (take 1000)
        (drop-while (comp seq :input))
        first
        :adjusted
        (map :pts)
        (apply concat)
        set
        count)))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (->> input
        vals
        (mapv prep-scanner)
        (#(hash-map :input %))
        (iterate match-all-beacons)
        (take 1000)
        (drop-while (comp seq :input))
        first
        :adjusted
        (map :offset)
        manhattan-dists
        (apply max))))

(comment
  (prep-scanner (:scanner-0 sample))
  (do-2  input)
  (->> sample
       vals
       (#(hash-map :input (into [] %)))
       (iterate match-all-beacons)
       (take 1000)
       (drop-while (comp seq :input))
       first
       :adjusted
       (apply concat)
       set
       count)
  (conj (pop [1 2 3 4]) (peek [1 2 3 4]))
  (beacons-match? (:scanner-2 sample) (:scanner-4 sample))
  (->> {:scanners input}
       (iterate merge-scanners)
       (drop 60)
       first
       :scanners)
  (->> {:scanners sample}
       (iterate merge-scanners)
       (drop-while #(-> % :scanners seq))
       first
       :merged
       count)
  (- 1)
  (into [] sample)

  (get {3 1 2 4} #{3 2})

  (sort [[2 5] [2 4]])
  (count (alt-points [4 5 6]))
  (scanner-dists [[:1] [[0 0 0] [1 1 1] [1 0 1]]])
  (->> [[1 2 3] [4 5 6]]
       (map point-rotations)
       (apply map vector))
  (point-rotations [1 2 3])
  (-> sample (#(into [] %)) (nth 0) scanner-dists)
  (-> sample (#(into [] %)) (nth 1) scanner-dists)
  (->> sample
       (map (comp scanner-dists peek))
       first)
  )

