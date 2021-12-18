(ns advent-of-code-2021.day18
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.zip :as z]))

(def sample [[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
             [[[5,[2,8]],4],[5,[[9,9],0]]]
             [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
             [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
             [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
             [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
             [[[[5,4],[7,7]],8],[[8,3],8]]
             [[9,3],[[9,9],[6,[4,9]]]]
             [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
             [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]])
(def input (core/get-input-as-edn 18))

(defn top
  [node]
  (-> node
      z/root
      z/vector-zip))

(defn leaf?
  [node]
  (-> node z/node number?))

(defn root?
  [[_ path]]
  (nil? path))

(defn next-leaf
  [node]
  (->> node
       (iterate z/next)
       (take 1000)
       (drop 1)
       (drop-while #(not (or (z/end? %) (leaf? %))))
       first))

(defn prev-leaf
  [node]
  (->> node
       (iterate z/prev)
       (take 1000)
       (drop 1)
       (drop-while #(not (or (root? %) (leaf? %))))
       first))

(defn explode
  [{:keys [node] :as state}]
  (if (-> node z/path count (> 4))
    (let [[l r] (-> node z/up z/node)]
      (-> node
          z/up
          (z/replace 0)
          prev-leaf
          (#(if (leaf? %) (z/edit % + l) %))
          next-leaf
          next-leaf
          (#(if (leaf? %) (z/edit % + r) %))
          (#(assoc state
                   :node %
                   :done true
                   :acted true))))
    (-> state
        (update :node next-leaf)
        (#(assoc % :done (z/end? (:node %)))))))

(defn split
  [{:keys [node] :as state}]
  (if (-> node z/node (>= 10))
    (-> node
        (z/edit #(vector (Math/floor (/ % 2)) (Math/ceil (/ % 2))))
        (#(assoc state
                 :node %
                 :done true
                 :acted true)))
    (-> state
        (update :node next-leaf)
        (#(assoc % :done (z/end? (:node %)))))))

(defn do-once
  [state f]
  (->> state
       (iterate f)
       (take 10000)
       (drop-while (comp not :done))
       first))

(defn reduce-step
  [{:keys [node]}]
  (let [after-explode (do-once {:node (-> node top next-leaf)} explode)]
    (if (:acted after-explode)
      {:node (:node after-explode)
       :done false}
      (let [after-split (do-once {:node (-> after-explode :node top next-leaf)} split)]
        {:node (:node after-split)
         :done (not (:acted after-split))}))))

(defn reduce-num
  [number]
  (->> number
       z/vector-zip
       (#(hash-map :node %))
       (iterate reduce-step)
       (drop-while (comp not :done))
       first
       :node
       top
       z/node))

(defn add
  [lnum rnum]
  (reduce-num [lnum rnum]))

(defn magnitude
  [number]
  (if (vector? number)
    (let [[left right] number]
      (+ (* 3 (magnitude left)) (* 2 (magnitude right))))
    (int number)))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        (reduce add)
        magnitude)))

(defn do-2
  ([]
   (do-2 input))
  ([input]
   (->> (for [left input
             right input
             :when (not= left right)]
         (magnitude (add left right)))
       (apply max))))

(comment
  (do-1 sample)
  (-> [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
      reduce-num)
  (-> [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
      z/vector-zip
      next-leaf
      next-leaf
      next-leaf
      next-leaf
      z/node))


