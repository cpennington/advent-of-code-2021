(ns advent-of-code-2021.search
  (:require
   [clojure.data.priority-map :refer [priority-map-keyfn]]))

(defn state-cost*
  [est-fn prior-cost [next-state delta-cost]]
  (let [new-actual (+ (:actual prior-cost) delta-cost)
        new-estimate (est-fn next-state)]
    [next-state
     {:actual new-actual
      :est new-estimate
      :total (+ new-estimate new-actual)}]))

(defn explore-next
  [{:keys [neighbor-fn est-fn frontier visited found target] :as search-state}]
  (let [[state state-cost] (peek frontier)
        rest-frontier (when (< 0 (count frontier)) (pop frontier))
        neighbors (remove #(contains? visited (first %)) (neighbor-fn state))
        neighbor-costs (into {} (map #(state-cost* est-fn state-cost %) neighbors))
        next-frontier (merge-with #(min-key :actual %1 %2) rest-frontier neighbor-costs)]
    (when (= 0 (mod (count visited) 1000))
      (prn {:visited (count visited)
            :state-cost state-cost
            :frontier (count frontier)
            :next (peek frontier)}))
    (when (= 12521 (:actual state-cost))
      (prn state state-cost))
    (assoc search-state
           :frontier next-frontier
           :visited (assoc visited state (:actual state-cost))
           :found (into found (filter (fn [[state _]]
                                        (= state target))
                                      neighbor-costs)))))

(defn search
  [search-state]
  (->> search-state
   (iterate explore-next)
   (drop-while (comp empty? :found))
   first
   :found
   first))

(defn setup
  [{:keys [est-fn neighbor-fn initial-states target]}]
  {:frontier (into (priority-map-keyfn :total)
                   (map #(vector % {:actual 0
                                    :est (est-fn %)
                                    :total (est-fn %)})
                        initial-states))
   :visited {}
   :found []
   :est-fn est-fn
   :neighbor-fn neighbor-fn
   :target target})

