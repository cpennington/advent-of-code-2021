(ns advent-of-code-2021.search)

(defn state-cost*
  [cost-fn est-fn prior-cost next-state]
  (let [new-actual (cost-fn prior-cost next-state)
        new-estimate (est-fn next-state)]
    [next-state
     {:actual new-actual
      :est new-estimate
      :total (+ new-estimate new-actual)}]))

(defn explore-next
  [{:keys [neighbor-fn cost-fn est-fn frontier visited found] :as search-state}]
  (let [[state state-cost] (peek frontier)
        rest-frontier (when (< 0 (count frontier)) (pop frontier))
        neighbors (remove #(visited %) (neighbor-fn state))
        neighbor-costs (into {} (map #(state-cost* cost-fn est-fn state-cost %) neighbors))
        next-frontier (merge-with #(min-key :actual %1 %2) rest-frontier neighbor-costs)]
    (assoc search-state
           :frontier next-frontier
           :visited (conj visited state)
           :found (into found (filter (fn [[_ cost]]
                                        (= (:actual cost) (:total cost)))
                                      neighbor-costs)))))

(defn search
  [search-state]
  (->> search-state
   (iterate explore-next)
   (drop-while (comp empty? :found))
   first
   :found
   first))