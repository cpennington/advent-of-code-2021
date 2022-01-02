(ns advent-of-code-2021.day23
  (:require
   [advent-of-code-2021.core :as core]
   [advent-of-code-2021.grid :as grid]
   [advent-of-code-2021.search :as search]
   [clojure.set :as set]))

(defn ->hallway
  [ix]
  (inc ix))

(def offset-mult 4)

(defn ->room
  [label ix]
  (let [offset (case label
                 \A 0
                 \B 1
                 \C 2
                 \D 3)]
    (- (+ (* offset-mult offset) ix))))

(defn loc->ix
  [loc]
  (if (pos? loc)
    (dec loc)
    (mod (- loc) offset-mult)))

(defn loc->type
  [loc]
  (if (pos? loc)
    :hallway
    :room))

(defn loc->label
  [loc]
  (-> (- loc)
      (quot offset-mult)
      (+ (int \A))
      char))

(defn ix->str-ix
  [ix]
  (+ ix 15))

(def str-ixs (into [] (range 27)))
(defn str-ix->ix
  [str-ix]
  (- str-ix 15))

(def ixs (mapv str-ix->ix str-ixs))

(defn loc->piece
  [pieces loc]
  (if (string? pieces)
    (get pieces (ix->str-ix loc))
    (get pieces loc \.)))

(defn piece-locs
  [pieces]
  (if (string? pieces)
    (->> pieces
         (map vector ixs)
         (filter #(not= \. (peek %))))
    pieces))

(defn occupied-locs
  [pieces]
  (if (string? pieces)
    (->> (piece-locs pieces)
         (map first))
    (keys pieces)))

(defn replace-in-str [in f from len]
  (let [before (subs in 0 from)
        after (subs in (+ from len))
        being-replaced (subs in from (+ from len))
        replaced (f being-replaced)]
    (str before replaced after)))

(defn move-piece
  [pieces piece loc loc']
  (if (string? pieces)
    (let [p (loc->piece pieces loc)
          p' (loc->piece pieces loc')]
      (assert (= p piece))
      (assert (= p' \.))
      (-> pieces
          (replace-in-str (constantly p') (ix->str-ix loc) 1)
          (replace-in-str (constantly p) (ix->str-ix loc') 1)))
    (do
      (assert (= (get pieces loc) piece))
      (-> pieces
        (assoc loc' (get pieces loc))
        (dissoc loc)))))

(defn encode-pieces
  ([pieces]
   (encode-pieces pieces false))
  ([pieces encode?]
   (if encode?
     (->> str-ixs
          (map str-ix->ix)
          (map #(get pieces % \.))
          (apply str))
     pieces)))

(defn visualize-pieces
  [pieces]
  (let [hallway (apply str (map (comp (partial loc->piece pieces) ->hallway) (range 11)))]
    (str hallway "\n"
         "  " (loc->piece pieces (->room \A 0))
         " "  (loc->piece pieces (->room \B 0))
         " "  (loc->piece pieces (->room \C 0))
         " "  (loc->piece pieces (->room \D 0)) "  \n"
         "  " (loc->piece pieces (->room \A 1))
         " "  (loc->piece pieces (->room \B 1))
         " "  (loc->piece pieces (->room \C 1))
         " "  (loc->piece pieces (->room \D 1)) "  \n"
         "  " (loc->piece pieces (->room \A 2))
         " "  (loc->piece pieces (->room \B 2))
         " "  (loc->piece pieces (->room \C 2))
         " "  (loc->piece pieces (->room \D 2)) "  \n"
         "  " (loc->piece pieces (->room \A 3))
         " "  (loc->piece pieces (->room \B 3))
         " "  (loc->piece pieces (->room \C 3))
         " "  (loc->piece pieces (->room \D 3)) "  \n")))

(def mini-sample {:map-def {:hall-size 7
                            :rooms {\A {:size 2 :door 2}
                                    \B {:size 2 :door 4}}}
                  :pieces {(->room \A 0) \B
                           (->room \A 1) \B
                           (->room \B 0) \A
                           (->room \B 1) \A}})

(def sample {:map-def {:hall-size 11
                       :rooms {\A {:size 2 :door 2}
                               \B {:size 2 :door 4}
                               \C {:size 2 :door 6}
                               \D {:size 2 :door 8}}}
             :pieces {(->room \A 0) \B
                      (->room \A 1) \A
                      (->room \B 0) \C
                      (->room \B 1) \D
                      (->room \C 0) \B
                      (->room \C 1) \C
                      (->room \D 0) \D
                      (->room \D 1) \A}})
(def input {:map-def {:hall-size 11
                      :rooms {\A {:size 2 :door 2}
                              \B {:size 2 :door 4}
                              \C {:size 2 :door 6}
                              \D {:size 2 :door 8}}}
            :pieces {(->room \A 0) \A
                     (->room \A 1) \D
                     (->room \B 0) \C
                     (->room \B 1) \D
                     (->room \C 0) \B
                     (->room \C 1) \B
                     (->room \D 0) \A
                     (->room \D 1) \C}})

(defn part-2-input
  [input]
  (-> input
      (assoc-in [:map-def :rooms \A :size] 4)
      (assoc-in [:map-def :rooms \B :size] 4)
      (assoc-in [:map-def :rooms \C :size] 4)
      (assoc-in [:map-def :rooms \D :size] 4)
      (assoc-in [:pieces (->room \A 1)] \D)
      (assoc-in [:pieces (->room \B 1)] \C)
      (assoc-in [:pieces (->room \C 1)] \B)
      (assoc-in [:pieces (->room \D 1)] \A)
      (assoc-in [:pieces (->room \A 2)] \D)
      (assoc-in [:pieces (->room \B 2)] \B)
      (assoc-in [:pieces (->room \C 2)] \A)
      (assoc-in [:pieces (->room \D 2)] \C)
      (assoc-in [:pieces (->room \A 3)] (get-in input [:pieces (->room \A 1)]))
      (assoc-in [:pieces (->room \B 3)] (get-in input [:pieces (->room \B 1)]))
      (assoc-in [:pieces (->room \C 3)] (get-in input [:pieces (->room \C 1)]))
      (assoc-in [:pieces (->room \D 3)] (get-in input [:pieces (->room \D 1)]))))

(defn piece-cost
  [piece]
  (case piece
    \A 1
    \B 10
    \C 100
    \D 1000))

(defn add-map-fns
  [{:keys [map-def] :as search-state}]
  (let [rooms (:rooms map-def)]
    (letfn [(path-between
              [a b]
              (into #{}
                    (if (= a b)
                      [a]
                      (case [(loc->type a) (loc->type b)]
                        [:hallway :hallway]
                        (let [a-ix (loc->ix a)
                              b-ix (loc->ix b)]
                          (map ->hallway
                               (range (min a-ix b-ix) (inc (max a-ix b-ix)))))

                        [:room :room]
                        (let [a-label (loc->label a)
                              a-ix (loc->ix a)
                              b-label (loc->label b)
                              b-ix (loc->ix b)]
                          (if (= a-label b-label)
                            (map #(->room a-label %)
                                 (range (min a-ix b-ix) (inc (max a-ix b-ix))))
                            (let [a-to-door (path-between a (->room a-label 0))
                                  b-to-door (path-between b (->room b-label 0))
                                  a-door (-> rooms (#(get % a-label)) :door)
                                  b-door (-> rooms (#(get % b-label)) :door)
                                  door-to-door (path-between (->hallway a-door) (->hallway b-door))]
                              (concat a-to-door b-to-door door-to-door))))

                        [:hallway :room]
                        (let [label (loc->label b)
                              to-door (path-between b (->room label 0))
                              door (-> rooms (#(get % label)) :door)
                              door-to-hall (path-between a (->hallway door))]
                          (concat to-door door-to-hall))

                        [:room :hallway]
                        (path-between b a)))))]
      (let [path-between-memo (memoize path-between)]
        (assoc-in search-state
                  [:map-def :path-between] path-between-memo)))))

(defn pieces-in-room
 [{:keys [rooms]} pieces room-label]
 (->> (get rooms room-label)
      :size
      range
      (map #(->room room-label %))
      (map #(loc->piece pieces %))
      (remove #{\.})
      set))

(defn path-open?
  [{:keys [path-between]} pieces a b]
  (let [path (-> (path-between a b)
                 (disj a))]
    (every? #(= (loc->piece pieces %) \.) path)))

(defn distance
  [{:keys [path-between]} a b]
  (dec (count (path-between a b))))

(defn path-cost
  [map-def piece a b]
  (* (piece-cost piece) (distance map-def a b)))

(defn possible-moves
  [{:keys [rooms hall-size] :as map-def} pieces]
  (let [room-size (-> rooms (#(get % \A)) :size)
        doors (->> rooms vals (map :door) set)
        room-occupants (into {} (map #(vector % (pieces-in-room map-def pieces %))
                                     [\A \B \C \D]))]
    (for [[loc p] (piece-locs pieces)
          :let [own-room-clean (->> (get room-occupants p)
                                    (remove #(= p %))
                                    empty?)]
          ;; Don't move if the current piece is in a clean room
          :when (not (and (= :room (loc->type loc))
                          (= p (loc->label loc))
                          own-room-clean))
          loc' (into (if own-room-clean
                       ;; Move into the deepest spot in own clean room
                       (->> room-size
                            (range 0)
                            (map #(->room p %))
                            (filter #(= \. (loc->piece pieces %)))
                            last
                            vector)
                       [])
                     (when (= (loc->type loc) :room)
                       (->> hall-size
                            range
                            (remove doors)
                            (map #(->hallway %)))))
          :when (and (not= loc loc')
                     (path-open? map-def pieces loc loc'))]
      [(move-piece pieces p loc loc')
       (path-cost map-def p loc loc')])))

(defn estimate-cost
  [{:keys [rooms hall-size] :as map-def} pieces]
  (let [room-size (-> rooms (#(get % \A)) :size)
        doors (->> rooms vals (map :door) set)
        room-occupants (into {} (map #(vector % (pieces-in-room map-def pieces %))
                                     [\A \B \C \D]))]
    (->> (piece-locs pieces)
         (map (fn [[loc piece]]
                (if (= piece (loc->label loc))
                  0
                  (* (piece-cost piece)
                     (distance map-def (->room piece 0) loc)))))
         (reduce + 0))))

(defn setup-search
  [{:keys [map-def pieces string-state?]}]
  (search/setup
   {:initial-states [(encode-pieces pieces string-state?)]
    :neighbor-fn #(possible-moves map-def %)
    :est-fn #(estimate-cost map-def %)
    ;; :est-fn (constantly 0)
    :target (->> pieces
                 (map (comp #(vector % (loc->label %)) first))
                 (into {})
                 (#(encode-pieces % string-state?)))}))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        add-map-fns
        setup-search
        (iterate search/explore-next)
        (take 400000)
        (drop-while (comp empty? :found))
        first
        :found
        first
        peek
        :total)))

(defn do-2
  ([]
   (do-2 input))
  ([input]
   (->> input
        part-2-input
        add-map-fns
        setup-search
        (iterate search/explore-next)
        (take 400000)
        (drop-while (comp empty? :found))
        first
        :found
        first
        peek
        :total)))

(comment
  (->> sample
       add-map-fns
       setup-search
       (iterate search/explore-next)
       (drop 10000)
       first
       :visited
       (group-by peek)
       (map #(vector (first %) (count (peek %))))
       sort)
  
  (->> sample
       add-map-fns
       :map-def
       :path-between
       (#(% (->room \A 1) (->room \C 0)))
       (map #(vector (loc->type %) (loc->label %) (loc->ix %)))
       sort)
  (print (visualize-pieces (encode-pieces (:pieces sample))))
  (update "12345" 3 (constantly \2))
  (occupied-locs (encode-pieces (:pieces (part-2-input sample))))
  (let [a (encode-pieces (:pieces sample) true)
        b (add-map-fns sample)
        c (->room \A 1)
        d (->room \D 1)]
    (time (dotimes [_ 100] ((get-in b [:map-def :path-between]) c d))))
  (let [a [2 5] b [3 6]] (time (dotimes [_ 100000] (= a b))))
  (let [a "12345" b "23456"] (time (dotimes [_ 100000] (= a b))))
  (require '[clj-async-profiler.core :as prof])

  (let [search-state (->> sample
                          part-2-input
                          add-map-fns
                          setup-search)
        pieces {(->room \B 0) \B
                (->room \D 2) \D
                (->room \D 1) \D
                (->room \A 3) \A
                (->room \C 2) \C
                (->room \C 1) \C
                (->room \B 3) \B
                (->room \D 3) \D
                (->room \B 1) \B
                (->room \B 2) \B
                (->room \A 2) \A
                (->room \C 3) \C
                (->room \D 0) \D
                (->room \A 1) \A
                (->room \C 0) \C
                (->room \A 0) \A}]
    (print (visualize-pieces pieces))
    (map (comp print visualize-pieces first) ((:neighbor-fn search-state) pieces)))

  (prof/serve-files 8080)
  (prof/profile (time (do-2)))
  (prof/profile (->> sample
                     part-2-input
                     add-map-fns
                     setup-search
      ;; :neighbor-fn
      ;; (#(% (:initial-states (first (setup-search sample)))))
                     (iterate search/explore-next)
                    ;;  (take 1000000)
                    ;;  (drop 10000)
                     (drop-while (comp empty? :found))
                     first
                     (#(assoc % :next (take 5 (:frontier %))))
                     (#(update % :frontier count))
                     (#(update % :visited count))
                     clojure.pprint/pprint))
  (part-2-input sample))
