(ns advent-of-code-2021.day22
  (:require
   [advent-of-code-2021.core :as core]
   [clojure.string :as str]
   [clojure.set :as set]))

(defn parse-input
  [input]
  (->> input
      str/split-lines
      (map #(re-find #"(on|off) x=(.*)\.\.(.*),y=(.*)\.\.(.*),z=(.*)\.\.(.*)" %))
       (map (fn [[_ val xmin xmax ymin ymax zmin zmax]]
              [(= val "on")
               [(Integer/parseInt xmin)
                (inc (Integer/parseInt xmax))
                (Integer/parseInt ymin)
                (inc (Integer/parseInt ymax))
                (Integer/parseInt zmin)
                (inc (Integer/parseInt zmax))]]))))

(def mini-sample (parse-input "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10"))
(def sample (parse-input "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682"))
(def sample-2 (parse-input "on x=-5..47,y=-31..22,z=-19..33
on x=-44..5,y=-27..21,z=-14..35
on x=-49..-1,y=-11..42,z=-10..38
on x=-20..34,y=-40..6,z=-44..1
off x=26..39,y=40..50,z=-2..11
on x=-41..5,y=-41..6,z=-36..8
off x=-43..-33,y=-45..-28,z=7..25
on x=-33..15,y=-32..19,z=-34..11
off x=35..47,y=-46..-34,z=-11..5
on x=-14..36,y=-6..44,z=-16..29
on x=-57795..-6158,y=29564..72030,z=20435..90618
on x=36731..105352,y=-21140..28532,z=16094..90401
on x=30999..107136,y=-53464..15513,z=8553..71215
on x=13528..83982,y=-99403..-27377,z=-24141..23996
on x=-72682..-12347,y=18159..111354,z=7391..80950
on x=-1060..80757,y=-65301..-20884,z=-103788..-16709
on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856
on x=-52752..22273,y=-49450..9096,z=54442..119054
on x=-29982..40483,y=-108474..-28371,z=-24328..38471
on x=-4958..62750,y=40422..118853,z=-7672..65583
on x=55694..108686,y=-43367..46958,z=-26781..48729
on x=-98497..-18186,y=-63569..3412,z=1232..88485
on x=-726..56291,y=-62629..13224,z=18033..85226
on x=-110886..-34664,y=-81338..-8658,z=8914..63723
on x=-55829..24974,y=-16897..54165,z=-121762..-28058
on x=-65152..-11147,y=22489..91432,z=-58782..1780
on x=-120100..-32970,y=-46592..27473,z=-11695..61039
on x=-18631..37533,y=-124565..-50804,z=-35667..28308
on x=-57817..18248,y=49321..117703,z=5745..55881
on x=14781..98692,y=-1341..70827,z=15753..70151
on x=-34419..55919,y=-19626..40991,z=39015..114138
on x=-60785..11593,y=-56135..2999,z=-95368..-26915
on x=-32178..58085,y=17647..101866,z=-91405..-8878
on x=-53655..12091,y=50097..105568,z=-75335..-4862
on x=-111166..-40997,y=-71714..2688,z=5609..50954
on x=-16602..70118,y=-98693..-44401,z=5197..76897
on x=16383..101554,y=4615..83635,z=-44907..18747
off x=-95822..-15171,y=-19987..48940,z=10804..104439
on x=-89813..-14614,y=16069..88491,z=-3297..45228
on x=41075..99376,y=-20427..49978,z=-52012..13762
on x=-21330..50085,y=-17944..62733,z=-112280..-30197
on x=-16478..35915,y=36008..118594,z=-7885..47086
off x=-98156..-27851,y=-49952..43171,z=-99005..-8456
off x=2032..69770,y=-71013..4824,z=7471..94418
on x=43670..120875,y=-42068..12382,z=-24787..38892
off x=37514..111226,y=-45862..25743,z=-16714..54663
off x=25699..97951,y=-30668..59918,z=-15349..69697
off x=-44271..17935,y=-9516..60759,z=49131..112598
on x=-61695..-5813,y=40978..94975,z=8655..80240
off x=-101086..-9439,y=-7088..67543,z=33935..83858
off x=18020..114017,y=-48931..32606,z=21474..89843
off x=-77139..10506,y=-89994..-18797,z=-80..59318
off x=8476..79288,y=-75520..11602,z=-96624..-24783
on x=-47488..-1262,y=24338..100707,z=16292..72967
off x=-84341..13987,y=2429..92914,z=-90671..-1318
off x=-37810..49457,y=-71013..-7894,z=-105357..-13188
off x=-27365..46395,y=31009..98017,z=15428..76570
off x=-70369..-16548,y=22648..78696,z=-1892..86821
on x=-53470..21291,y=-120233..-33476,z=-44150..38147
off x=-93533..-4276,y=-16170..68771,z=-104985..-24507"))
(def input (parse-input (core/get-input 22)))

(defn overlap
  [[xmin xmax ymin ymax zmin zmax]
   [xmin2 xmax2 ymin2 ymax2 zmin2 zmax2]]
  [(max xmin xmin2)
   (min xmax xmax2)
   (max ymin ymin2)
   (min ymax ymax2)
   (max zmin zmin2)
   (min zmax zmax2)])

(defn volume
  [[xmin xmax ymin ymax zmin zmax]]
  (* (- xmax xmin) (- ymax ymin) (- zmax zmin)))

(defn overlap?
  [cube1 cube2]
  (not= 0 (volume (overlap cube1 cube2))))

(defn split-x
  [[xmin xmax ymin ymax zmin zmax] x]
  (if (and (< xmin x) (< x xmax))
    [[xmin x ymin ymax zmin zmax]
     [x xmax ymin ymax zmin zmax]]
    [[xmin xmax ymin ymax zmin zmax]]))

(defn split-y
  [[xmin xmax ymin ymax zmin zmax] y]
  (if (and (< ymin y) (< y ymax))
    [[xmin xmax ymin y zmin zmax]
     [xmin xmax y ymax zmin zmax]]
    [[xmin xmax ymin ymax zmin zmax]]))

(defn split-z
  [[xmin xmax ymin ymax zmin zmax] z]
  (if (and (< zmin z) (< z zmax))
    [[xmin xmax ymin ymax zmin z]
     [xmin xmax ymin ymax z zmax]]
    [[xmin xmax ymin ymax zmin zmax]]))

(defn combine-cubes
  [[xmin xmax ymin ymax zmin zmax :as cube1]
   [xmin2 xmax2 ymin2 ymax2 zmin2 zmax2 :as cube2]]
  (let [match-x (and (= xmin xmin2)
                     (= xmax xmax2))
        match-y (and (= ymin ymin2)
                     (= ymax ymax2))
        match-z (and (= zmin zmin2)
                     (= zmax zmax2))]
    (cond
      (and match-y match-z (and (>= xmax xmin2) (>= xmax2 xmin)))
      [(min xmin xmin2) (max xmax xmax2) ymin ymax zmin zmax]
      (and match-x match-z (and (>= ymax ymin2) (>= ymax2 ymin)))
      [xmin xmax (min ymin ymin2) (max ymax ymax2) zmin zmax]
      (and match-x match-y (and (>= zmax zmin2) (>= zmax2 zmin)))
      [xmin xmax ymin ymax (min zmin zmin2) (max zmax zmax2)]
      :else nil)))

(defn combine-all
  ([{:keys [dirty reduced] :or {dirty [] reduced []}}]
;;    (prn "combine-all" (count dirty) (count reduced))
   (cond
     (or (empty? dirty) (nil? dirty))
     (do
       (prn (for [l reduced
                  r reduced
                  :when (and (not= l r)
                             (overlap? l r))]
              [l r]))
       (set reduced))
     :else (let [[fst & rest] dirty
                 [first-combine] (->> reduced
                                      (map #(vector (combine-cubes fst %) %))
                                      (remove (comp nil? first)))]
             (if (nil? first-combine)
               (recur {:dirty rest :reduced (conj reduced fst)})
               (let [[combined combined-with] first-combine
                     dirty* (into #{} (conj rest combined))
                     reduced* (into #{} (remove #{combined-with} reduced))]
                 (when (or (not= dirty dirty*) (not= reduced reduced*))
                   (recur {:dirty dirty* :reduced reduced*}))))))))

(defn split-all
  [cubes split-fn]
  (into #{} (mapcat split-fn cubes)))

(defn merge-cubes
  [[xmin-1 xmax-1 ymin-1 ymax-1 zmin-1 zmax-1 :as existing]
   [on? [xmin-2 xmax-2 ymin-2 ymax-2 zmin-2 zmax-2 :as new]]]
  (if (overlap? existing new)
    (if on?
      (let [splits-2 (reduce split-all #{new} [#(split-x % xmin-1)
                                               #(split-x % xmax-1)
                                               #(split-y % ymin-1)
                                               #(split-y % ymax-1)
                                               #(split-z % zmin-1)
                                               #(split-z % zmax-1)])
            remove-overlap (disj splits-2 (overlap existing new))
            recombined (combine-all {:dirty remove-overlap})]
        {:reduced #{existing}
         :dirty recombined})
      (let [splits-1 (reduce split-all #{existing} [#(split-x % xmin-2)
                                                    #(split-x % xmax-2)
                                                    #(split-y % ymin-2)
                                                    #(split-y % ymax-2)
                                                    #(split-z % zmin-2)
                                                    #(split-z % zmax-2)])
            remove-overlap (disj splits-1 (overlap existing new))
            recombined (combine-all {:dirty remove-overlap})]
          {:dirty recombined}))
    {:reduced #{existing}
     :dirty #{new}}))

(defn add-cube
  [on-cubes [on? cube]]
;;   (prn (count on-cubes))
  (if (empty? on-cubes)
    (if on? #{cube} #{})
    (let [{overlapping-cubes true
           not-overlapping false :or
           {overlapping-cubes []
            not-overlapping []}} (group-by #(overlap? % cube) on-cubes)
        ;;   _ (prn "overlapping" overlapping-cubes not-overlapping)
          merged-cubes (for [cube* overlapping-cubes]
                         (merge-cubes cube* [on? cube]))
          to-combine (apply merge-with
                            into
                            (into [{:reduced not-overlapping}] merged-cubes))
        ;;   _ (prn "to-combine" to-combine (update to-combine :dirty #(combine-all {:dirty %})))
          ]
      (combine-all to-combine))))

(defn valid-1
  [[on? [xmin xmax ymin ymax zmin zmax]]]
  (every? #(and (>= % -50) (<= % 51)) [xmin xmax ymin ymax zmin zmax]))

(defn do-1
  ([]
   (do-1 input))
  ([input]
   (->> input
        (filter valid-1)
        (reduce add-cube {})
        (map volume)
        (reduce +))))

(defn do-2
  ([]
  (do-2 input))
  ([input]
   (->> input
        (reduce add-cube {})
        (#(get % true))
        (map volume)
        (reduce +))))

(comment
  (combine-all { :dirty #{[-22 8 24 47 -50 0] [-46 -22 -6 47 -50 0] [-22 8 -6 24 -50 -26]}})
  (map first '([[[0 2 0 1 0 1]] [1 2 0 1 0 1] [2 3 0 1 0 1]]))
  (combine-all {:dirty [[0 1 0 1 0 1] [1 2 0 1 0 1] [2 3 0 1 0 1]]})
  (clamp-1 (last sample))
  (filter valid-1 sample)

  (->> sample
       (filter valid-1)
       (reduce add-cube {})
       (map volume)
       (reduce +))
  (require '[clj-async-profiler.core :as prof])
  (prof/profile (do-1 sample))
  (prof/serve-files 8080)

  (reduce add-cube {} (map clamp-1 sample))
  sample
  (do-1 (take 8 sample))
  (->> sample
       (filter valid-1)
       (reduce add-cube {})
       (#(get % true))
       (map volume)
       (reduce +))
  )

