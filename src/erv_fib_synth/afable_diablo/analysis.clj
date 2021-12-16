(ns erv-fib-synth.afable-diablo.analysis
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv-fib-synth.afable-diablo.scale :refer [+cents]]
   [erv.cps.core :as cps]
   [erv.cps.similarity :as cpss]))

(do
  (defn all-rotations
    "Zero based rotations. `scale` is in cents"
    [scale]
    (map (fn [note] (sort (map #(mod (- % note) 1200) scale))) scale))

  (def dorico-six-notes
    (->> (combo/combinations [0 200 300 500 700 900 1000] 6)
         (mapcat all-rotations)
         #_(filter #(and ((set %) 300)))
         set)))

(comment
  ;; cps-sorted-by-euclidean-distance
  (-> (range 1 31 2)
      (combo/combinations 4)
      (->> (pmap #(->> (cps/make 2 %)
                       (cpss/+gens %)
                       cpss/+cents
                       cpss/+euclidean-distance))
           (sort-by :euclidean-distance)
           (filter #(dorico-six-notes
                     (:closest-12-edo %)))
           (sort-by :euclidean-distance)
           (group-by #(->> (:factors %)
                           set
                           (set/intersection #{1 3 9 19})
                           count))
           (#(dissoc % 0))
           (map (fn [[k v]]
                  [k (map (juxt :euclidean-distance :factors :cents) v)]))))

  (-> (combo/combinations [1 3 9 19 15 21 7] 4)
      (->> (pmap #(->> (cps/make 2 %)
                       (cpss/+gens %)
                       cpss/+cents
                       cpss/+euclidean-distance))
           (sort-by :euclidean-distance)
           ;; can use filter to look for scales that fit dorian
           (filter #(dorico-six-notes
                     (:closest-12-edo %)))
           (sort-by :euclidean-distance)
           (map (juxt :euclidean-distance :factors :cents)))
      #_  count))




(->> (cps/make 4 [1 3 9 19 15 21 7])
     cps/+all-subcps
     :subcps
     keys
     (filter #(str/includes? % "2)4"))
     (filter #(str/includes? % "3.7.19.21"))

     )

(-> (cps/make 4 [1 3 9 19 15 21 7] :norm-fac 315 #_(* 49/3 513))
    cps/+all-subcps
    :subcps
    ;; keys
    (select-keys '("2)4 of 4)7 1.15-3.7.19.21"
                   "2)4 of 4)7 9.15-3.7.19.21"
                   "2)4 of 4)7 1.9-3.7.19.21"))
    seq
    (nth 0)
    second
    :scale
    +cents)
