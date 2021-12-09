(ns analysis
  (:require [clojure.math.combinatorics :as combo]
            [erv.cps.similarity :as cpss]
            [erv.cps.core :as cps]
            [clojure.set :as set]
            [erv.scale.core :as scale]))



(do
  (defn all-rotations
    "Zero based rotations. `scale` is in cents"
    [scale]
    (map (fn [note] (sort (map #(mod (- % note) 1200) scale))) scale))

  (def dorico-six-notes (set (mapcat all-rotations (combo/combinations [0 200 300 500 700 900 1000] 6)))))

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

  (-> (combo/combinations [1 3 9 19 15 21 11] 4)
      (->> (pmap #(->> (cps/make 2 %)
                       (cpss/+gens %)
                       cpss/+cents
                       cpss/+euclidean-distance))
           (sort-by :euclidean-distance)
           ;; can use filter to look for scales that fit dorian
           (remove #(dorico-six-notes
                     (:closest-12-edo %)))
           (sort-by :euclidean-distance)
           (map (juxt :factors :cents)))))


(def candidate (cps/make 3 [1 3 9 19 15 21 11]))
