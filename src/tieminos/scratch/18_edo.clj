(ns tieminos.scratch.18-edo
  (:require
   [clojure.set :as set]
   [erv.edo.molt :as molt]
   [erv.utils.core :refer [get-all-rotations]]))

(do
  (defn as-degrees [pattern]
    (->> pattern
         (reduce (fn [acc interval]
                   (conj acc (+ (last acc) interval)))
                 [0])
         (drop-last)))

  (as-degrees '(5 4 5 4)))

(sort-by count (molt/make 18))

(set/subset? #{1} #{1})

(do
  (defn- count-chord-in-molt
    [chord molt]
    (->> molt
         get-all-rotations
         (map (fn [molt-r]
                (if (->> molt-r as-degrees set (#(set/subset? (set chord) %)))
                  1 0)))
         (apply +)))

  (count-chord-in-molt [7 7 4] [5 4 5 4]))

(molt/make 18)

(->> (molt/make 18)
     (map (fn [molt]
            [molt]
            [molt [(count-chord-in-molt [3 3 1 3 3 1 3 1]
                                        molt)]]))
     (into {}))

(sort-by (fn [[_ counts]] (apply + counts))
         (merge-with concat
                     (->> (molt/make 18)
                          (map (fn [molt]
                                 [molt]
                                 [molt [(count-chord-in-molt [7 7 4] molt)]]))
                          (into {}))
                     (->> (molt/make 18)
                          (map (fn [molt]
                                 [molt]
                                 [molt [(count-chord-in-molt [5 7 6] molt)]]))
                          (into {}))
                     (->> (molt/make 18)
                          (map (fn [molt]
                                 [molt]
                                 [molt [(count-chord-in-molt [7 5 6] molt)]]))
                          (into {}))))
