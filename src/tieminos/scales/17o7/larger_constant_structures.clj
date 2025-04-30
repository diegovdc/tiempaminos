(ns tieminos.scales.17o7.larger-constant-structures
  (:require
   [clojure.edn :as edn]
   [erv.constant-structures.brute-force :as cs.brute-force]
   [erv.lattice.v2]
   [erv.utils.core :refer [period-reduce]]
   [erv.utils.ratios :refer [ratios->scale]]
   [erv.utils.scale :refer [find-subset-degrees]]
   [tieminos.lattice.v1.lattice :as lattice.v1]
   [tieminos.scales.17o7.original :refer [original-tritavic-scale-ratios]]))

(comment

  (do
    (def ratios (let [chain (reduce (fn [acc i] (conj acc (apply * (repeat i 2)))) [] (range 9))
                      ratios  (->> (flatten [chain
                                             (map #(* 17/7 %) chain)
                                             (map #(* 7/17 %) chain)])
                                   (map #(period-reduce 3 (/ % 8/3))))]
                  ratios))

    (count ratios))
  (lattice.v1/draw-lattice
   {:id :17o7/larger-cs-search
    :ratios ratios
    :period 3
    :custom-edges #{17/7}
    :width 1600
    :height 600
    :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[2 3]])})

  (def subsets (cs.brute-force/quick-cs-subsets [17] (ratios->scale 3 ratios)))
  (take 1 subsets)
  (->  subsets count)
  (nth subsets 0)

  (spit "resources/data-sets/17o7-constant-structures/17t-cs-from-27-tone-no1.edn"
        (str (into [] subsets)))
  ;; 12, 15, 16, 18, 19, 20, 21,

  (def subset-index (atom -1))
  (lattice.v1/draw-lattice
   {:id :17o7/larger-cs-subset
    :ratios (map :bounded-ratio (nth subsets (swap! subset-index inc)))
    :period 3
    :custom-edges #{17/7}
    :width 1600
    :height 600
    :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[2 3]])}))

(comment
  (def cs-of-22t-from-30t
    (->> (slurp "resources/data-sets/17o7-constant-structures/22t-cs-from-30-tone-no1.edn")
         edn/read-string))

  (->> cs-of-22t-from-30t
       #_(take 2)
       (map-indexed
        (fn [i scale]
          (let [match-data (find-subset-degrees
                            {:scale scale
                             :subset-ratios original-tritavic-scale-ratios})]
            {:scale-index i
             :match-data {:total (count match-data)
                          :data match-data}})))
       (sort-by #(-> % :match-data :total) >))

  cs-of-22t-from-30t)
