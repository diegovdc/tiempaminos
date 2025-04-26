(ns tieminos.scales.17o7.core
  (:require
   [erv.lattice.v2]
   [erv.utils.ratios :refer [ratios->scale]]
   [tieminos.scales.17o7.22tone-through-parallel-tranpositions
    :as variation-22t]
   [tieminos.scales.17o7.original :refer [original-tritavic-scale-ratios]]
   [tieminos.scales.17o7.some-constant-structures1 :as some-17o7-cs-1]
   [tieminos.utils :refer [careful-merge]]))

(def scales
  (->> (careful-merge
        {:original {:meta {:scl/name "8t-17o7-original-tritavic.scl"
                           :scl/description "Tritavic 17/7 colored scale."}
                    :scale (ratios->scale 3 original-tritavic-scale-ratios)}}
        variation-22t/scales
        some-17o7-cs-1/scales)
       (map (fn [[k scale-data]]
              [k (update-in scale-data [:meta]
                            merge {:period 3
                                   :lattice/custom-edges #{17/7}
                                   :lattice/coords (erv.lattice.v2/swap-coords
                                                    erv.lattice.v2/base-coords
                                                    [[2 3]])})]))
       (into {})))
