(ns tieminos.compositions.garden-earth.scratch.1-3-7-9-11-15-exploration
  (:require
   [clojure.set :as set]
   [erv.cps.core :as cps]))

(comment
  (set/difference
    (->> (cps/make 3 [1 3 7 9 11 15])
         :scale
         (map :bounded-ratio)
         set)
    (->> (cps/make 3 [1 3 5 7 9 11])
         :scale
         (map :bounded-ratio)
         set)))
