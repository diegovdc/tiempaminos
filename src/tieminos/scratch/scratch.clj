(ns tieminos.scratch.scratch
  (:require [erv.cps.core :as cps]))

(concat []
        (->> (cps/make 2 [1 3 5 7] :norm-fac 7)
             :scale
             (map :bounded-ratio))
        #_(->> (cps/make 2 [1/3 1/4 1/5 1/7] :norm-fac 1/35)
               :scale
               (map (juxt :set :ratio))))

[[#{1 1/7} 1N]
 [#{1/3 1} 7/6]
 [#{1/3 1/7} 4/3]
 [#{1/5 1} 7/5]
 [#{1/5 1/7} 8/5]
 [#{1/3 1/5} 28/15]]
