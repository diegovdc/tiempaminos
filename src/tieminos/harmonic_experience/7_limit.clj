(ns tieminos.harmonic-experience.7-limit
  (:require
   [clojure.math :refer [pow]]
   [erv.utils.core :refer [period-reduce]]))

(->> (range -4 4)
     (map #(apply * (repeat (abs %) (if (> % 0) 7 1/7))))
     (map period-reduce)
     #_(conv/cps->name*))
