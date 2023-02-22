(ns tieminos.habitat.attractors
  (:require [tieminos.attractors.lorentz :as lorentz]))

(def lor1 (lorentz/init-system :x 0.3 :y 0.02 :z 0.012))

(comment
  (->> 1000
       range
       (map #(lorentz/bound (lor1 (+ 500 (* 5 %))) :x 0 3))
       (partition 2 1)
       (map (comp #(Math/abs %) #(apply - %)))
       (sort)
       reverse))
