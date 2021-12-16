(ns erv-fib-synth.afable-diablo.scale
  (:require
   [erv.cps.core :as cps]
   [erv.utils.conversions :as conv]))

(defn +cents [scale]
  (map #(assoc % :cents (conv/ratio->cents (:bounded-ratio %)))
       scale))
(defn +degree [scale]
  (map-indexed (fn [i n] (assoc n :degree i)) scale))

(def polydori
  (->> (cps/make 4 [1 3 9 19 15 21 7] :norm-fac (* 7 1 15 19))
       :scale
       +cents
       +degree))

(def polydori-by-sets (->> polydori (group-by :set)))
