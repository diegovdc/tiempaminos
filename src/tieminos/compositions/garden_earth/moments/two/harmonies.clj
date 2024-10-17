(ns tieminos.compositions.garden-earth.moments.two.harmonies
  (:require
   [erv.meru.core :as meru]
   [erv.utils.ratios :refer [ratios->scale]]))


(def meta-pelog
  "12 tone meta-pleog"
  (->> {:seed [1 1 1]
        :formula :meta-pelog}
       (meru/recurrent-series)
       :series
       (drop 8)
       (take 7)
       (ratios->scale)))


(def meta-slendro1
  "12 tone meta-slendro"
  (->> {:seed [1 1 1]
        :formula :meta-slendro}
       (meru/recurrent-series)
       :series
       (drop 10)
       (take 12)
       (ratios->scale)))

(def fib
  (->> {:seed [1 1 2]
        :formula :fibonacci}
       (meru/recurrent-series)
       :series
       (drop 3)
       (take 12)
       (ratios->scale)))

(comment
  (-> fib)
  (-> meta-pelog))
