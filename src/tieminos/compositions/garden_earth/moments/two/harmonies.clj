(ns tieminos.compositions.garden-earth.moments.two.harmonies
  (:require
   [erv.meru.core :as meru]
   [erv.utils.ratios :refer [ratios->scale]]))


(def meta-pelog
  "5 tone meta-pelog"
  (->> {:seed [1 1 1]
        :formula :meta-pelog}
       (meru/recurrent-series)
       :series
       (drop 5)
       (take 5)
       (ratios->scale)))

(def meta-pelog2
  "5 tone meta-pelog (Wilson's own reseed)"
  (->> {:seed [1 2 2]
        :formula :meta-pelog}
       (meru/recurrent-series)
       :series
       (drop 5)
       (take 5)
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

(defn normalize-to-1
  [n-seq]
  (let [min* (apply min n-seq)]
    (map #(/ % min*) n-seq)))

(def meta-slendro2
  "12 tone meta-slendro (Wilson's own reseed)"
  (->> {:seed [2 1 1]
        :formula :meta-slendro}
       (meru/recurrent-series)
       :series
       (drop 10)
       (take 12)
       normalize-to-1
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
  (-> meta-pelog)
  (-> meta-pelog2)
  (-> meta-slendro2)
  (-> meta-slendro1))
