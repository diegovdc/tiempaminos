(ns tieminos.afable-diablo.scale
  (:require
   [erv.cps.core :as cps]
   [erv.utils.conversions :as conv]))

(defn +cents [scale]
  (map #(assoc % :cents (conv/ratio->cents (:bounded-ratio %)))
       scale))
(defn +degree [scale]
  (map-indexed (fn [i n] (assoc n :degree i)) scale))

(def polydori
  (-> (cps/make 4 [1 3 9 19 15 21 7]
                :norm-fac (* 15 21 19 9)  ; la buena tonica del dorico
                ;; :norm-fac (* 7 1 15 3)
                )
      cps/+all-subcps
      (update :scale (comp +cents +degree))))
(comment (-> polydori :scale))
(def polydori-by-sets (->> polydori :scale (group-by :set)))
#_(-> polydori-by-sets)
