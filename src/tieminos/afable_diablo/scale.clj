(ns tieminos.afable-diablo.scale
  (:require
   [erv.cps.core :as cps]
   [erv.utils.conversions :as conv]
   [erv.scale.scl :refer [make-scl-file]]))

(def root 293.66476791741)

(defn +cents [scale]
  (map #(assoc % :cents (conv/ratio->cents (:bounded-ratio %)))
       scale))

(defn +degree [scale]
  (map-indexed (fn [i n] (assoc n :degree i)) scale))

(defn dedupe-scale [scale]
  (->> scale
       (group-by :bounded-ratio)
       (map (comp first second))
       (sort-by :bounded-ratio)))

(def polydori
  (-> (cps/make 4 [1 3 9 19 15 21 7]
                :norm-fac (* 15 21 19 9) ; la buena tonica del dorico
                ;; :norm-fac (* 7 1 15 3)
                )
      cps/+all-subcps
      (update :scale (comp +cents +degree))))

(comment (-> polydori :scale))

(def polydori-by-sets (->> polydori :scale (group-by :set)))
#_(-> polydori-by-sets)

(comment
  (let [{:keys [filename content]} (make-scl-file (assoc polydori :scale
                                                         (->> polydori :scale
                                                              (group-by :bounded-ratio)
                                                              (map (comp first second))
                                                              (sort-by :bounded-ratio)
                                                              #_(map :bounded-ratio))))]
    (spit (str "/Users/diego/Music/tunings/" filename) content)))

