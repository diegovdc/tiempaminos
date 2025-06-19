(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.harmonies
  (:require
   [erv.scale.core :as scale]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [root]]
   [tieminos.polydori.analysis.dorian-hexanies :refer [dorian-hexanies-in-polydori-2]]
   [tieminos.polydori.scale :refer [polydori-set->deg polydori-v2]]
   [tieminos.synths :refer [short-plate]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

(comment
  (mapv
   (fn [dorian]
     (let [diat4v2-sets
           (set (:sets (nth dorian-hexanies-in-polydori-2
                            dorian)))]

       (->> polydori-v2
            :subcps
            (filter (fn [[k {:keys [scale]}]]
                      (and (= 6 (count scale))
                           (= diat4v2-sets (set (map :set scale))))))
            #_first
            (mapv (fn [[k {:keys [scale]}]]

                    [dorian k (mapv (comp polydori-set->deg :set)
                                    scale)]))
             ;; first
            )))
   (range 21))

  [[["2)4 of 4)7 3.19-1.7.9.21" [3 8 9 14 20 26]]]
   [["2)4 of 4)7 15.19-1.7.9.21" [0 7 12 17 18 23]]]
   [["2)4 of 4)7 3.15-1.7.9.21" [4 10 16 22 27 28]]]
   [["2)4 of 4)7 3.15-7.9.19.21" [5 10 12 22 23 28]]]]

  (defn get-subscps-scale
    [cps-name]
    (:scale (get-in polydori-v2 [:subcps cps-name])))
  ;; non-diatonic neighbors
  (->> ["2)4 of 4)7 3.19-7.9.15.21" ;; 14b
        "2)4 of 4)7 15.19-3.7.9.21" ;; 12b
        ;; good, connects with "2)4 of 4)7 1.9-3.7.19.21" (number 10)
        "2)4 of 4)7 1.19-3.7.9.21" ;; 10b
        ]
       (mapv (fn [cps-name]
               [cps-name
                (->> cps-name
                     get-subscps-scale
                     (mapv (fn [note]

                             (->> ((comp polydori-set->deg :set)
                                   note))))
                     set
                     sort
                     vec)])))

  (rain.v2/stop)
  (rain.v2/ref-rain
   :id :non-diat
   :durs [1]
   :ratio 1/9
   :on-event (rain.v2/on-event
              #_(low (scale/deg->freq (get-subscps-scale "2)4 of 4)7 3.19-1.7.9.21")
                                      (* root 2)
                                      (at-i [0 5 1 -2 3])))
              (short-plate (scale/deg->freq (get-subscps-scale (at-i ["2)4 of 4)7 1.19-3.7.9.21"
                                                                      "2)4 of 4)7 15.19-3.7.9.21"
                                                                      "2)4 of 4)7 15.19-1.7.9.21"]))
                                            (* root 1)
                                            (at-i (range 12))))
              #_(low (scale/deg->freq (get-subscps-scale "2)4 of 4)7 15.19-3.7.9.21")
                                      root
                                      (+ 2 (at-i [4 3 4 5 (at-i [7 2]) 0 -1])))))))
