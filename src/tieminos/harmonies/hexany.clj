(ns tieminos.harmonies.hexany
  (:require [clojure.string :as str]
            [tieminos.midi.core :refer [note-on]]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [erv.utils.conversions :as conv]
            [overtone.core :as o]
            [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
            [time-time.standard :refer [wrap-at]]))



(do
  (defn period [seconds durs]
    (let [ratio (/ seconds (apply + durs))]
      (mapv #(* ratio %) durs)))
  (period 1 [1 2])
  (defn periods [seconds & durs]
    (mapcat (partial period seconds) durs))
  (periods 1 [1] [1]))


(do
  (o/defsynth h1 [freq 400
                  amp 0.5]
    (o/out 0
           (-> (+ (o/sin-osc freq)
                  (* 0.5 (o/sin-osc (* 3 freq)))
                  (* (o/lf-tri freq) (o/saw freq))
                  #_(* 0.7 (o/pulse freq) (o/saw freq) (o/env-gen (o/env-perc 0.7 0.01))))
               (o/lpf 7000)
               (o/pan2)
               (* 1/10 amp (o/env-gen (o/env-perc 0.1 1) :action o/FREE)))))
  #_(h1))

(defn degs->freq [scale degs]
  (map #(scale/deg->freq scale 200 %) degs))

(defn gen-chord [scale [gens degs]]
  (let [scale** (->> scale (#(cps/filter-scale % gens)))]
    (map (partial scale/deg->freq scale** 200) degs)))
(map gen-chord [[#{3} [0 1 2]]])

(->> (map gen-chord [[#{1 5} [0 1 2 10]]
                     [#{1 3} [0 1 2 4 -9]]
                     [#{3} [0 1 2 4 -7]]
                     [#{3 5} [0 1 2 3 11 4]]
                     [#{5} [0 1 2 4 6 9]]
                     [#{3} [3 1 2 -1 8]]
                     [#{3 5} [3 1 2 6 7 11 -10]]
                     [#{5} [4 3 2 0 6]]
                     [#{3} [0 1 2 3 6 10]]
                     [#{7} [0 1 2 3 7]]
                     [#{1 7} [0 1 2 3]]])
     (map sort)
     (map #(map conv/cps->name %)))
(comment
  (gp/stop)

  (let [scale* (->> [1 3 5 7]
                    (cps/->cps 3)       ;; 2 y 3 funcionan muy bien
                    cps/set->maps
                    (cps/bound-ratio 2)
                    (cps/maps->data :bounded-ratio)
                    :scale)
        harmonies (map (partial degs->freq scale*)
                       [[0 4 5 9 11]
                        [1 4 5]
                        [1 2 3]
                        [0 3 5]])
        harmonies2 (mapv (partial gen-chord scale*)
                         [[#{1 5} [0 1 2 10]]
                          [#{1 3} [0 1 2 4 -9]]
                          [#{3} [0 1 2 4 -7]]
                          [#{3 5} [0 1 2 3 11 4]]
                          [#{5} [0 1 2 4 6 9]]
                          [#{3} [3 1 2 -1 8]]
                          [#{3 5} [3 1 2 6 7 11 -10]]
                          [#{5} [4 3 2 0 6]]
                          [#{3} [0 1 2 3 6 10]]
                          [#{7} [0 1 2 3 7]]
                          [#{1 7} [0 1 2 3]]])]

    (ref-rain :id ::b
              :durs (periods 2.7
                             [3 1 2 1 1 1 2 1/2 1/2 1 2]
                             [1 3 2 1 1 1 2 1 2 1]
                             )
              :on-event (on-event
                         (case (wrap-at index [0 1 1])
                           0 (mapv h1 (wrap-at index harmonies))
                           1 (mapv #(h1 % (wrap-at index [0.7 0.9 0.5]))
                                   (wrap-at index harmonies2))
                           nil)))))
