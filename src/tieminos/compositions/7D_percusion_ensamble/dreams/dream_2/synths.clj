(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.synths
  (:require
   [overtone.core :as o]))

(o/defsynth low
  [freq 85
   amp 0.5
   mod-freq 8300
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (o/range-lin (o/pulse mod-freq) (- freq 15) (+ freq 15))
                 o/sin-osc
                 #_(o/pan2 pan)
                 (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))
