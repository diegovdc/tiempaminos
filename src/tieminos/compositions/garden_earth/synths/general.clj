(ns tieminos.compositions.garden-earth.synths.general
  (:require
   [overtone.core :as o]))

(o/defsynth tuning-monitor
  [freq 440
   a 1.5
   r 3.5
   amp 0.05
   pan 0
   lpf-freq 2500
   out 0]
  (o/out out
         (-> (o/sin-osc [freq
                         (* 2 freq)
                         (* 3 freq)
                         (* 4 freq)
                         (* 5 freq)])
             (o/lpf lpf-freq)
             (o/pan2 pan)
             o/mix
             (* amp (o/env-gen (o/env-perc a r :curve 0.5)
                               :action o/FREE)))))
