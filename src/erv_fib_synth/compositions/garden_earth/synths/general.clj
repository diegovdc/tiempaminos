(ns erv-fib-synth.compositions.garden-earth.synths.general
  (:require
   [overtone.core :as o]))

(o/defsynth tuning-monitor [freq 440 a 1.5 r 3.5 amp 0.05 out 0]
  (o/out out (* amp (o/env-gen (o/env-perc a r) :action o/FREE)
              (o/mix (o/pan2 (o/lpf (o/sin-osc [freq
                                                (* 2 freq)
                                                (* 3 freq)])
                                    3000)
                             0)))))
