(ns erv-fib-synth.afable-diablo.synths
  (:require
   [erv-fib-synth.overtone-extensions :refer [defsynth]]
   [overtone.core :as o]))

(defsynth sini [freq 440 amp 1 r 0.5 out 0]
  (o/out out (-> [(o/sin-osc freq)
                  (o/sin-osc (* 2 freq))
                  (o/sin-osc (* 3 freq))
                  #_(o/sin-osc (* 5 freq))]
                 o/mix
                 (o/lpf 2500)
                 o/pan2
                 (* amp (o/env-gen (o/env-perc 0.05 (* 0.5 r)) :action o/FREE)))))

(defsynth sini [freq 440 amp 1 r 0.5 out 0]
  (o/out out (-> [(o/sin-osc freq)
                  (o/sin-osc (* 2 freq))
                  (o/sin-osc (* 3 freq))
                  #_(o/sin-osc (* 5 freq))]
                 o/mix
                 (o/lpf 2500)
                 o/pan2
                 (* amp (o/env-gen (o/env-perc 0.05 (* 0.5 r)) :action o/FREE)))))
