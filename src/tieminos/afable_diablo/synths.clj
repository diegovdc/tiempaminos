(ns tieminos.afable-diablo.synths
  (:require
   [overtone.core :as o]
   [tieminos.overtone-extensions :refer [defsynth]]))

(defsynth sini [freq 440 amp 1 a 0.05 r 0.5 pan 0 out 0]
  (o/out out (-> [(o/sin-osc freq)
                  (o/sin-osc (* 2 freq))
                  (o/sin-osc (* 3 freq))]
                 o/mix
                 (o/lpf 2500)
                 (o/pan2 pan)
                 (* amp (o/env-gen (o/env-perc a (* 0.5 r)) :action o/FREE)))))
