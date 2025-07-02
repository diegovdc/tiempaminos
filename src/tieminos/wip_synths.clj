(ns tieminos.wip-synths
  (:require
   [overtone.core :as o]
   [tieminos.overtone-extensions :refer [defsynth]]))

(defsynth dbass [freq 440 amp 1 a 0.05 r 0.5 pan-speed 0 out 0]
  (o/out out (-> [(o/sin-osc freq)
                  (o/sin-osc (* 2 freq))
                  (o/sin-osc (* 3 freq))]
                 o/mix
                 (o/lpf 2500)
                 (o/pan2 (o/lf-noise1 pan-speed))
                 (* amp (o/env-gen (o/env-perc a (* 0.5 r)) :action o/FREE)))))

(comment
  ;; Mapping pan-az to different channels
  (let [s (o/synth
           (map
            (fn [i sig]
              (o/out i sig))
            [0 1 2 3 0 1 1 0]
            (o/pan-az 8 (o/sin-osc) (o/line 0 13/8 2 o/FREE))))]
    (s)))
