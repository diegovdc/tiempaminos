(ns tieminos.harmonic-experience.drones
  (:require [overtone.core :as o]
            [tieminos.sc-utils.synths.v1 :refer [lfo]]))

(comment
  (defn lfo [freq min* max*]
    (o/lin-lin (o/lf-noise1 freq) -1 1 min* max*))
  (o/defsynth drone
    [freq 130
     amp 1]
    (o/out 0
           (-> (o/saw freq)
               (o/pan2 (lfo 0.4 -0.5 0.5))
               (o/hpf 700)
               (#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
               (* amp 0.7 (o/sin-osc 0.01))
               (+ 0.3))))
  (o/defsynth drone ;; sine
    [freq 130
     amp 1]
    (o/out 0
           (-> (map #(* (lfo 0.6 0.3 0.5) (o/sin-osc (* % freq))) [1 2 3 4 5 6 7 8 9 15])
               (o/mix)
               (o/pan2 (lfo 0.4 -0.5 0.5))
               #_(o/hpf 700)
               (#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
               (* amp 0.7 (o/sin-osc 0.01))
               (+ 0.3))))
  (def sa (drone 110))
  (o/kill sa)
  (def ga (drone (* 3/2 110)))
  (o/kill sa)
  (o/stop))
