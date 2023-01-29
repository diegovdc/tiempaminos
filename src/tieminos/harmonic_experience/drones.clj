(ns tieminos.harmonic-experience.drones
  (:require [overtone.core :as o]
            [erv.utils.conversions :as conv]
            [tieminos.sc-utils.synths.v1 :refer [lfo]]))

(comment
  (/ (conv/midi->cps 63)
     2)
  (defn lfo [freq min* max*]
    (o/lin-lin (o/lf-noise1 freq) -1 1 min* max*))
  (o/defsynth drone
    [freq 130
     amp 1
     gate 1]
    (o/out 0
           (-> (o/saw freq)
               (o/pan2 (lfo 0.4 -0.5 0.5))
               (o/hpf 700)
               (#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
               (* amp 0.7 (o/env-gen (o/env-asr 2 1 2) :gate gate :action o/FREE)))))
  (o/defsynth drone ;; sine
    [freq 130
     amp 1
     gate 1]
    (o/out 0
           (-> (map #(* (lfo 0.6 0.3 0.5) (o/sin-osc (* % freq))) [1 2 3 4 5 6 7 #_16/5 8 9 15])
               (o/mix)
               (o/pan2 (lfo 0.4 -0.5 0.5))
               #_(o/hpf 700)
               (#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
               (* amp 0.7 (o/env-gen (o/env-asr 2 1 2) :gate gate :action o/FREE)))))
  (o/defsynth harmonic
    [freq 130
     amp 1
     gate 1]
    (o/out 0
           (-> (* (lfo 0.6 0.3 0.5) (o/sin-osc freq))
               (o/pan2 (lfo 0.4 -0.5 0.5))
               #_(o/hpf 700)
               (#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
               (* amp 0.7 (o/env-gen (o/env-asr 2 1 2) :gate gate :action o/FREE)))))
  (o/stop)
  (def sa (drone 155))
  (def h (harmonic (* 155 5)))
  (o/ctl sa :gate 0)
  (o/ctl h :gate 0)
  (def ga (drone (* 3/2 155)))
  (o/ctl ga :gate 0)
  (o/stop))
