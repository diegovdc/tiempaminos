(ns tieminos.harmonic-experience.drones.sounds
  (:require
   [overtone.core :as o]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]))

(o/defsynth drone2
  [freq 130
   amp 1
   gate 1]
  (o/out 0
         (o/saw freq)))

(o/defsynth drone ;; sine
  [freq 130
   amp 1
   gate 1]
  (o/out 0
         (-> (map #(* (lfo (o/n-rand 0.5 1.2) 0.2 0.6)
                      (o/sin-osc (* % freq)))
                  [1 2 3 4 5 6 7 8 9 11 13 15])
             (o/mix)
             (o/pan2 (lfo 0.4 -0.5 0.5))
             #_(o/hpf 700)
             (#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
             (* amp 0.7 (lfo 0.437 0.8 1) (o/env-gen (o/env-asr 5 1 5 0.1) :gate gate :action o/FREE)))))

(o/defsynth harmonic
  [freq 130
   amp 1
   a 2
   s 1
   r 2
   gate 1]
  (o/out 0
         (-> (* 0.7 #_(lfo 0.6 0.2 0.6) (o/mix (o/sin-osc
                                                [freq
                                                 (* 2 freq)
                                                 (* 4 freq)
                                                 (* 5 freq)] 0
                                                [1 0.8 0.6 0.5])))
             (o/pan2 #_(lfo 0.4 -0.5 0.5))
             #_(o/hpf 700)
             #_(#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
             (* amp (o/env-gen (o/env-asr a s r) :gate gate :action o/FREE)))))

(o/defsynth sine
  [freq 130
   amp 1
   a 2
   s 1
   r 2
   gate 1]
  (o/out 0
         (-> (* 0.7 (lfo 0.6 0.2 0.6) (o/mix (o/sin-osc [freq freq])))
             (o/pan2 (lfo 0.4 -0.5 0.5))
             #_(o/hpf 700)
             (#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
             (* amp (o/env-gen (o/env-asr a s r) :gate gate :action o/FREE)))))
