(ns tieminos.synths
  (:require
   [overtone.core :as o]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]))

;;;;;;;;;;;
;; Basic ;;
;;;;;;;;;;;
(o/defsynth soft-saw
  [freq 200
   amp 0.5
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out
         (-> (o/saw freq)
             (o/lpf 2000)
             (o/pan2 pan)
             (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
             (* amp (o/amp-comp-a freq)))))

;;;;;;;;;;;;;;;;
;; Percussion ;;
;;;;;;;;;;;;;;;;

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
                 (o/pan2 pan)
                 (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(comment (low))

(o/defsynth short-plate
  [freq 200
   amp 0.5
   mod-freq 1000
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (o/range-lin (o/pulse mod-freq) (- freq 200) (+ freq 200))
                 o/sin-osc
                 (o/pan2 pan)
                 (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(comment (short-plate))

(o/defsynth short-plate2
  ;; "A soft version"
  [freq 200
   amp 1
   mod-freq 8300
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (o/range-lin (o/pulse mod-freq) (- freq 200) (+ freq 200))
                 o/sin-osc
                 (o/pan2 pan)
                 (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                 (* amp))))

(comment (short-plate2))

(o/defsynth sharp-plate
  [freq 350
   amp 1
   mod-freq 300
   pan 0
   atk 0.01
   dcy 0.5
   out 0]
  (o/out out (-> (o/range-lin (o/saw mod-freq) (- freq 350) (+ freq 350))
                 o/sin-osc
                 (o/pan2 pan)
                 (o/lpf 2000)
                 (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(comment (sharp-plate))

;;;;;;;;;;;
;;; melodic (they have gate)

(o/defsynth soft-saw2
  [freq 200
   amp 0.5
   pan 0
   atk 0.01
   dcy 1
   sust 0.3
   rel 0.5
   gate 1
   out 0]
  (o/out out
         (-> (o/saw freq)
             (o/lpf 2000)
             (o/pan2 pan)
             (* (o/env-gen (o/env-adsr atk dcy sust rel)
                           :gate gate :action o/FREE))
             (* amp (o/amp-comp-a freq)))))

(o/defsynth low2
  [freq 85
   amp 1
   mod-amp 1
   mod-amp-end 0.1
   mod-dur 1
   mod-freq 8300
   mod-freq-range 30
   pan 0
   atk 0.01
   dcy 0.7
   sust 0.3
   rel 0.5
   gate 1
   out 0]
  (o/out out (-> (o/range-lin (* (o/env-gen
                                  (o/envelope [mod-amp mod-amp-end]
                                              [mod-dur]))
                                 (o/pulse mod-freq))
                              (- freq (/ mod-freq-range 2))
                              (+ freq (/ mod-freq-range 2)))
                 o/sin-osc
                 (o/pan2 pan)
                 (* (o/env-gen (o/env-adsr atk dcy sust rel)
                               :gate gate :action o/FREE))
                 (* amp))))

(comment
  (def l (low2))
  (o/ctl l :gate 0))

(o/defsynth sharp-plate2
  [freq 350
   amp 1
   mod-freq 300
   mod-amp 50
   mod-amp2 1
   pan 0
   atk 0.01
   dcy 0.7
   sust 0.3
   rel 0.5
   gate 1
   out 0]
  (o/out out (-> (o/range-lin (+ (* (o/saw (/  mod-freq 7.5))
                                    (o/sin-osc (/  freq 2))
                                    mod-amp2
                                    0.1)
                                 (* (o/lf-tri mod-freq) 0.3 mod-amp2))
                              (- freq mod-amp) (+ freq mod-amp))
                 o/sin-osc
                 (o/pan2 pan)
                 (* (o/env-gen
                     (o/env-adsr atk dcy sust rel :curve :sin)
                     :gate gate :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(o/defsynth noise-tone
  [freq 200
   bwr 0.01
   pan 0
   out 0
   amp 1]
  (o/out out
         (-> (o/white-noise)
             (o/resonz freq bwr)
             (o/lpf (* 1.1 freq))
             (* 120 amp (o/env-gen (o/env-perc) :action o/FREE))
             (o/pan2 pan))))

(comment
  (noise-tone :amp 1)
  (o/stop))

(oe/defsynth hh2
  [freq 100
   amp 1
   pan 0
   atk 0
   dcy 0.5
   perc-curve -5
   lpf-freq 6000
   hpf-freq 6000
   out 0]
  (o/out out (-> (o/sin-osc (* (lfo 0.4 9000 15000)
                               (o/sin-osc (* 7000 (o/sin-osc
                                                   (* 800 (o/pulse freq)))))))
                 (o/pan2 pan)
                 (o/comb-l 0.01 (o/line 0.05 0.02 0.05) 0.4)
                 (o/rhpf hpf-freq 0.4)
                 (o/moog-ff lpf-freq 2.3)
                 (* (o/env-gen (o/env-perc atk dcy :curve perc-curve) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(comment
  (hh2 {:freq 100
        :atk 0
        :hpf-freq 6000
        :dcy (rand-nth [0.5])
        :mod-freq 100}))

(oe/defsynth mooga
      ;; A nice analog sounding synth by Alex Franco Briones.
      ;; Good as a mono synth.
  [freq 100
   amp 0.5
   gate 1
   pan 0
   detuning 0.05
   filter-lfo-rate 0.2
   filter-min-freq 1000
   filter-max-freq 6000
   q-lfo-rate 1
   filter-mode 1
   out 0]
  (let [amount-hz (/ (* detuning 100) freq)
        freq1 (- freq amount-hz)
        freq2 (+ freq amount-hz)
        env (o/env-gen (o/env-adsr 0.01 0.1 0.6 0.1)
                       :gate gate
                       :action o/FREE)
        sig (-> (o/b-moog
                 (o/lf-saw (o/lag [freq1 freq2] 0.075))
                 (o/range-lin (o/sin-osc:kr filter-lfo-rate) filter-min-freq filter-max-freq)
                 (+ 0.1 (* 0.8 (o/sin-osc:kr q-lfo-rate)))
                 filter-mode))]
    (o/out out
           (-> (o/comb-n sig 0.15 [0.1 0.15] 2)
               (* 0.4)
               (+ (* 0.5 sig))
               (* amp env)
               (o/pan2 pan)
               (o/mix)))))

(comment
  (require '[time-time.dynacan.players.gen-poly :as gp])
  ;; wip
  ;; 2 freqs detuning calc
  (do
    (o/stop)

    (mooga)
    (gp/stop)
    (def mooga1 (mooga {:detuning 0.05
                        :freq 400}))

    (gp/ref-rain
     :id :mooga
     :durs [0.2]
     :on-event (gp/on-event

                (o/ctl mooga1
                       :freq (* 300 (at-i [1 6/5 3/2])))))))
