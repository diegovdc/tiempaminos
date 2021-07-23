(ns erv-fib-synth.synths
  (:require [overtone.core :refer :all :as o]))

;;;;;;;;;;;;;;;;
;; Percussion ;;
;;;;;;;;;;;;;;;;


(defsynth low
  [freq 85
   amp 1
   mod-freq 8300
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (range-lin (pulse mod-freq) (- freq 15) (+ freq 15))
                 sin-osc
                 (pan2 pan)
                 (* (env-gen (env-perc atk dcy) :action FREE))
                 (* amp))))

(comment (low))

(defsynth short-plate
  [freq 200
   amp 1
   mod-freq 1000
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (range-lin (pulse mod-freq) (- freq 200) (+ freq 200))
                 sin-osc
                 (pan2 pan)
                 (* (env-gen (env-perc atk dcy) :action FREE))
                 (* amp))))

(comment (short-plate))

(defsynth short-plate2
  "A soft version"
  [freq 200
   amp 1
   mod-freq 8300
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (range-lin (pulse mod-freq) (- freq 200) (+ freq 200))
                 sin-osc
                 (pan2 pan)
                 (* (env-gen (env-perc atk dcy) :action FREE))
                 (* amp))))

(comment (short-plate2))

(defsynth sharp-plate
  [freq 350
   amp 1
   mod-freq 300
   pan 0
   atk 0.01
   dcy 0.5
   out 0]
  (o/out out (-> (range-lin (saw mod-freq) (- freq 350) (+ freq 350))
                 sin-osc
                 (pan2 pan)
                 (* (env-gen (env-perc atk dcy) :action FREE))
                 (* amp))))

(comment (sharp-plate))




;;;;;;;;;;;
;;; melodic (they have gate)


(defsynth low2
  [freq 85
   amp 1
   mod-freq 8300
   pan 0
   atk 0.01
   dcy 0.7
   sust 0.3
   rel 0.5
   gate 1
   out 0]
  (o/out out (-> (range-lin (pulse mod-freq) (- freq 15) (+ freq 15))
                 sin-osc
                 (pan2 pan)
                 (* (env-gen (env-adsr atk dcy sust rel) :gate gate :action FREE))
                 (* amp))))

(comment
  (def l (low2))
  (ctl l :gate 0))



(defsynth sharp-plate2
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
  (o/out out (-> (range-lin (+ (* (saw (/  mod-freq 7.5))
                                  (sin-osc (/  freq 2))
                                  mod-amp2
                                  0.1)
                               (* (lf-tri mod-freq) 0.3 mod-amp2))
                            (- freq mod-amp) (+ freq mod-amp))
                 sin-osc
                 (pan2 pan)
                 (* (env-gen (env-adsr atk dcy sust rel :curve :sin) :gate gate :action FREE))
                 (* amp))))
