(ns tieminos.synths.mono
  (:require
   [overtone.core :as o]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]))

(oe/defsynth noistr
  [freq 200
   amp 0.5
   a 0.01
   d 1
   ladder-ratio 1
   out 0]
  (let [attack (-> (o/white-noise)
                   (o/moog-ff (o/rand 3000))
                   (* amp (o/env-gen (o/env-perc 0 (min 0.2 (+ a d))))))
        body (+ (-> (o/saw freq)
                    (o/moog-ladder (*  freq) 0.3)
                    (* amp 8
                       (lfo-kr 3 0.8 1)
                       (o/env-gen (o/env-perc 0.01 (+ a d -0.01)))))
                (-> (+ (o/sin-osc freq)
                       (* (o/sin-osc-fb (/ freq 2)
                                        (* 2 (o/env-gen (o/env-perc 0 (min (* 2/3 d) (+ a d))))))
                          (o/env-gen (o/env-perc 0 (min 0.7 (+ a d))))))
                    (o/moog-ladder (* ladder-ratio  freq) 0.3)
                    (* amp 8
                       (lfo-kr 3 0.8 1)
                       (o/env-gen (o/env-perc a d)
                                  :action o/FREE))))]
    (o/out out (-> (+ attack body)
                   (* 0.5 (o/amp-comp-a freq))))))

(comment
  (noistr {:freq 500 :d 5})
  (metalstr {:freq 500 :d 1 :amp 5}))

(oe/defsynth metalstr
  [freq 200
   amp 0.5
   a 0.01
   d 1
   ladder-ratio 1
   ladder-reso 0.8
   out 0]
  (let [attack (-> (o/sin-osc (o/line:kr freq (/ freq 4) 0.01))
                   (o/moog-ladder (o/rand 3000))
                   (* amp (o/env-gen (o/env-perc 0 (min 0.2 (+ a d))))))
        body (-> (o/formant (+ (* 0.1 (o/sin-osc 2)) freq)
                            (+ (* 1/2 freq (o/sin-osc (o/line 2 0 (+ a d)) (o/rand 0 3.14)))
                               (* freq 2)))
                 (o/moog-ladder (* 2 ladder-ratio freq) ladder-reso)
                 (* amp 1.5
                    (lfo-kr (o/line:kr (o/rand 6 10) 0 d) 0.8 1)
                    (o/env-gen (o/env-perc a (* 2 d) 1.5)
                               :action o/FREE)))
        sig (+ attack body)]
    (o/out out (-> sig
                   (o/compander sig)
                   (* 0.5 (o/amp-comp-a freq))))))

(oe/defsynth low
  [freq 85
   amp 0.5
   mod-freq 8300
   a 0.01
   d 1
   out 0]
  (o/out out (-> (o/range-lin (o/pulse mod-freq) (- freq 15) (+ freq 15))
                 o/sin-osc
                 (* (o/env-gen (o/env-perc a d) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(oe/defsynth sharp-plate
  [freq 350
   amp 1
   mod-freq 300
   lpf 2000
   a 0.01
   d 0.5
   out 0]
  (o/out out (-> (o/range-lin (o/saw mod-freq) (- freq 350) (+ freq 350))
                 o/sin-osc
                 (o/lpf lpf)
                 (* (o/env-gen (o/env-perc a d) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(oe/defsynth bd
  [freq 80
   amp 0.5
   dur 1
   out 0]
  (o/out out
         (let [sig (o/sin-osc (o/line:kr 15000 freq 0.001))]
           (-> (+ sig (o/moog-ladder sig))
               (* amp
                  (o/amp-comp-a freq)
                  (o/env-gen (o/env-perc 0 dur)
                             :action o/FREE))))))
(oe/defsynth hh
  [d 0.1
   amp 0.5
   hpf 500
   ff-freq 16000
   out 0]

  (o/out out
         (-> (o/white-noise)
             (o/moog-ff ff-freq)
             (o/hpf hpf)
             (* amp (o/env-gen (o/env-perc 0 d)
                               :action o/FREE)))))
(comment
  (require '[tieminos.utils :refer [rrange]])
  (hh {:ff-freq (rrange 800 3000)
       :d 0.2}))

(oe/defsynth snare
  [d 2
   noise-curve 0.3
   amp 0.5
   hpf 500
   freq 300
   ladder-freq 20000
   ladder-res 0.5
   ff-pregain 1
   ff-gain 0.0
   ff-freq 800
   out 0]
  (o/out out
         (-> (+ (* (o/white-noise)
                   (o/env-gen (o/env-perc 0 d noise-curve)))
                (o/sin-osc (o/line 1500 freq 0.01)))
             (o/moog-ladder ladder-freq ladder-res)
             (* ff-pregain)
             (o/moog-ff ff-freq ff-gain)
             (o/hpf hpf)
             (* amp 5 (o/env-gen (o/env-perc 0 d)
                                 :action o/FREE)))))
(comment
  ;; WIP make this the defaults
  (snare {:d 1
          :ladder-freq 20000
          :ladder-res 0.5
          :noise-curve 0.3
          :freq 150
          :ff-freq 16000
          :ff-pregain 1
          :ff-gain 0.1})
  (snare {:d 1
          :freq 150
          :ladder-freq 300
          :ladder-res 1
          :ff-freq 900
          :ff-gain 0.5
          :hpf 300
          :amp 7}))

(comment
  (require '[tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.wavetable :refer [available-dirs akwf-samples]])
  (def akwf (apply akwf-samples (take 5 (available-dirs))))
  (available-dirs)
  (-> akwf keys)
  (mono-mooglad
   (merge (akwf :bw-sawrounded)
          {:amp 0.3
           :range 0.2
           :offset 0.5
           :freq 560
           :out 8}))
  mono-mooglad)

(oe/defsynth mono-mooglad
  ;; A wavetable synth
  [freq 220
   min-wave 0 ;; supplied by the `akwf` function
   max-wave 1 ;; supplied by the `akwf` function
   offset 0
   range 1
   a 0.01
   s 0.5
   s-amp 1
   d-amp 1
   r 0.49
   amp 0.5
   ladder-freq 20000
   ladder-res 0.5
   out 0]
  (let [env (o/env-gen (o/envelope [0 s-amp d-amp 0]
                                   [a s r]
                                   0.1)
                       :action o/FREE)
        dur (+ a s r)
        wave-range (- max-wave min-wave)
        min-wave* (+ min-wave (* offset wave-range))
        max-wave* (+ min-wave* (* range wave-range))
        sig (-> (o/v-osc (o/line:kr (o/clip min-wave* min-wave max-wave)
                                    (o/clip max-wave* min-wave max-wave)
                                    dur)
                         freq)
                (o/moog-ladder ladder-freq ladder-res))]
    (o/out out (-> sig
                   (* amp env)))))
