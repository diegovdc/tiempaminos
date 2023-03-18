(ns tieminos.habitat.synths.convolution
  (:require
   [overtone.core :as o]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.panners :refer [current-panners panner stop-panner!]]
   [tieminos.habitat.recording :as rec :refer [norm-amp]]
   [tieminos.habitat.routing
    :refer [inputs special-inputs texto-sonoro-rand-mixer-bus]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]))

(oe/defsynth live-convolver
  [in1 0
   in1-amp 1
   in2 0
   in2-amp 1
   amp 1
   release 5
   gate 1
   rev-mix 1
   rev-room 0.2
   out 0]
  (o/out out (-> (+ (o/convolution (* in1-amp (o/in in1))
                                   (* in2-amp (o/in in2))
                                   4096))
                 #_(o/rhpf 500 0.5)
                 (o/free-verb rev-mix rev-room)
                 (#(+ %
                      (let [bpf-sig (o/bpf % (lfo 1 100 3400) (lfo 0.3 0.03 0.5))]
                        (+ (* 0.8 bpf-sig)
                           (* 1.2 (o/free-verb bpf-sig 0.8 1))))))
                 (* 2 amp (o/env-gen (o/env-adsr 5 1 1 release :curve [1 -0.5 -0.5])
                                     gate
                                     :action o/FREE))
                 (o/limiter 0.9 0.005)
                 #_(o/lpf 5000))))

(oe/defsynth live-convolver-perc
  ;; `perc` instead of `asr`
  [in1 0
   in1-amp 1
   delay 0
   in2 0
   in2-amp 1
   amp 1
   amp-lfo-freq 0.1
   amp-lfo-min 1
   amp-lfo-max 1
   a 5
   r 5
   curve -4.0
   max-amp 0.9
   bpf-amp 0.8
   bpf-rev-amp 1.5
   hpf-freq 500
   lpf-freq 20000
   rev-mix 1
   out 0]
  (o/out out
         (-> (+ (o/convolution (* in1-amp (o/in in1))
                               (* in2-amp (o/in in2))
                               4096))
             (o/hpf hpf-freq)
             (o/free-verb rev-mix 0.2)
             (#(+ %
                  (let [bpf-sig (o/bpf % (lfo 1 100 3400) (lfo 0.3 0.03 0.5))]
                    (+ (* bpf-amp bpf-sig)
                       (* bpf-rev-amp (o/free-verb bpf-sig 0.8 1))))))
             (o/lpf lpf-freq)
             (* 2 amp
                (lfo amp-lfo-freq amp-lfo-min amp-lfo-max)
                (o/env-gen (o/env-perc a r 1 curve)))
             (o/limiter max-amp 0.01)
             (o/delay-n delay delay)
             (* (o/env-gen (o/envelope [0 1 1 0] [0.001 (+ a r delay) 0.001])
                           :action o/FREE)))))

(comment
  (def texto-sonoro-rand-mixer-bus (o/audio-bus 1 "texto-sonoro-rand-mixer-bus"))
  (oe/defsynth texto-sonoro-rand-mixer
    [in 0
     out 0]
    (o/out out
           (o/select (lfo 1 0 3)
                     (o/sound-in (map #(+ % in) (range 4))))))
  (def ts (texto-sonoro-rand-mixer
           {:in (-> special-inputs :texto-sonoro :in)
            :out texto-sonoro-rand-mixer-bus}))
  (o/kill ts)

  ;;  TODO left here, need to send to the live-convolver and test mixing with some other signals
  (o/demo (o/in texto-sonoro-rand-mixer-bus 1))
  (o/demo (o/in (-> inputs :guitar :bus)  1))
  (o/kill texto-sonoro)

  (def lc-out-bus (o/audio-bus 1 "lc-out-bus"))
  (def lc (live-convolver {:group (groups/mid)
                           :in1 (-> inputs :guitar :bus)
                           :in1-amp 2
                           :in2 texto-sonoro-rand-mixer-bus
                           :in2-amp 1
                           :out lc-out-bus}))
  (o/kill lc)
  (o/ctl lc :gate 0)
  (o/ctl lc :amp 0.8)
  (o/ctl lc :in1-amp 1)
  (o/ctl lc :in2-amp 2)
  (o/ctl lc
         :in1 texto-sonoro-rand-mixer-bus
         :in2 (-> inputs :guitar :bus))

  (oe/defsynth input*
    [in 0 out 0]
    (o/out out (o/in in 1)))
  (def g (input* {:out lc-out-bus
                  :in (-> inputs :guitar :bus)}))
  (o/ctl g :out 0)
  (o/kill g)

  (def lc-out (input* {:in lc-out-bus}))
  (o/kill lc-out)
  (def lc-panner (panner {:in #_texto-sonoro-rand-mixer-bus
                          lc-out-bus #_(-> inputs :guitar :bus)
                          :type :rand
                          :out 0}))
  (stop-panner! lc-out-bus)
  (-> @current-panners)
  (stop-panner! texto-sonoro-rand-mixer-bus)
  (stop-panner! (-> inputs :guitar :bus))
  (o/kill lc-panner)
  (-> special-inputs :texto-sonoro))

(comment
  (require '[tieminos.habitat.routing :refer [guitar-bus
                                              mic-1-bus
                                              mic-2-bus
                                              mic-3-bus
                                              mic-4-bus
                                              preouts]]))

(comment
  ;; WIP and model for other convolvers
  (do
    (declare c)
    (try (o/kill c) (catch Exception _ nil))
    (oe/defsynth convolver
      [buf-1 0
       buf-1-amp 0.5
       buf-2 0
       buf-2-amp 0.5]
      (o/out 0 (-> (+ (o/convolution (* buf-1-amp (lfo 0.2 0.1 0.4)
                                        (o/play-buf 1 buf-1
                                                    :rate 1
                                                    :loop true
                                                    ;; :start-pos (lfo 0.4 1000 50000)
                                                    ;; :trigger (o/dust 0.11)
                                                    ))
                                     (* buf-2-amp (lfo 0.2 0.1 0.3)
                                        (o/play-buf 1 buf-2
                                                    :start-pos (lfo 0.4 1000 50000)
                                                    :trigger (o/dust 0.1)
                                                    :rate 1
                                                    :loop true))
                                     4096))
                   #_(o/rhpf 500 0.5)
                   (o/free-verb 1 0.2)
                   (#(+ %
                        (let [bpf-sig (o/bpf % (lfo 1 100 3400) (lfo 0.3 0.03 0.5))]
                          (+ (* 0.8 bpf-sig)
                             (* 1.2 (o/free-verb bpf-sig 0.8 1))))))
                   (* 2)
                   #_(o/lpf 5000)
                   (o/pan4 (o/lf-noise1 0.3)
                           (o/lf-noise1 0.3)))))
    (let [buf-keys (-> @rec/test-samples keys sort)
          buf-key (rand-nth buf-keys) #_:amanecer-pt1-mic-2-1
          buf-key2 (rand-nth buf-keys) #_:amanecer-pt1-mic-3-2
          _ (println buf-key buf-key2)
          buf-1       (-> @rec/test-samples buf-key)
          buf-2  (-> @rec/test-samples buf-key2)]
      (def c (convolver
              {:buf-1 buf-1
               :buf-1-amp (* 0.8 (norm-amp buf-1))
               :buf-2 buf-2
               :buf-2-amp (* 0.8 (norm-amp buf-2))}))))

  (-> @rec/test-samples keys))
