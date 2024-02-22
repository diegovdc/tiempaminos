(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.synth-explorations
  (:require
   [overtone.core :as o]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :refer [inputs percussion-processes-main-out
                                     preouts]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.sc-utils.ndef.v1 :as ndef :refer [ndef]]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr lfo0-kr]]
   [tieminos.utils :refer [rrange]]))

(comment
  ;; KEEP, makes percussive events
  #_(ndef ::percussive-events
          (let [sig (-> (o/mix [#_(o/in (-> @inputs :guitar :bus))
                                (o/in (-> @inputs :mic-1 :bus))])
                        #_(o/pan4 (lfo-kr 4 -1 1) (lfo-kr 3 -1 1))
                        #_o/distort)]
            (o/mix (mapv (fn [i]
                           (-> sig
                               (* 2 #_(o/sin-osc:kr (lfo0-kr 3 0.1 2)
                                                    (lfo0-kr 100 0.1 2) :mul 2)
                                  (o/pulse (lfo0-kr 3 0.1 2)))
                               (o/bpf (lfo0-kr 3 80 1400) 0.1)
                               (o/free-verb (lfo-kr 1 0 1)
                                            (lfo-kr 1 0.5 1)
                                            0)
                               (o/pan4 (lfo-kr 2 -1 1) (lfo-kr 2 -1 1))
                               (* (lfo-kr 10 0 1) 20)
                             ;; NOTE optional
                               #_(o/free-verb (lfo-kr 1 0 1)
                                              (lfo-kr 1 0.5 1)
                                              0)))
                         (range 10))))
          {:out percussion-processes-main-out})

  ;; KEEP, a nice spectralish-resonant distortion
  #_(ndef ::spectral-distortion
      ;; has some "optional" delay, see `o/mix` below
          (let [sig (o/in (-> @inputs :mic-1 :bus))
                ps-sig (+ (* (lfo-kr 0.4 0.1 0.4) (o/pitch-shift sig 0.1 3/2))
                          (* (lfo-kr 0.5 0.1 0.4) (o/pitch-shift sig 0.1 5/4))
                          (* (lfo-kr 0.5 0.1 0.4) (o/pitch-shift sig 0.2 11/4)))
                dist-sig (-> ps-sig
                             (o/b-low-shelf 200 1 -60)
                             (o/b-hi-shelf 10000 1 -12)
                             (o/disintegrator (lfo-kr 2 0 1) 2)
                             (o/b-hi-shelf 10000 1 12)
                             #_(o/bpf 1000 0.04)
                             (* 0.6)
                             #_(o/free-verb 1 (lfo-kr 1 0.1 10) 0))]
        ;; Both elements of this mix can be played on their own or mixed
            (o/mix [;; Original effected signal
                    #_(o/pan4 dist-sig (lfo-kr 2 -1 1) (lfo-kr 2 -1 1))
                ;; Delays the signal by up to 1s, sounds good on it's own
                    (-> dist-sig
                        (o/delay-l 1 (lfo-kr 1 0.1 1))
                        (o/free-verb 1 (lfo-kr 1 1 10) 0.5)
                        (o/pan4 (lfo-kr 2 -1 1) (lfo-kr 2 -1 1))
                        #_(* 0.5))]))
          {:out percussion-processes-main-out})
  (ndef ::exploration
        (let [sig (o/in (o/mix [(-> @inputs :mic-1 :bus)
                                (-> @inputs :guitar :bus)]))
              ps-sig (+ (* (lfo-kr 0.4 0.1 0.4) (o/pitch-shift sig 0.1 3/2))
                        (* (lfo-kr 0.5 0.1 0.4) (o/pitch-shift sig 0.1 5/4))
                        (* (lfo-kr 0.5 0.1 0.4) (o/pitch-shift sig 0.2 11/4)))
              dist-sig (-> ps-sig
                           (o/b-low-shelf 200 1 -60)
                           (o/b-hi-shelf 10000 1 -12)
                           (o/disintegrator (lfo-kr 2 0 1) 2)
                           (o/b-hi-shelf 10000 1 12)
                           #_(o/bpf 1000 0.04)
                           (* 0.6)
                           #_(o/free-verb 1 (lfo-kr 1 0.1 10) 0))]
        ;; Both elements of this mix can be played on their own or mixed
          (o/mix [;; Original effected signal
                  (o/pan4 dist-sig (lfo-kr 2 -1 1) (lfo-kr 2 -1 1))
                ;; Delays the signal by up to 1s, sounds good on it's own
                  (-> dist-sig
                      (o/delay-l 1 (lfo-kr 1 0.1 1))
                      (o/free-verb 1 (lfo-kr 1 1 10) 0.5)
                      (o/pan4 (lfo-kr 2 -1 1) (lfo-kr 2 -1 1))
                      #_(* 0.5))]))
        {:out percussion-processes-main-out})

  (ndef/stop ::exploration)

  (when @habitat-initialized?
    (main/stop-sequencer! hseq/context)
    (reset! rec/recording? {})
    (reset! rec/bufs {}))
  (init!)
  (-> percussion-processes-main-out)
  (open-inputs-with-rand-pan
   {:inputs inputs
    :preouts preouts}))
