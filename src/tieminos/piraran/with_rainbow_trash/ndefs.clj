(ns tieminos.piraran.with-rainbow-trash.ndefs
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
  (ndef ::percussive-events
        (let [sig (o/sound-in 2)]
          (-> (o/mix
               (mapv (fn [i]
                       (-> sig
                           (* 2 (o/pulse (lfo0-kr 3 0.1 0.4)
                                         0.8))
                           (o/bpf (lfo0-kr 3 80 1400) 0.1)
                           (o/free-verb (lfo-kr 1 0 1)
                                        (lfo-kr 1 0.5 1)
                                        0)
                           (o/pitch-shift 0.1 (rand-nth [1 7/4 11/4]) 0 0)
                           (o/pan2 (lfo-kr 2 -1 1) (lfo-kr 2 -1 1))
                           (* (lfo0-kr 10 0 1) 10)

                           #_(o/free-verb (lfo0-kr 10 0 1)
                                          (lfo-kr 10 0.5 50)
                                          (lfo-kr 1 0.5 1))))
                     (range 3)))
              (o/distort)
              #_(* 0.5)))
        {:out 1 :fade-time 3})
  (ndef/stop ::percussive-events)

  (ndef ::teresita (o/free-verb (o/sound-in 2) 0.5 0.5 0.7)
        {:out 0})

  (ndef/stop ::teresita)

  (ndef ::teresita2 (-> 2
                        o/sound-in
                        (o/free-verb 0.5 5 0.7)
                        (o/pan2 (lfo-kr 4 -1 1)))
        {:out 0})
  (ndef/stop ::teresita2)

  (ndef ::kali
        (let [sig (o/sound-in 2)]
          (-> (map
               (fn [i]
                 (-> (o/pitch-shift sig 0.1 (* i 2) 2)
                     (o/pan2 (lfo0-kr 2 -1 1))
                     #_(o/free-verb 1 (lfo0-kr 2 0 1))
                     (o/disintegrator (lfo-kr 2 0 1) 2)
                     (* (lfo0-kr 2 0 2))))
               (range 5 9))
              (o/mix)
              (o/free-verb 1 0.5)
              (* 0.5)))
        {:out 1 :fade-time 3})

  (ndef/stop ::kali)

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
