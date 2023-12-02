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
  #_(ndef ::exploration
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

  (ndef ::exploration
      (let [sig (o/in (-> @inputs :mic-1 :bus))]
        (o/mix (mapv (fn [_i]
                       (-> sig
                           (* 2 (o/pulse (lfo0-kr 3 0.1 2)))
                           (o/bpf (lfo0-kr 3 80 1400) 0.1)
                           ;; NOTE optional
                           #_(o/free-verb (lfo-kr 1 0 1)
                                          (lfo-kr 1 0 1)
                                          0)
                           (o/pan4 (lfo-kr 2 -1 1) (lfo-kr 2 -1 1))
                           (* (lfo-kr 10 0 1) 20)
                           ;; NOTE optional
                           #_(o/free-verb (lfo-kr 1 0 1)
                                          (lfo-kr 1 0.5 1)
                                          0)))
                     (range 10))))
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
