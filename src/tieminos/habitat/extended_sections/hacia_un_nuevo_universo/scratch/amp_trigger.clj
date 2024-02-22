(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.amp-trigger
  (:require
   [overtone.core :as o]
   [overtone.osc :refer [osc-debug]]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :refer [inputs percussion-processes-main-out
                                     preouts]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.osc.core :as osc]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]))

(comment
  (def server (osc/init-server 57110))

  (osc-debug false)

  (ndef/ndef ::trigy
             (* 0.1 (o/sin-osc 100))
             #_(o/poll:ar (o/impulse 10) (abs (lfo 1 -1 1))))

  (o/on-event "/tr"
              (fn [data] (println data))
              ::on-trigy)

  (o/stop)

  (when @habitat-initialized?
    (main/stop-sequencer! hseq/context)
    (reset! rec/recording? {})
    (reset! rec/bufs {}))
  (init!)
  (-> percussion-processes-main-out)
  (open-inputs-with-rand-pan
   {:inputs inputs
    :preouts preouts}))
