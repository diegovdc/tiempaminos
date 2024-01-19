(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.amp-trig
  (:require
   [overtone.core :as o]
   [tieminos.habitat.amp-trigger :as amp-trig :refer [reg-amp-trigger]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :refer [inputs percussion-processes-main-out
                                     preouts]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :as scu]))

(comment
  (when @habitat-initialized?
    (reset! rec/recording? {})
    (main/stop-sequencer! hseq/context)
    (reset! rec/bufs {})
    (reset! amp-trig/handlers {}))
  (init!)

  (do
    ;; NOTE keep, only ps below
    (oe/defsynth synth
      [in 0
       ps1 1
       ps2 1
       rz-freq 200
       out 0]
      (o/out out (let [sig (o/mix [(o/in in 1)
                                   ;; agregado para la toma 2
                                   (o/in (-> @inputs :mic-2 :bus) 1)
                                   ;; agregado para la toma 3, considerar controlar el volumen de la guitarra para que no exceda a la 
                                   (o/in (-> @inputs :guitar :bus) 1)])
                       sig2 (+ #_(-> sig
                                     (o/pitch-shift 0.1 ps1)
                                     (* 0.8)
                                     (#(o/pan-az 4 % (scu/lfo-kr 0.5 -1 1))))
                               (-> sig
                                   (o/pitch-shift 0.1 ps2)
                                   (* 0.5)
                                   (#(o/pan-az 4 % (scu/lfo-kr 0.5 -1 1))))
                               (-> sig
                                   (o/ringz rz-freq 0.1)
                                   (* 0.07)
;;;  TODO add limiter
                                   #_(o/lpf 8000)
                                   (#(o/pan-az 4 % (scu/lfo-kr (scu/lfo-kr 2 1 5) -1 1)))))]
                   (-> (+ sig2
                          #_(-> sig2
                                (o/comb-l 0.2 (scu/lfo-kr 1 0.15 0.2) 1)
                                (o/pitch-shift 0.1 ps2 0 0.0001)
                                (* 0.5 (o/env-gen (o/env-perc 2 1  0.2 0.7)))))

                       (* (o/env-gen (o/envelope [0 1 0.7 0.5 0] [0.3 3 2 5])
                                     :time-scale 1
                                     :action o/FREE) )
                       (o/free-verb (scu/lfo-kr 2 0.4 0.7) 2 0)))))

    (defn amp-trig-handler
      [{:keys [in]}]
      (println (rand))
      (synth {:group (groups/mid)
              :in in
              :ps1 3/2
              :ps2 (rand-nth [1/4 11/32 #_7/2 13/32])
              :rz-freq (* (rand-nth [200 250 300])
                          (rand-nth [1 2 3 4 5]))
              :out percussion-processes-main-out
              })))

  (def a-t (reg-amp-trigger {:in (-> @inputs :mic-1 :bus)
                             :handler #'amp-trig-handler}))
  (-> a-t )
  (o/kill a-t)
  (swap! amp-trig/handlers dissoc 312)
  (amp-trig/dereg-handler {:id 312})


  (open-inputs-with-rand-pan
    {:inputs inputs
     :preouts preouts})
  )
