(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.amp-trig
  (:require
   [overtone.core :as o]
   [tieminos.habitat.amp-trigger :as amp-trig :refer [reg-amp-trigger]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :refer [inputs]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :as scu]))

(comment
  (when @habitat-initialized?
    (main/stop-sequencer! hseq/context)
    (reset! rec/recording? {})
    (reset! rec/bufs {})
    (reset! amp-trig/handlers {}))
  (init!)

  (synth)
  (do
    (oe/defsynth synth
      [in 0
       ps1 1
       ps2 1
       rz-freq 200]
      (o/out 0 (let [sig (o/in in 1)
                     sig2 (+ (-> sig
                            (o/pitch-shift 0.1 ps1)
                            (* 0.8)
                            (#(o/pan-az 4 % (scu/lfo-kr 0.5 -1 1))))
                        (-> sig
                            (o/pitch-shift 0.1 ps2 0 0.0001)
                            (* 0.5)
                            (#(o/pan-az 4 % (scu/lfo-kr 0.5 -1 1))))
                        (-> sig
                            (o/ringz rz-freq 0.1)
                            (* 0.05)
                            (#(o/pan-az 4 % (scu/lfo-kr (scu/lfo-kr 2 1 5) -1 1)))))]
                 (-> (+ sig2
                        (-> sig2
                            (o/comb-l 0.2 (scu/lfo-kr 1 0.15 0.2) 1)
                            (o/pitch-shift 0.1 ps2 0 0.0001)
                            (* (o/env-gen (o/env-perc 2 1  0.2 0.7)))))

                     (* (o/env-gen (o/envelope [0 1 0.7 0.5 0] [0.3 3 2 5])
                                   ;; :time-scale (/ 2 10.3)
                                   :action o/FREE) )
                     (o/free-verb (scu/lfo-kr 2 0.4 0.7) 2 0)))))

    (defn amp-trig-handler
      [{:keys [in]}]
      (println (rand))
      (synth {:group (groups/mid)
              :in in
              :ps1 3/2
              :ps2 (rand-nth [1/4 11/4 #_7/2 #_13/4])
              :rz-freq (* 200 (rand-nth [1 2 3 4 5]))})))

  (reg-amp-trigger {:in (-> @inputs :mic-1 :bus)
                    :handler #'amp-trig-handler})
  (swap! amp-trig/handlers dissoc 312)
  (amp-trig/dereg-handler {:id 312})


  )
