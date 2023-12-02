(ns tieminos.impros.bp-kb-delay
(:require
   [erv.utils.conversions :refer [midi->cps]]
   [overtone.core :as o]
   [tieminos.math.utils :refer [linexp*]]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [tieminos.synths :refer [soft-saw2]]))


(oe/defsynth bp-delay
  [in 0
   out 0
   delay-time 1/5
   bp-freq 1000
   amp 1]
  (o/out out (-> (o/in in 1)
                 (o/comb-n 1 delay-time 10)
                 o/leak-dc
                 (o/bpf #_(lfo0-kr (* 3 delay-time) 200 3000)
                        (o/lin-lin:kr (o/lf-noise0:kr (* 3 delay-time)) -1 1 200 3000)
                        0.1)
                 (o/free-verb 0.7 10 0)
                 (* amp (lfo 0.1 1 2))
                 (o/pan4 (lfo 1 -1 1) (lfo 1 -1 1)))))

(comment
  (-> oxygen)
  (def delay-in (o/audio-bus 1 "delay-in"))
  (def delay-in2 (o/audio-bus 1 "delay-in2"))
  (def delay-out (o/audio-bus 1 "delay-out"))
  (o/stop)

  (groups/init-groups!)

  (oe/defsynth pan-out
    [in 0]
    (o/out 0 (o/pan2 (o/in in 1) 0)))

  (o/kill delay1 delay2 delay3)

 (do #_(def pan1 (pan-out {:group (groups/late)
                          :in delay-out}))

      (def delay1 (bp-delay {:group (groups/mid)
                             :in delay-in2
                             :bp-freq 1000
                             :delay-time 1/2
                             :amp 2}))
      (def delay2 (bp-delay {:group (groups/mid)
                             :in delay-in2
                             :bp-freq 2000
                             :delay-time 1
                             :amp 2}))
      (def delay3 (bp-delay {:group (groups/mid)
                             :in delay-in2
                             :bp-freq 3000
                             :delay-time 1/3
                             :amp 2})))
 (o/ctl delay3 :amp 2)
  (o/ctl pan1 :in delay-out)

  (def oxygen (get-oxygen!))
  (midi-in-event
    :midi-input oxygen
    :note-on (fn [{:keys [note velocity]}]
               (soft-saw2 (groups/early)
                          :freq (midi->cps note)
                          :amp (linexp* 0 127 0.5 1 velocity)
                          :out delay-in2
                          ))))
