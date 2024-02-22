(ns tieminos.impros.session1
  (:require
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [tieminos.osc-midi-receiver :refer [midi-event]]
   [tieminos.synths :as synth]
   [tieminos.utils :refer [linexp]]))

(def hex1 (:scale (cps/make 2 [1 23 13 5 7])))

(comment
  ;; impro 1
  (midi-event
   :note-on (fn [msg]
              (let [degree (- (:note msg) 36)
                    vel (:vel msg)]
                #_(synth/low2 (scale/deg->freq hex1 200  degree)
                              :atk 0.5
                              :dcy 0.3
                              :sust (linexp  1 127 0.8 0.01 vel)
                              :rel 2
                              :amp (linexp  1 127 0.01 0.7 vel))))))

(comment
  ;; impro 2
  (midi-event
   :note-on (fn [msg]
              (let [degree (- (:note msg) 52)
                    vel (:velocity msg)]
                (synth/sharp-plate2
                 :freq (scale/deg->freq hex1 200  degree)
                 :mod-freq 200
                 :mod-amp (linexp  1 127 50 200 vel) ;; la toma 1 va de 50-400
                 :atk (linexp  1 127 0.01 0.1 vel)
                 :dcy (linexp  1 127 0.5 0.8 vel)
                 :sust (linexp  1 127 0.3 0.01 vel)
                 :rel (linexp  1 127 3 0.2 vel)
                 :amp (linexp  1 127 0.01 0.15 vel)
                 :mod-amp2 (linexp  1 127 1 0.4 vel))))))
