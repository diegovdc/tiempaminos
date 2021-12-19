(ns tieminos.compositions.garden-earth.fl-grain-1
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :refer [interval->ratio]]
   [overtone.core :as o]
   [tieminos.compositions.garden-earth.base
    :refer [eik
            eik-notes
            on-event
            pitch-class->pr-fingering
            ref-rain
            scale->pr-fingerings
            stop
            subcps]]
   [tieminos.compositions.garden-earth.synths.general
    :refer [tuning-monitor]]
   [tieminos.compositions.garden-earth.synths.granular :as granular]
   [tieminos.compositions.garden-earth.synths.recording :as rec]))

;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;

(do
  (defn replay [buf-key]
    (let [b (@rec/bufs buf-key)
          subcps "1)4 of 3)6 1.9-3.5.7.11"
          scale (-> eik :subcps (get subcps) :scale)]
      (ref-rain
       :id :fl-grain
       :tempo 900
       :ratio 3/2
       :durs [8 8 8 15 8 8 8 15 8 8 15]
       :on-event (on-event
                  (let [root (weighted {0 70  1 30})
                        intervals (weighted {[1] 1 [2 4] 1 [3] 1} #_{[1 10] 20
                                             [3] 10
                                             [4] 10
                                             [-7] 10
                                             [2 5] 30

                                             [-8] 50})
                        start (min 0 (rand 0.5))
                        end (min 1 (+ start (rand 1)))
                        dur* (max 0.05 (rand (weighted {10 70
                                                        3 5
                                                        6 2})))]

                    (vec (for [rate intervals
                               g-dur [1/10 1/19]]
                           (let [rate (interval->ratio scale root rate)]
                             #_(println rate)
                             (granular/grain-lfo
                              {:buf b
                               :dur dur*
                               :trig-rate 100
                               :grain-dur g-dur
                               :rate rate
                               :amp (max 0.1 (rand 2.5))
                               :min-amp 0.8
                               :amp-lfo 0.01
                               :start 0 #_start
                               :end 1 #_ end
                               :mix 1
                               :room 3
                               :damp 0.8
                               :a (* 0.1 dur*)
                               :r (* 0.8 dur*)})))))))))
  #_(stop)
  (comment
    (replay ["A+53" :a])))

(println (scale->pr-fingerings (subcps "2)4 of 3)6 11-1.5.7.9"))
         "\n\n")
(println (scale->pr-fingerings (subcps "2)4 of 3)6 11-1.5.7.9"))
         "\n\n")
(println (scale->pr-fingerings (subcps "3)5 of 3)6 1.5.7.9.11"))
         "\n\n")
(comment
  (stop))

(comment
  (ref-rain
   :id :fl-grain/record
   :durs [15 20 10 3 10]
   :on-event (on-event
              (rec/start-recording
               :bufs-atom rec/bufs
               :buf-key :olips
               :seconds (max 3 (rand 10))
               :msg ""
               :on-end replay))))




(comment
  (tuning-monitor
   (-> eik-notes (get "C+20") :bounded-ratio (* 440))
   25 25
   ))
