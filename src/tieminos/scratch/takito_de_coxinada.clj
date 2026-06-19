(ns tieminos.scratch.takito-de-coxinada
  (:require
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [algo-note]]
   [tieminos.osc.reaper :as reaper]
   [tieminos.seq-utils.core :refer [** rainseq]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

(comment
  (/ 160 144)
  (reaper/init)
  (def sink (midi/midi-out "VirMIDI"))
  (rain.v2/stop)

  (rain.v2/stop ::beat10o9)
  (do (rain.v2/stop) (rain.v2/reset) (reaper/stop))
  (rain.v2/ref-rain :id ::clock :durs [7/8] :tempo 160 :ratio 1 :on-event (rain.v2/on-event (when (= 2 i) (reaper/rec))))

  (rain.v2/ref-rain
   :id ::beat10o9
   :ref ::clock
   :durs [1/2]
   :ratio 10/9 ;; 144bpm
   :on-event (rain.v2/on-event
              (when (#{0 1 2} (mod i (rainseq [4 5])))
                (algo-note {:sink sink
                            :dur  0.05
                            :note (into [] (set [45 (rainseq [45 38])]))
                            :vel (rainseq [30 80 [50 30]])}))))

  (rain.v2/ref-rain
   :id ::clave
   :ref ::clock
   :durs [3 2 2]
   :ratio 1/2
   :on-event
   (rain.v2/on-event
    (algo-note {:sink sink
                :dur  0.2
                :note [47 #_(rainseq [51 51 52 53])]
                :vel (int (rainseq  (**
                                     [1 0.95 0.9 0.85 0.8 0.75 0.7 0.85 0.8 0.85 0.9 0.95]
                                     [80 60 70])))}))))
