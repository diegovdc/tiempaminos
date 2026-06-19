(ns tieminos.impros.21-04-2025
  "Testing new ref-rain...
  Funky jazzy stuff"
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [algo-note]]
   [time-time.dynacan.players.refrain.v2 :as rain2 :refer [cyi? on-event
                                                           ref-rain]]))

(comment
  (rain2/stop)
  (def sink (midi/midi-out "VirMIDI"))
  (ref-rain
   :id :bd
    ;; :reset-cycle? true
   :durs [1 1 1 1 2 1 1 1]
   :cycle-len (* 1/8 4)
   :ratio 1/8
   :on-event (on-event
              (algo-note {:sink sink
                          :dur (* 0.99 dur-s)
                          :note (+ (if (cyi?) 80 (at-i [60 61 62 63 64 66])) (weighted {-20 5 -10 3}))
                          :vel (if (cyi?) 120 80)})
              (when (> (rand) 0.5)
                (algo-note {:sink sink
                            :chan 1
                            :dur (* 0.99 dur-s)
                            :note (if (cyi?) (at-i [80 81 79]) (at-i [60 61 62 63 64 66]))
                            :vel (if (cyi?) 120 70)}))))
  (ref-rain
   :id :hh
   :ref :bd
    ;; :reset-cycle? true
   :durs [1/2 1 1 1 1 2 1 1 1]
   :cycle-len (* 1/8 5)
   :ratio 1/8
   :on-event (on-event

              (algo-note {:sink sink
                          :chan 1
                          :dur (* 0.99 dur-s)
                          :note (if (cyi? 0) 80 (at-i [60 61 62 63 64 66]))
                          :vel (min 127 (+ 10 (if (cyi? 1) 100 80)))})))
  (ref-rain
   :id :sd
   :ref :bd
    ;; :reset-cycle? true
   :durs [1 2 1 2 1 2 1]
   :cycle-len (* 1/8 3)
   :ratio 1/8
   :on-event (on-event
              (algo-note {:sink sink
                          :chan 2
                          :dur (* 0.99 dur-s)
                          :note (if (cyi? 4) 80 (at-i [60 61 62 63 64 66]))
                          :vel (min 127 (+ 10 (if (cyi? 2) 120 60)))})))
  (ref-rain
   :id :bass
   :ref :bd
    ;; :reset-cycle? true
   :durs [1 2 1 2 1 2 1]
   :cycle-len (* 1/8 8)
   :ratio 1/8
   :on-event (on-event
              (algo-note {:sink sink
                          :chan 3
                          :dur (* 0.99 dur-s)
                          :note (if (or (cyi? 1)
                                        (cyi? 3)
                                        (cyi? 5))
                                  (at-i [80 78 (at-i [75 81 85 78])])
                                  (at-i [60 61 62 63 64 66]))
                          :vel (min 127 (+ 10 (if (or (cyi? (at-i [0 1]))
                                                      (cyi? (at-i [3 5])))
                                                120
                                                (at-i [60 80 76]))))}))))
