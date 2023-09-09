(ns tieminos.habitat.scratch.fib-harmonies
  (:require
   [overtone.core :as o]
   [tieminos.habitat.extended-sections.main :refer [fib-chord-seq
                                                    transpose-chord]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(fib-chord-seq (transpose-chord [0 3 7] (range 0 21)))
(comment

  (gp/stop)
  (o/stop)
  (ref-rain
    :id :fib-harmonies
    :durs [5 3 5]
    :tempo 120
    :ratio 1/4
    :on-event
    (on-event
      ((o/synth
         (o/out 1
                (* 0.3
                   (o/env-gen (o/env-perc 2 2) :action o/FREE)
                   (o/pan2:ar
                     (o/mix
                       (o/lpf
                         (o/saw
                           (* 200 (nth
                                    (interleave
                                      (fib-chord-seq (transpose-chord [ 11 15 19] (range 21)))
                                      #_(fib-chord-seq
                                        (transpose-chord [0 4 8 12 16 20 24] (range 21)))
                                      #_(fib-chord-seq
                                        (transpose-chord [0 9 14] (range 21))))
                                    (at-i [12])
                                    #_(at-i [1 10 20 16 12
                                             3 10 19 16 11]))))
                         3000))
                     0))))))))
