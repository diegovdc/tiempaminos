(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths
  (:require
   [overtone.core :as o]
   [tieminos.overtone-extensions :as oe]))

(oe/defsynth
  cristal-liquidizado
  ;; Original version from 2.2.9.x
  [buf 0
   rate 1
   amp 0.5
   pan 0
   dur 1
   out 0]
  (o/out out
         (-> (o/play-buf 1 buf rate)
             (* amp
                (o/env-gen
                 (o/envelope
                  [0 1 1 0]
                  [(* 0.1 dur)
                   (* 0.7 dur)
                   (* 0.2 dur)])
                 :action o/FREE))
             (#(o/pan-az:ar 4 % pan)))))

(comment
  (def buf (o/load-sample "samples/habitat_samples/take-1-gusano-cuantico-2.2.9.2-algo-2-2-9-mic-2-bus-43.wav"))
  (cristal-liquidizado
   {:buf buf})

  (oe/defsynth sini
    [freq 200
     amp 0.5
     out 0]
    (o/out out (-> (o/sin-osc 200)
                   (* amp (o/env-gen (o/env-perc) :action o/FREE)))))

  (sini :out 3)

  (oe/defsynth sini-o
    [freq 200
     amp 0.5
     out 0]
    (o/out out (-> (o/sin-osc 200)
                   (* amp (o/env-gen (o/env-perc 2 2) :action o/FREE))
                   (#(oe/circle-az :num-channels 4
                                   :in %
                                   :pos (o/saw 1)))))))
