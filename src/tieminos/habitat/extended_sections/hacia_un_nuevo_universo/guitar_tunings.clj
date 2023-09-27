(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.guitar-tunings
  (:require
   [overtone.core :as o]))

(def f 112)

(def string->ratio {6 1
                    5 3/2
                    4 5/2
                    3 7/2
                    2 11/2
                    1 12/2             ;maybe later 25/4 or 13/2
                    })
(o/defsynth tuner
  [freq 112
   ratio 1
   amp 0.5]
  (o/out 0 (* amp (o/pan2 (o/saw (* freq ratio))))))

(comment
  (tuner f (string->ratio 1))
  (o/stop))
