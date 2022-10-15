(ns tieminos.scratch.multiple-outs
  (:require
   [overtone.core :as o]))

(o/defsynth s1
  []
  #_(o/out 0
         (* (o/decay2
              (o/impulse 1 0.25)
              0.01
              0.2)
            (o/pink-noise)))
  (o/out 1
         (* (o/decay2
              (o/impulse 1 0.25)
              0.01
              0.2)
            (o/pink-noise))))



(comment
  (s1)
  (o/stop)
  (o/demo 10 (* (o/decay2
                 (o/impulse 1 0.25)
                 0.01
                 0.2)
                (o/pink-noise))))
