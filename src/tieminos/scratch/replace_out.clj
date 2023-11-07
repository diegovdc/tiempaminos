(ns tieminos.scratch.replace-out
  (:require
   [overtone.core :as o]))

(comment

  (o/defsynth sini [freq 300 out 0]
    (o/out out (* 0.2 (o/sin-osc freq))))

  (o/defsynth revy [bus 0]
    (o/replace-out bus [(* (o/sin-osc 2) (o/in bus)) ]))

  (o/defsynth o0 [bus 0]
    (o/out 0 (o/in bus)))

  (def outy (o/audio-bus 1 "outy"))
  (sini :out outy)
  (sini)
  (revy :bus outy)

  (o0 outy)

  (o/demo (*  (o/sin-osc 300) (o/sin-osc 2)))

  (o/stop)
  )
