(ns tieminos.scratch.lag-and-ctl
  (:require
   [overtone.core :as o]
   [tieminos.sc-utils.ndef.v1 :as ndef]))

(comment

  (ndef/ndef ::sine
      (let [freq 200]
        (-> (o/sin-osc 200)
            (* 0.2)
            (o/pan2))))

  (ndef/stop ::sine)
  (o/stop)

  (o/defsynth laggy-sine
    [freq 200]
    (o/out 0 (* 0.2 (o/sin-osc freq))))

  (o/defsynth laggy-sine
    [freq 200]
    (o/out 0 (* 0.2 (o/sin-osc (o/lag:kr 200 1)))))

  (laggy-sine)
  )
