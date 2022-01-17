(ns tieminos.compositions.garden-earth.synths.fx
  (:require
   [overtone.core :as o]
   [tieminos.compositions.garden-earth.base :as ge-base]))

(o/defsynth rev [in 0 out 0 mix 0.5 room 0.8 damp 0.5 amp 1]
  (o/out out (* amp (o/free-verb (o/in in) mix room damp))))


(comment
  ;; `rev` usage
  (o/defsynth sini [out 0]
    (o/out out (* (o/pan2 (o/sin-osc 200) 0) 0.2 (o/env-gen (o/env-perc) :action o/FREE))))
  (sini)
  ;; using out 8
  (do
    ;; stereo capture of the signal
    (rev [:tail 31] 8 :amp 2)
    (rev [:tail 31] 9 1 :amp 2))

  (sini [:head @ge-base/groups] 8)

  (o/stop))
