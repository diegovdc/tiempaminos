(ns tieminos.harmonic-experience.cello
  (:require
   [erv.utils.conversions :refer [midi->cps]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.drones.sounds :refer [drone2]]))

(comment

  (o/stop)
  (o/volume 0.9)

  (drone2 (/ (midi->cps 60) 4))
  (drone2 (* 2 65)))
