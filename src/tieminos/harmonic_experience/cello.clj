(ns tieminos.harmonic-experience.cello
  (:require
   [overtone.core :as o]
   [tieminos.harmonic-experience.drones.sounds :refer [drone2]]))

(comment
  (o/stop)
  (o/volume 0.9)
  (drone2 65)
  (drone2 (* 2 65)))
