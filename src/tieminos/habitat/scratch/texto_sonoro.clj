(ns tieminos.habitat.scratch.texto-sonoro
  (:require
   [overtone.core :as o]
   [tieminos.habitat.routing :refer [input reaper-returns special-inputs]]))

(comment
  (def texto-sonoro (input {:in (-> special-inputs :texto-sonoro :in)
                            :out (reaper-returns 1)}))
  (o/kill texto-sonoro)
  (-> special-inputs :texto-sonoro))
