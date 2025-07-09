(ns tieminos.scratch.scratch
  #_{:clj-kondo/ignore [:unused-referred-var]}
  (:require
   [erv.cps.core :as cps]
   [erv.utils.conversions :as conv]))
(comment
  (conv/ratio->cents (/ 19 14)))
