(ns tieminos.scratch.tunings.centaura
  (:require
   [erv.utils.conversions :as conv]))

(def centaura
  [1
   33/32
   9/8
   7/6
   5/4
   4/3
   11/8
   3/2
   14/9
   5/3
   7/4
   15/8
   ])


(conv/ratio->cents (/ 66/32 14/9))
