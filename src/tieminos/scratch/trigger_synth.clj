(ns tieminos.scratch.trigger-synth
  (:require
   [overtone.core :as o]
   [tieminos.sc-utils.ndef.v1 :as ndef :refer [ndef]]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]))

(comment

  (ndef ::triggy
        (* 0.2
           (o/sin-osc (o/env-gen (o/asr) #_(o/envelope [200 400] [1])
                                 (lfo-kr 1 -1 1))))
        {:fade-time 0.1})

  (ndef/stop ::triggy))
