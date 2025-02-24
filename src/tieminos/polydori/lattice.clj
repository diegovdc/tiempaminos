(ns tieminos.polydori.lattice
  (:require
   [tieminos.harmonic-experience.lattice :as hexp.lattice]
   [tieminos.midi.core :as midi]
   [tieminos.polydori.scale :refer [polydori-v2]]))

(comment
  (hexp.lattice/setup-kb
   {:ref-note 31
    :root 1
    :scale (-> polydori-v2 :scale)
    :midi-kb (midi/get-lumatone!)}))
