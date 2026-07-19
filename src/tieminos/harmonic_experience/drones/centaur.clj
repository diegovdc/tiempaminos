(ns tieminos.harmonic-experience.drones.centaur
  (:require
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.scale :refer [rotate-scale]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.drone-box :as hexp.drone-box]
   [tieminos.harmonic-experience.lattice :as hexp.lattice]
   [tieminos.harmonic-experience.trainer :as hexp.trainer]
   [tieminos.harmonic-experience.utils :as hexp.utils]
   [tieminos.midi.core :refer [get-exquis!]]
   [tieminos.scales.core :as scales]
   [tieminos.seq-utils.core :refer [choose]]))

(def root (midi->cps 60))
(def scale
  "Scale modes in the white keys:
  | degree | root | description 
  |  0     | 1    | 5-limit major ji
  |  3     | 7/6  | archytas ionian on white keys (starting in C) - major sounding, but with a touch of sadness/nostalgia
                  | D F# G B C - slendric pentatonic
                  | D Eb F# G Ab B C - Chromatic heptatonic
  "
  (-> (scales/get :grady :centaur)
      (rotate-scale 3)))

(comment
  (o/stop)
  (hexp.utils/set-output-mode! :reaper)

  (hexp.lattice/setup-kb {:ref-note 60
                          :root root
                          :scale scale
                          :midi-kb (get-exquis!)
                          :out (hexp.utils/out 7)})

  (hexp.trainer/trainer {:scale scale
                         :root (midi->cps 60)
                         :degrees (choose 0 2 4 5 9 11)
                         :amp 0.4
                         :out (hexp.utils/out 3)})
  (hexp.trainer/stop)

  (hexp.drone-box/start {:root root
                         :scale scale
                         :degrees (map #(- % 12) [0  7])
                         :amps [0.5]
                         :out (hexp.utils/out 5)})
  (hexp.drone-box/stop))
