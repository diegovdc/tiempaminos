(ns tieminos.harmonic-experience.drones.centaur
  (:require
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.scale :refer [rotate-scale]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.lattice :as hexp.lattice]
   [tieminos.harmonic-experience.trainer :as hexp.trainer]
   [tieminos.harmonic-experience.utils :as hexp.utils]
   [tieminos.harmonic-experience.drone-box :as hexp.drone-box]
   [tieminos.midi.core :refer [get-exquis!]]
   [tieminos.scales.core :as scales]))

(def root (midi->cps 60))
(def scale
  "Scale modes of in the white keys:
  deg 0 - 1 - 5-limit major ji
  deg 3 - 7/6 - archytas ionian on white keys (starting in C) - major sounding, but with a touch of sadness/nostalgia"
  (-> (scales/get :grady :centaur)
      (rotate-scale 0)))

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
                         :degrees [0 2 4 9 10]
                         :amp 0.9
                         :out (hexp.utils/out 3)})
  (hexp.trainer/stop)

  (hexp.drone-box/start {:root root
                         :scale scale
                         :degrees (map #(- % 12) [0 -12 #_7])
                         :amps [0.5]
                         :out (hexp.utils/out 5)})
  (hexp.drone-box/stop))
