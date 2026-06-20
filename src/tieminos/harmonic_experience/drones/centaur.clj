(ns tieminos.harmonic-experience.drones.centaur
  (:require
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.scale :refer [rotate-scale]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.lattice :as hexp.lattice]
   [tieminos.harmonic-experience.trainer :as hexp.trainer]
   [tieminos.harmonic-experience.utils :as hexp.utils]
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
  (hexp.lattice/setup-kb {:ref-note 60
                          :root root
                          :scale scale
                          :midi-kb (get-exquis!)
                          :out 0})

  (hexp.trainer/trainer {:scale scale
                         :root (midi->cps 60)
                         :degrees [0 4 9 10]
                         :out 0})
  (hexp.trainer/stop)
  (hexp.utils/drone-box {:root root
                         :scale scale
                         :degrees [0 7]
                         :amps [0.5]
                         :out 0})
  (hexp.utils/stop-drone-box))
