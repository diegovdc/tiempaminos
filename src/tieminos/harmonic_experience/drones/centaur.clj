(ns tieminos.harmonic-experience.drones.centaur
  (:require
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.ratios :refer [ratios->scale]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.drones.sounds :refer [drone drone2 harmonic]]
   [tieminos.harmonic-experience.lattice :as hexp.lattice]
   [tieminos.harmonic-experience.trainer :as hexp.trainer]
   [tieminos.harmonic-experience.utils :as hexp.utils]
   [tieminos.midi.core :refer [get-oxygen!]]))

(def root (midi->cps 48))
(def note-mappings [1
                    21/20
                    9/8
                    7/6
                    5/4
                    4/3
                    7/5
                    3/2
                    14/9
                    5/3
                    7/4
                    15/8])

(def scale
  "Scale modes of in the white keys:
  1 - 5-limit major ji
  7/6 - archytas ionian on white keys (starting in C) - major sounding, but with a touch of sadness/nostalgia"
  (ratios->scale (map #(/ % #_7/6 1) note-mappings)))

(comment
  (o/stop)
  (hexp.lattice/setup-kb {:ref-note 48
                          :root root
                          :scale scale
                          :midi-kb (tieminos.midi.core/get-exquis!)})

  (hexp.trainer/trainer {:scale scale
                         :root (midi->cps 60)
                         :degrees [0 3 10]})
  (hexp.trainer/stop)
  (hexp.utils/drone-box root scale [0 #_7] [1])
  (hexp.utils/drone-box root scale []))
