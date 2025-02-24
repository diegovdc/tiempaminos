(ns tieminos.harmonic-experience.drones.centaur
  (:require
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.ratios :refer [ratios->scale]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.drones.sounds :refer [drone drone2 harmonic]]
   [tieminos.harmonic-experience.lattice :as hexp.lattice]
   [tieminos.harmonic-experience.trainer :as hexp.trainer]
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

(def scale (ratios->scale note-mappings))

(comment
  (hexp.lattice/setup-kb {:ref-note 48
                          :root root
                          :scale scale
                          :midi-kb (tieminos.midi.core/get-oxygen!)})
  (hexp.trainer/trainer {:scale scale
                         :root (midi->cps 60)
                         :degrees [0 3 6  10]})
  (hexp.trainer/stop)
  (def sa (drone root))
  (o/ctl sa :gate 0)
  (def sa2 (drone2 root :amp 0.6))
  (o/ctl sa2 :gate 0)
  (def pa (drone (* 3/2 root)))
  (o/ctl pa :gate 0)
  (def ma (drone (* 14/18 root) :amp 0.8))
  (o/ctl ma :gate 0)

  (def h (harmonic (* root 9/8)))
  (o/ctl h :gate 0)

  (o/stop))
