(ns tieminos.harmonic-experience.drones.bicycle
  (:require
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.ratios :refer [ratios->scale]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.drones.sounds :refer [drone harmonic]]
   [tieminos.harmonic-experience.lattice :as hexp.lattice]
   [tieminos.midi.core]))

(def ref-note 48)
(def root (midi->cps ref-note))
(def note-mappings [1
                    13/12
                    9/8
                    7/6
                    5/4
                    4/3
                    11/8
                    3/2
                    13/8
                    5/3
                    7/4
                    11/6])

(comment
  (def sa (drone root :amp 0.5))
  (o/ctl sa :gate 0)
  #_(def sa2 (drone2 root :amp 0.6)) ;; doesn't work, too loud
  #_(o/ctl sa2 :gate 0)
  (def pa (drone (* 3/2 root)))
  (o/ctl pa :gate 0)
  (def ma (drone (* 4/3 root) :amp 0.8))
  (o/ctl ma :gate 0)
  (def h (harmonic (* root 9/8)))

  (o/ctl h :gate 0)
  (o/stop)

  (hexp.lattice/setup-kb
   {:ref-note ref-note
    :root root
    :scale (ratios->scale note-mappings)
    :midi-kb (tieminos.midi.core/get-oxygen!)}))
