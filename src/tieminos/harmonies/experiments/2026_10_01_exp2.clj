(ns tieminos.harmonies.experiments.2026-10-01-exp2
  (:require
   [erv.cps.core :as cps]
   [tieminos.blackhole :as bh]
   [tieminos.harmonic-experience.drones.sounds :refer [drone]]
   [tieminos.harmonic-experience.sustainer :as legato]
   [tieminos.harmonies.cps-patterns :refer [->freqs cps-seq rotate]]
   [tieminos.synths :as s]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]
   [time-time.standard :refer [rrand]]))
(comment
  (def root 220)
  (legato/play! :id1 {:inst drone :params {:freq (* root 1)}})
  (legato/stop! :id1)

  (legato/multi :my-chord
                {1 {:inst drone :params {:freq (* root 1)}}
                 3/2 {:inst drone :params {:freq (* root 3/2)}}})
  (legato/multi :my-chord {1 {:inst drone :params {:freq (* root 1)}}})
  (legato/multi :my-chord {1 {:inst drone :params {:freq (* root 2)}}})
  (legato/multi :my-chord {})

  (def hex-chords
    (let [fs [1 3 5 7]]
      (map #(->> (cps-seq
                  (cps/make 2 fs)
                  (rotate %1 "ab ac1 ad"))
                 (->freqs 1))
           [0 2 1 3])))
  (def chorda (atom nil))
  (rain.v2/ref-rain
   :id :chordseq
   :durs [10]
   :ratio 1
   :on-event (rain.v2/on-event
              (let [chord (at-i hex-chords)]
                (reset! chorda chord)
                (legato/multi :my-chord
                              (->> chord
                                   (map (fn [r] [r {:inst drone :params {:a 1
                                                                         :s 0.8
                                                                         :r 1
                                                                         :out (bh/bus 1)
                                                                         :freq (* 1/2 root r)}}]))
                                   (into {}))))))

  (rain.v2/ref-rain
   :id :chordseq
   :durs (fn [_] (rrand 0.7 5))
   :ratio 1
   :on-event (rain.v2/on-event
              (let [chord @chorda]
                (s/low (rand-nth chord))))))
