(ns tieminos.harmonies.experiments.2026-10-01-exp2
  (:require
   [erv.cps.core :as cps]
   [erv.utils.conversions :as conv]
   [overtone.midi :as midi]
   [tieminos.blackhole :as bh]
   [tieminos.harmonic-experience.drones.sounds :refer [drone]]
   [tieminos.harmonic-experience.sustainer :as legato]
   [tieminos.harmonies.cps-patterns :refer [->freqs cps-seq shift]]
   [tieminos.midi.plain-algo-note :refer [algo-note]]
   [tieminos.seq-utils.core :refer [lin rainseq]]
   [tieminos.synths :as s]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))
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
    (let [fs [1 3 11 7]]
      (map #(->> (cps-seq
                  (cps/make 2 fs)
                  (shift %1 "ab ac cb"))
                 (->freqs 1))
           [0 2 1 3])))
  (def chorda (atom nil))
  (rain.v2/stop #_:chordseq)
  (rain.v2/ref-rain
   :id :chordseq
   :ref :drums
   :durs [10]
   :ratio 1
   :on-event (rain.v2/on-event
              (let [chord (at-i hex-chords)]
                (println (map #(conv/cps->name* (* root %)) chord))
                (reset! chorda chord)
                (legato/multi :my-chord
                              (->> chord
                                   (take 1)
                                   (map (fn [r] [r {:inst drone :params {:a 1
                                                                         :s 0.8
                                                                         :r 1
                                                                         :out (bh/bus 1)
                                                                         :amp 0.3
                                                                         :freq (* 1/2 root r)}}]))
                                   (into {}))))))

  (rain.v2/ref-rain
   :id :melo
   :ref :drums
   :durs (fn [{:keys [index]}] (let [i index] (rainseq {0.5 1 0.25 1})))
   :ratio 1
   :on-event (rain.v2/on-event
              (let [chord @chorda]
                (s/low (* root 4 (rand-nth chord))))))

  (def sink (midi/midi-out "VirMIDI"))

  (rain.v2/stop)
  (rain.v2/ref-rain
   :id :drums
   :durs [1]
   :ratio 1/4
   :on-event (rain.v2/on-event
              (algo-note {:sink sink
                          :dur (* 0.1 dur-s)
                          :note (rainseq [34 0 35 0 35 (lin 0 43) 37 0 34 0 38 0 35 0 42 42])
                          :vel 80}))))
