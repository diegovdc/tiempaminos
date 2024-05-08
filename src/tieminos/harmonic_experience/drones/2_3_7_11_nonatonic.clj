(ns tieminos.harmonic-experience.drones.2-3-7-11-nonatonic
  "Scale used in the Djinnerator piece by Jacky Lingon (@jl)"
  (:require
   [erv.utils.conversions :refer [midi->cps ratio->cents]]
   [erv.utils.core :refer [period-reduce]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.drones.sounds :refer [drone drone2 harmonic]]
   [tieminos.harmonic-experience.lattice :refer [draw-lattice]]
   [tieminos.math.utils :refer [linexp*]]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [tieminos.utils :refer [wrap-at]]))

(def root (midi->cps 44))
(def note-mappings  [1
                     22/21
                     33/28
                     121/98
                     4/3
                     3/2
                     11/7
                     242/147
                     363/196])

(comment
  (o/defsynth drone-bis ;; no 5th harmonic
    [freq 130
     amp 1
     gate 1]
    (o/out 0
           (-> (map #(* (lfo (o/n-rand 0.5 1.2) 0.2 0.6)
                        (o/sin-osc (* % freq)))
                    [1 2 3 4 6 7 #_8 9 11 13 #_15])
               (o/mix)
               (o/pan2 (lfo 0.4 -0.5 0.5))
               #_(o/hpf 700)
               (#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
               (* amp 0.7 (lfo 0.437 0.8 1) (o/env-gen (o/env-asr 2 1 2) :gate gate :action o/FREE)))))
  (def sa (drone-bis root))
  (ratio->cents 5/4)
  (o/ctl sa :gate 0)
  (def sa2 (drone2 root :amp 0.6))
  (o/ctl sa2 :gate 0)
  (def pa (drone (* 3/2 root)))
  (o/ctl pa :gate 0)
  (def ma (drone (* 14/18 root) :amp 0.8))
  (o/ctl ma :gate 0)
  (def h (harmonic (* root 9/8)))

  (o/ctl h :gate 0)
  (o/stop)

  (def lattice-data (draw-lattice
                     {:ratios (into #{} (map period-reduce
                                             note-mappings))}))

  (def oxygen (get-oxygen!))

  (when oxygen
    (midi-in-event
     :midi-input oxygen
     :note-on (fn [ev]
                (let [ratio* (wrap-at (:note ev) note-mappings)
                      ratio (* (inc (quot (- (:note ev) 45)
                                          9))
                               ratio*)]
                  (println ratio* (ratio->cents ratio*))
                  (swap! lattice-data update :played-notes conj (period-reduce ratio))
                  (harmonic (* root ratio) :amp (linexp* 0 127 0.5 5 (:velocity ev)))))
     :note-off (fn [ev]
                 (let [ratio (period-reduce (wrap-at (:note ev) note-mappings))]
                   (swap! lattice-data update :played-notes #(->> %
                                                                  (remove (fn [r] (= r ratio)))
                                                                  (into #{})))
                   nil)))))
