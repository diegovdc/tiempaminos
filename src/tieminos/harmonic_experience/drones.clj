(ns tieminos.harmonic-experience.drones
  (:require
   [erv.utils.core :refer [period-reduce]]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [taoensso.timbre :as timbre]
   [tieminos.harmonic-experience.lattice :refer [draw-lattice]]
   [tieminos.math.utils :refer [linexp*]]
   [tieminos.midi.core :refer [midi-in-event]]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]))

(def root 130)
(def note-mappings
  {41 4/6   ;; ma
   43 3/4   ;; pa
   44 8/10  ;; komal dha
   45 5/6   ;; dha
   46 9/10  ;; komal ni
   47 15/16 ;; ni
   48 1     ;; sa
   50 9/8   ;; re
   51 6/5   ;; komal ga
   52 5/4   ;; ga
   53 4/3   ;; ma
   54 45/32 ;; ma
   55 3/2   ;; pa
   56 8/5   ;; komal dha
   57 5/3   ;; dha
   58 9/5   ;; komal ni
   59 15/8  ;; ni
   60 2     ;; sa
   62 9/4   ;; re
   64 10/4  ;; re
   })


(comment
  ;; for lattice
  1
  9/8
  6/5
  5/4
  4/3
  3/2
  8/5
  5/3
  9/5
  15/8)

(comment
  
  (o/stop)
  (def sa (drone root))
  (o/ctl sa :gate 0)
  (def sa2 (drone2 root :amp 0.6))
  (o/ctl sa2 :gate 0)
  (def pa (drone (* 3/2 root)))
  (o/ctl pa :gate 0)
  (def h (harmonic (* root 5/4)))
  (o/ctl h :gate 0)
  (o/stop)

  (def lattice-data (draw-lattice
                      (into #{} (map period-reduce (map second note-mappings)))))

  (-> @lattice-data)
  (swap! lattice-data update :played-notes conj 5/4)
  (when (midi/midi-in "USB MIDI")
    (defonce oxygen (midi/midi-in "USB MIDI")))

  (when oxygen
    (midi-in-event
      :midi-input oxygen
      :note-on (fn [ev]
                 (if-let [ratio (note-mappings (:note ev))]
                   (do
                     (swap! lattice-data update :played-notes conj (period-reduce ratio))
                     (harmonic (* root ratio) :amp (linexp* 0 127 0.5 3 (:velocity ev))))
                   (timbre/error "Note not mapped" (:note ev))))
      :note-off (fn [ev]
                  (let [ratio (period-reduce (note-mappings (:note ev)))]
                    (swap! lattice-data update :played-notes #(->> %
                                                                   (remove (fn [r] (= r ratio)))
                                                                   (into #{})))
                    nil)))))

(o/defsynth drone2
  [freq 130
   amp 1
   gate 1]
  (o/out 0
         (o/saw freq)))

(o/defsynth drone ;; sine
  [freq 130
   amp 1
   gate 1]
  (o/out 0
         (-> (map #(* (lfo (o/n-rand 0.5 1.2) 0.2 0.6) (o/sin-osc (* % freq))) [1 2 3 4 5 6 7 #_16/5 8 9 15])
             (o/mix)
             (o/pan2 (lfo 0.4 -0.5 0.5))
             #_(o/hpf 700)
             (#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
             (* amp 0.7 (lfo 0.437 0.8 1) (o/env-gen (o/env-asr 2 1 2) :gate gate :action o/FREE)))))

(o/defsynth harmonic
  [freq 130
   amp 1
   gate 1]
  (o/out 0
         (-> (* 0.7 (lfo 0.6 0.2 0.6) (o/sin-osc [freq (* 2 freq)]))
             (o/pan2 (lfo 0.4 -0.5 0.5))
             #_(o/hpf 700)
             (#(+ % (o/bpf % (lfo 0.5 (* 2 freq) 800) 0.3)))
             (* amp (o/env-gen (o/env-asr 2 1 2) :gate gate :action o/FREE)))))
