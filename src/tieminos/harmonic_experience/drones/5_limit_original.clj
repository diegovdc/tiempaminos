(ns tieminos.harmonic-experience.drones.5-limit-original
  (:require
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.core :refer [period-reduce]]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [taoensso.timbre :as timbre]
   [tieminos.harmonic-experience.drones.sounds :refer [drone drone2 harmonic]]
   [tieminos.harmonic-experience.lattice :refer [draw-lattice]]
   [tieminos.math.utils :refer [linexp*]]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]))

(def root
  #_(midi->cps 48)
  (midi->cps 44))

(def note-mappings
  {41 4/6   ;; ma
   43 3/4   ;; pa
   44 8/10  ;; komal dha
   45 5/6   ;; dha
   46 9/10  ;; komal ni
   47 15/16 ;; ni
   48 1     ;; sa
   49 16/15 ;; komal re
   50 9/8   ;; re
   51 6/5   ;; komal ga
   52 5/4   ;; ga
   53 4/3   ;; ma
   54 45/32 ;; ma
   55 3/2   ;; pa
   56 8/5   ;; komal dha
   57 5/3   ;; dha
   58 9/5   ;; komal nise
   59 15/8  ;; ni
   60 2     ;; sa
   61 32/15 ;; komal re
   62 9/4   ;; re
   63 12/5  ;; komal ga
   64 10/4  ;; ga
   65 8/3  ;; ga
   66 45/16  ;; ma
   })

(comment
  (def sa (drone root))
  (o/ctl sa :gate 0)

  (def pa (drone (* 3/2 root) :amp 0.6))
  (o/ctl pa :gate 0)
  (def ma (drone (* 4/3 root) :amp 0.6))
  (o/ctl ma :gate 0)
  (def h (harmonic (* root 9/8)))

  (o/ctl h :gate 0)
  (o/stop)

  (def lattice-data (draw-lattice
                      {:ratios (into #{} (map period-reduce (map second note-mappings)))}))

  (def oxygen (get-oxygen!))

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
                  (when (note-mappings (:note ev))
                    (let [ratio (period-reduce (note-mappings (:note ev)))]
                      (swap! lattice-data update :played-notes #(->> %
                                                                     (remove (fn [r] (= r ratio)))
                                                                     (into #{})))
                      nil))))))
