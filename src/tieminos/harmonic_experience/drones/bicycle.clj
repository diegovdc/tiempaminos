(ns tieminos.harmonic-experience.drones.bicycle
  (:require
   [erv.scale.core :as scale]
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.core :refer [period-reduce]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.drones.sounds :refer [drone drone2 harmonic sine]]
   [tieminos.harmonic-experience.lattice :refer [draw-lattice]]
   [tieminos.math.utils :refer [linexp*]]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]
   [tieminos.utils :refer [wrap-at]]))

(def root (midi->cps 45))
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
  (o/stop)

  (def lattice-data (draw-lattice
                     {:ratios (into #{} (map period-reduce
                                             note-mappings))}))

  (def oxygen (get-oxygen!))

  (when oxygen
    (let [scale (map (fn [r] {:bounded-ratio r :bounding-period 2}) note-mappings)]
      (midi-in-event
       :midi-input oxygen
       :note-on (fn [ev]
                  (let [ratio (wrap-at (:note ev) note-mappings)
                        deg (- (:note ev) 48)
                        freq (scale/deg->freq scale root deg)]

                    (swap! lattice-data update :played-notes conj ratio)
                    (if (= 11/8 ratio)
                      (sine freq :amp (linexp* 0 127 0.5 3 (:velocity ev)))
                      (harmonic freq :amp (linexp* 0 127 0.5 3 (:velocity ev))))))
       :note-off (fn [ev]
                   (let [ratio (period-reduce (wrap-at (:note ev) note-mappings))]
                     (swap! lattice-data update :played-notes #(->> %
                                                                    (remove (fn [r] (= r ratio)))
                                                                    (into #{})))
                     nil))))))
