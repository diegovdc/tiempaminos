(ns tieminos.harmonic-experience.drones.7-11
  "Experiment with ratios related to 7 and 11"
  (:require
   [erv.constant-structures.graphics :refer [init-cs-tool! update-state]]
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.core :refer [period-reduce]]
   [erv.utils.ratios :refer [ratios->scale]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.drones.sounds :refer [drone drone2 harmonic]]
   [tieminos.lattice.v1.lattice :refer [add-played-ratio draw-lattice remove-played-ratio]]
   [tieminos.midi.core :refer [get-lumatone! midi-in-event]]
   [tieminos.utils :refer [wrap-at]]))

(def root (midi->cps 45))

(def scale-11-tones (ratios->scale [1

                                    7
                                    11

                                    1/7
                                    1/11

                                    11/7
                                    7/11

                                    (* 7 11)
                                    (/ 1 (* 7 11))

                                    (* 11 11)
                                    (/ 1 (* 11 11))]))
(comment
  (-> scale-11-tones)
  (update-state cs-tool
                scale-11-tones
                [])
  (update-ratios! lattice-data
                  (map :bounded-ratio scale-11-tones)))

(def scale-15-tones (ratios->scale [1

                                    7
                                    11

                                    11/7
                                    7/11

                                    1/7
                                    1/11

                                    (* 7 11)
                                    (/ 1 (* 7 11))

                                    (* 11 11)
                                    (/ 1 (* 11 11))

                                    (* 7 7 7)
                                    (/ 1 (* 7 7 7))
                                    (* 7 7 7 11)
                                    (/ 1 (* 7 7 7 11))]))

(def note-mappings (map :bounded-ratio scale-11-tones))

(comment
  (update-ratios! lattice-data
                  (map :bounded-ratio scale-11-tones))

  (def cs-tool (init-cs-tool! (ratios->scale note-mappings) []))
  (update-state cs-tool
                scale-11-tones

                (->> [#_(* 1/7 11/7)
                      #_(* 1/7 1/7)]
                     ratios->scale
                     (map :bounded-ratio))))

(def deg->ratio (into {} (map-indexed
                          (fn [i {:keys [bounded-ratio]}] [i bounded-ratio])
                          scale-11-tones)))

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

  (def lattice-atom (draw-lattice
                     {:ratios (into #{} (map period-reduce
                                             note-mappings))}))

  (def oxygen #_(get-oxygen!)
    (get-lumatone!))

  (when oxygen
    (midi-in-event
     :midi-input oxygen
     :note-on (fn [{:keys [note channel]}]
                (let [deg (deg->ratio (- note 45))]

                  (add-played-ratio lattice-atom
                                    {:ratio deg
                                     :group-id channel
                                     :color [25 215 153]})
                  #_(harmonic freq :amp (linexp* 0 127 0.5 3 (:velocity ev)))))
     :note-off (fn [{:keys [note channel]}]
                 (let [deg (deg->ratio (- note 45))]
                   (remove-played-ratio lattice-atom
                                        {:ratio deg
                                         :group-id channel
                                         :color [25 215 153]})

                   nil)))))
