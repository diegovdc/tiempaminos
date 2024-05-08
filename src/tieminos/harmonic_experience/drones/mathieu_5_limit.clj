(ns tieminos.harmonic-experience.drones.mathieu-5-limit
  "Full tuning for the tuning of lattice of The Harmonic Experience"
  (:require
   [erv.scale.core :as scale]
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.core :refer [pow]]
   [erv.utils.ratios :refer [ratios->scale]]
   [overtone.core :as o]
   [tieminos.harmonic-experience.drones.sounds :refer [drone harmonic]]
   [tieminos.harmonic-experience.utils :refer [midi->ratio&freq]]
   [tieminos.lattice.v1.lattice :refer [add-played-ratio draw-lattice
                                        remove-played-ratio]]
   [tieminos.math.utils :refer [linexp*]]
   [tieminos.midi.core :refer [get-lumatone! midi-in-event]]))

(def ref-note 48)
(def root (midi->cps ref-note))

(def harmonic-experience-12-tone
  (ratios->scale
    [1     ;; sa
     16/15 ;; komal re
     9/8   ;; re
     6/5   ;; komal ga
     5/4   ;; ga
     4/3   ;; ma
     45/32 ;; ma
     3/2   ;; pa
     8/5   ;; komal dha
     5/3   ;; dha
     9/5   ;; komal nise
     15/8  ;; ni
     ]))

(def harmonic-experience-22-tone
  "Example 16.20"
  (ratios->scale
    (set (concat
             (map #(* 5 5 (pow 3 %)) (range -1 1))
             (map #(* 5 (pow 3 %)) (range -2 4))
             (map #(pow 3 %) (range -3 6))
             (map #(* 1/5 (pow 3 %)) (range -1 3))
             [1/25]))))

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

  (> 3/2 45/32)
  (do
    (def scale harmonic-experience-22-tone)
    (let [lattice-size 120]
      (def lattice-atom (draw-lattice
                          {:ratios (map :bounded-ratio scale)
                           :width (* 16 lattice-size)
                           :height (* 9 lattice-size)}))))

  (def oxygen #_(get-oxygen!)
    (get-lumatone!))

  (defn get-note-data [ev]
    (midi->ratio&freq {:ref-note 16
                       :root 1
                       :scale harmonic-experience-22-tone
                       :midi-note (:note ev)}))

  (when oxygen
    (midi-in-event
      :midi-input oxygen
      :note-on (fn [ev]
                 (let [{:keys [ratio freq] } (get-note-data ev)]
                   (println (:note ev) ratio freq)
                   (add-played-ratio lattice-atom {:ratio ratio :stroke-weight 10  :color [200 200 120]})
                   (harmonic freq :amp (linexp* 0 127 0.5 3 (:velocity ev)))))
      :note-off (fn [ev]
                  (let [{:keys [ratio] } (get-note-data ev)]
                    (remove-played-ratio lattice-atom {:ratio ratio})
                    )
                  ))))
