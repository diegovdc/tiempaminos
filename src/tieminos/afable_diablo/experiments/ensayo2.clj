(ns tieminos.afable-diablo.experiments.ensayo2
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :refer [deg->freq]]
   [overtone.core :as o]
   [tieminos.afable-diablo.base :as base :refer [get-out]]
   [tieminos.afable-diablo.harmonic-clock :as hc]
   [tieminos.afable-diablo.harmonic-form :as form]
   [tieminos.afable-diablo.scale :refer [root]]
   [tieminos.math.utils :refer [linexp]]
   [tieminos.midi.core :refer [midi-in-event]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(def scale* "Index of a scale for `hc/get-scale`" (atom 0))
(-> @scale*)
(defn set-scale [i] (reset! scale* i))

(defn get-freq
  ([scale deg] (get-freq deg 0))
  ([scale deg period]
   (deg->freq scale root deg :period period)))

(def period-weights (atom {-2 20}))

(defn main-8v
  ([period] (main-8v period 20))
  ([period weight] (reset! period-weights {period weight})))

(o/defsynth low2
  [freq 85
   amp 1
   mod-amp 1
   mod-amp-end 0.1
   mod-dur 1
   mod-freq 8300
   mod-freq-range 30
   pan 0
   atk 0.01
   dcy 0.7
   sust 0.3
   rel 0.5
   gate 1
   out 0]
  (o/out out (-> (o/range-lin (* (o/env-gen
                                  (o/envelope [mod-amp mod-amp-end]
                                              [mod-dur]))
                                 (o/pulse mod-freq))
                              (- freq (/ mod-freq-range 2))
                              (+ freq (/ mod-freq-range 2)))
                 o/sin-osc
                 (o/pan2 pan)
                 (* (o/env-gen (o/env-adsr atk dcy sust rel)
                               :gate gate :action o/FREE))
                 (* amp))))

;; TODO
;; algo con los grafos de los cps?
#_(defn in-re [weights])

(comment
  (midi-in-event
   :note-on (fn [ev]
              (let [weights (merge {-3 1  -8 3 -4 2 -6 3} @period-weights)
                    weight (weighted weights)
                    freq (get-freq
                          (hc/get-scale @scale*)
                          (ev :note)
                          weight)
                    vel (ev :velocity)
                    amp (* 0.2 (/ vel 127))]

                (low2
                 :freq freq
                 :amp (* 4 amp)
                 :mod-amp (* 0.5 (rand-nth [40
                                            (rrange 60 100)
                                            (rrange 300 400)
                                            (rand 2000)]) amp)
                 :mod-freq (rrange 800 900)
                 :atk (first (linexp 127 0 0.02 1 [vel]))
                 :pan (weighted {0.5 10 0 4 -1 2})
                 :rel (first (linexp 100 0 0.3 (inc (rand 4)) [vel]))
                 :out (get-out :midi-kb))))
   ;; :note-off (fn [_] (println "off" ((juxt :channel :note) _)))
   ))

(comment
  (set-scale 0)
  (main-8v -1)
  (main-8v -2)
  (main-8v -3)
  (main-8v -4)
  (main-8v -5)
  (main-8v -7)
  (main-8v -8)
  (main-8v -10)
  (main-8v -11)
  (main-8v -13)
  (main-8v -16)
  )

(defn chord-durs [events-num total-dur]
  (let [single-dur (/ total-dur events-num)]
    (repeat events-num single-dur)))


(defn m3-chord-seq []
  (ref-rain :id m3-chord-seq
            :loop? false
            :durs (->> form/harmonic-form
                       (filter #(= 3 (:momento %)))
                       first :dur
                       (chord-durs 12))
            :on-event (on-event (set-scale index))))

(comment
  ;; TODO mover a ns init?
  (do (gp/stop) (reset! base/elapsed-time 0) (o/recording-stop))
  (do
    (o/recording-start (format "/home/diego/Desktop/piraran-ensayo-%s.wav" (java.util.Date.)))
    (base/count-time)
    (hc/run
      {:moment-events {#_ #_3 m3-chord-seq}})))
