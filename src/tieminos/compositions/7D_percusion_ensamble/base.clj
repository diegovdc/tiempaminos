(ns tieminos.compositions.7D-percusion-ensamble.base
  (:require
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [tieminos.midi.core :refer [get-iac2!]]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.polydori.analysis.dorian-hexanies
    :refer [dorian-hexanies-in-polydori]]
   [tieminos.polydori.scale :refer [polydori-v2]]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.utils :refer [map-subscale-degs]]
   [time-time.standard :refer [rrand]]))

(def sink (midi/midi-out "VirMIDI"))
(def iac2 (get-iac2!))

(defn bh
  "Blackhole outs (starts on chan 3)"
  [out]
  (+ 22 out))

(defn my-malgo
  [config]
  (malgo-note (merge {:sink sink
                      :scale-size 29
                      :base-midi-deg 60
                      :base-midi-chan 0}
                     config)))

(defn deg->freq [& {:keys [base-freq scale degree]}]
  (scale/deg->freq (:scale polydori-v2)
                   base-freq
                   (map-subscale-degs (count (:scale polydori-v2))
                                      (:degrees
                                       (nth
                                         dorian-hexanies-in-polydori
                                         scale))
                                      degree)))


(defn diat->polydori-degree
  [scale degree]
  (map-subscale-degs (count (:scale polydori-v2))
                     (:degrees
                      (nth
                        dorian-hexanies-in-polydori
                        scale))
                     degree))

(o/defsynth low
  [freq 85
   amp 0.5
   mod-freq 8300
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (o/range-lin (o/pulse mod-freq) (- freq 15) (+ freq 15))
                 o/sin-osc
                 (o/pan2 pan)
                 (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(o/defsynth short-plate
  [freq 200
   amp 0.5
   mod-freq 1000
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out (-> (o/range-lin (o/pulse mod-freq) (- freq 200) (+ freq 200))
                 o/sin-osc
                 (o/pan2 pan)
                 (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                 (* amp (o/amp-comp-a freq)))))

(def synths (map #(partial % (groups/early)) [low short-plate]))

(defn init! []
  (groups/init-groups!))

(def mempan
  (memoize (fn [deg-mod]
             (rrand -1.0 1))))
