(ns tieminos.compositions.7D-percusion-ensamble.exploration3
  (:require
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [tieminos.midi.core :refer [get-iac2!]]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.polydori.analysis.dorian-hexanies :refer [dorian-hexanies-in-polydori]]
   [tieminos.polydori.scale :refer [polydori-v2]]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.utils :refer [map-subscale-degs rrange]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]
   [time-time.dynacan.players.gen-poly :as gp]
   [time-time.standard :refer [rrand]]))

(def sink (midi/midi-out "VirMIDI"))

(def iac2 (get-iac2!))

(defn bh "Blackhole outs" [out] (+ 20 out))

(defn my-malgo
  [config]
  (malgo-note (merge {:sink sink
                      :scale-size 6
                      :base-midi-deg 60}
                     config)))

(defn- deg->freq [& {:keys [base-freq scale degree]}]
  (scale/deg->freq (:scale polydori-v2)
                   base-freq
                   (map-subscale-degs (count (:scale polydori-v2))
                                      (:degrees
                                       (nth
                                        dorian-hexanies-in-polydori
                                        scale))
                                      degree)))

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

(comment
  (init!)

  (gp/stop)

;;;;;;;;;;;;
;;; INTRO
;;;;;;;;;;;;;

  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :pan (mempan (mod deg 2))
                    :amp (rrange 0.45 0.51)
                    :out (bh 0))
                  (my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))

  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0 2])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :pan (mempan (mod deg 2))
                    :amp (rrange 0.45 0.51)
                    :out (bh 0))
                  (my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))

  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0 2 1 2])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :amp (rrange 0.45 0.51)
                    :out (bh 0))
                  (my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))

  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0 2 1 (at-i [2 3])])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :amp (rrange 0.45 0.51)
                    :out (bh 0))
                  (my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))



  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0 2 1 (at-i [2 3]) 4])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :amp (rrange 0.45 0.51)
                    :out (bh 0))
                  (my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))


  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0 2 1 (at-i [2 3]) (at-i [7 4]) (at-i [4 5 ]) 3])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :out (bh 0))
                  (when (#{} (mod (inc index) 7))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale 2 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :out (bh (case deg -7 2 0))))
                  (my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))

  (ref-rain ;; NOTE evol previous
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                                        ; [0 5 5] 2 [1 -7] 8 [4 7 3] [6 5] [3 3 8 9]
                      deg (at-i [(at-i [0 #_ #_5 5])
                                 2
                                 (at-i [1 -7])
                                 (at-i [8 #_ #_2 8])
                                 (at-i [4 7 3])
                                 (at-i [6 5])
                                 (at-i [3 #_ #_3 8 9])])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :out (bh (case deg -7 2 0)))
                  (when (or #_true (#{} (mod index 5)))
                    (synth
                      :freq (deg->freq :base-freq (at-i [200 #_100]) :scale 2 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :out (bh (case deg -7 2 0))))
                  (my-malgo {:base-midi-chan (case deg -7 1 0) :deg deg :dur 0.1 :vel 100})))))
