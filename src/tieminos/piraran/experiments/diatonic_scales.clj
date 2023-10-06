(ns tieminos.piraran.experiments.diatonic-scales
  (:require
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [tieminos.polydori.analysis.dorian-hexanies
    :refer [dorian-hexanies-in-polydori]]
   [tieminos.polydori.scale :refer [polydori-v2]]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.synths :as s]
   [tieminos.utils :refer [map-subscale-degs]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(defn- deg->freq [& {:keys [base-freq scale degree]}]
  (scale/deg->freq (:scale polydori-v2)
                   base-freq
                   (map-subscale-degs (count (:scale polydori-v2))
                                      (:degrees
                                       (nth
                                        dorian-hexanies-in-polydori
                                        scale))
                                      degree)))

(comment
  (gp/stop)
  ;; routing
  (def out-bus (o/audio-bus 2 "blackhole"))
  (groups/init-groups!)
  (o/defsynth out-ctl [in 0 out 0] (o/out out (o/in in 2)))
  (out-ctl (groups/late) out-bus)
  (out-ctl (groups/late) out-bus 2)
  (def synths (map #(partial % (groups/early) :out out-bus) [s/low s/short-plate])))

(comment
  ;; first exploration
  (ref-rain
   :id ::scale-tester-1
   :durs [3 2 2]
   :ratio 1/9
   :on-event (on-event
              (let [deg (at-i [(at-i [0 1 12 0 6 7])
                               2
                               (at-i [-1 -7 -8])
                               4
                               (at-i [5 6])
                               8
                               (at-i [7 11 6])
                               (at-i [7 14 13])])]
                ((rand-nth [s/low s/short-plate])
                 (deg->freq (rand-nth [50 100]) (at-i [0 0 0 0 0 9 9 9 9]) deg)
                 :atk (rand-nth [0.1])
                 :mod-freq (rrand 300 10000))
                #_((rand-nth [s/low s/short-plate])
                   (deg->freq 400 (at-i [2 2 2 2 2 11 11 11  11]) deg)
                   :dcy 1
                   :amp (at-i [0.5 0.3 0.8])
                   :mod-freq (rrand 300 10000)))))
  (gp/stop)
  (ref-rain
   :id ::scale-tester-2
   :durs [5 5 3]
   :ratio 1/9
   :on-event (on-event
              (case (mod index 2)
                0 ((rand-nth [s/low s/short-plate])
                   (deg->freq 100 2 (at-i [0 -4 2 5 8 7 11]))
                   :atk 3
                   :dcy 3
                   :amp (rand 0.7)
                   :mod-freq (rrand 600 10000))
                1 ((rand-nth [s/low s/short-plate])
                   (deg->freq 100 0 (rand-nth [11 16 12 13 17 19]))
                   :atk (rand 0.3)
                   :dcy (rrand 2 8)
                   :amp (rand 0.7)
                   :mod-freq (rrand 6000 10000))
                nil))))

(comment
  ;; YT video version
  (gp/stop ::1)
  ;; now let's add some pads
  (do
    (ref-rain ;; sequencer
     :id ::1
     :durs [3 2 2]       ;; rhythm, it loops
     :ratio 1/9          ;; rhytmic ratio, sequencer runs at 9*60bpm
     :on-event (on-event ;; all this runs on every rhythmic event
                (let [degree (at-i [(at-i [10 0 11 17])
                                    (at-i [14 13])
                                    (at-i [1 -7 12 16])
                                    (at-i [9 8 10])
                                    (at-i [4 -3 13])
                                    (at-i [6 5 11])
                                    (at-i [3 13 8 -5 9])])
                      synth (rand-nth synths)]
                    ;; all 21 diatonicish scales have 3 parallel scales, so we really have
                    ;; 7 unique scales repeated 3 times each
                    ;; let's add a parallel line to this
                    ;;
                    ;; scale 14 seems really nice, but we must go on :)
                  (synth :freq (deg->freq :base-freq (at-i [200 200 200 100]) ; fundamental frequency
                                          :scale (at-i [20]) ; alternating the scale
                                          :degree degree)
                         :mod-freq (rrand 600 10000))
                  (synth :freq (deg->freq :base-freq (at-i [200 400])
                                          :scale (at-i [20])
                                          :degree degree)
                         :mod-freq (rrand 600 10000)))))

    ;; this is the last one, bye!

    (ref-rain ;; sequencer
     :id ::2
     :durs [5 3 3]       ;; rhythm, it loops
     :ratio 1/9          ;; rhytmic ratio, sequencer runs at 9*60bpm
     :on-event (on-event ;; all this runs on every rhythmic event
                (let [degree (at-i [(at-i [0 5])
                                    (at-i [1 -7])
                                    (at-i [1 -7 -13])
                                    (at-i [3 3 9])
                                    (at-i [0 11 2])])
                      synth (rand-nth synths)]
                  (synth :freq (deg->freq :base-freq (at-i [400 400 200])
                                          :scale 20
                                          :degree degree)
                         :atk (rand 0.2)
                         :dcy (rrand 3 7)
                         :amp (rand 0.3)
                         :mod-freq (rrand 600 10000))

                  (synth :freq (deg->freq :base-freq (at-i [800 400])
                                          :scale 20
                                          :degree degree)
                         :atk 5
                         :dcy 3
                         :amp (rand 0.4)
                         :mod-freq (rrand 6000 10000)))))))
