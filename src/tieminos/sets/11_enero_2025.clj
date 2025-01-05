(ns tieminos.sets.11-enero-2025
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [algo-note malgo-note]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]
   [time-time.dynacan.players.gen-poly :as gp]
   [time-time.standard :refer [rrand]]))

(def sample-base-dir "~/Music/samples/")

(defn delacreme-stacks
  [sample-name]
  (format  "%sDelacreme-One-Shots/stacks/%s.wav"
           sample-base-dir
           sample-name))

(defn delacreme-single
  [sample-name]
  (format  "%sDelacreme-One-Shots/singles/%s.wav"
           sample-base-dir
           sample-name))

(def bh-default-out 20)

(o/defsynth st-smpl
  [buf 0
   amp 1
   rate 1
   pan 0
   out bh-default-out]
  (o/out out (-> (o/play-buf 2 buf rate)
                 (o/mix)
                 (o/pan2 pan)
                 (* amp (o/env-gen (o/envelope [0 1 1 0]
                                               [0.0001
                                                (- (o/buf-dur buf)
                                                   0.0002)
                                                0.0001])
                                   :action o/FREE)))))
(o/defsynth mono-smpl
  [buf 0
   rate 1
   amp 1
   pan 0
   out bh-default-out]

  (o/out out (-> (o/play-buf 1 buf rate)
                 (o/pan2 pan)
                 (* amp (o/env-gen (o/envelope [0 1 1 0]
                                               [0.0001
                                                (- (o/buf-dur buf)
                                                   0.0002)
                                                0.0001])
                                   :action o/FREE)))))
(def rim (o/sample (delacreme-stacks "Rim_multirim")))
(def bd (o/sample (delacreme-single "bd29_01_Synthdrum Pack")))
(def bd2 (o/sample (delacreme-single "BD_Kick boom1_Kick Pack Deluxe")))
(def hh (o/sample (delacreme-single "HH_Zildjian Avedis_V-08_s_HiHat Essentials")))
(def hho (o/sample (delacreme-single "HH_Zildjian quick_V-01_s_HiHat Essentials")))
(def congao (o/sample (delacreme-single "conga open_World Sounds Vol3")))
(def conga-low (o/sample (delacreme-single "conga low open_World Sounds Vol3")))
(def conga-high (o/sample (delacreme-single "conga high hit_World Sounds Vol3")))
(def sd (o/sample (delacreme-single "SD_dnb_Raw Muffled Snares")))
(st-smpl rim :out 20)
(st-smpl bd)
(-> sd :n-channels)

(comment

  (gp/stop)

  (ref-rain
   :id :bd
   :tempo 180
   :ratio 1/4
   :durs [4 4 4 4 4 4 4 2 1 1 2 2]
   :on-event (on-event
              (when-not (and (> (rand) 0.5) (= dur 1/4))
                (st-smpl (weighted {bd 3
                                    ;; bd2 1
                                    })))))
  (ref-rain
   :id :rim
   :ref :bd
   :tempo 180
   :ratio 1/4
   :durs [6 6 4 2 4]
   :on-event (on-event
              (st-smpl rim :pan (rrand -1.0 1))))

  (ref-rain
   :id :hh
   :ref :bd
   :tempo 180
   :ratio 1/4
   :durs [1 2 2 2 2 2 2 2 2 1]
   :on-event (on-event
              (if (>  0.5 (rand))
                (mono-smpl hh
                           :amp (weighted {1 1
                                           0.8 2
                                           0.7 3
                                           0.5 4})
                           :pan (rrand -1.0 1))
                (mono-smpl hho :amp (weighted {1 1
                                               0.8 2
                                               0.7 3
                                               0.5 4})
                           :pan (rrand -1.0 1)))))

  (ref-rain
   :id :congas
   :ref :bd
   :tempo 180
   :ratio 1/4
   :durs [2 2 2 2 2 2 2 2 1 2 1 1 1]
   :on-event (on-event
              (st-smpl (rand-nth [conga-low conga-high congao])
                       :amp (+ #_(at-i [0.5 0 0 0])
                             (weighted {1 1
                                        0.8 2
                                        0.7 3}))
                       :rate (weighted {1 8
                                        12/11 8
                                        13/11 4
                                        7/4 3
                                        3/2 2}))))
  (ref-rain
   :id :sd
   :ref :bd
   :tempo 180
   :ratio 1
   :durs [5/4 3/4]
   :on-event (on-event (when-not (= dur 5/4) (mono-smpl sd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (def sink (midi/midi-out "VirMIDI"))
  (def sink2 (midi/midi-out "VirMIDI Bus 3"))
  (def sink3 (midi/midi-out "VirMIDI Bus 4"))

  (ref-rain
   :id :bd
   :tempo 180
   :ratio 1/4
   :durs [4 2 2 4 1 2 1 1 3 4 2 1 1 2 2]
   :on-event (on-event
              (when-not (and (> (rand) 0.5) (= dur 1/4))
                (st-smpl (weighted {bd 3
                                    bd2 1})
                         :amp 1))))
  (gp/stop)
  (ref-rain
   :id :melody
   :ref :bd
   :tempo 180
   :ratio 1
   :durs [4 2 4 3 1]
   :on-event (on-event
              (algo-note {:sink           sink
                          :dur            (weighted {3 1})
                          :vel            (min 127 (int (* 12 (at-i [8 3 4 5 3]))))
                          :scale-size     7
                          :chan           0
                          :offset         60
                          :note           (+ (at-i [0 3 5 7 8 8]))})
              (when (zero? (mod i 6))
                (algo-note {:sink           sink
                            :dur            (weighted {5 1})
                            :vel            (min 127 (int (* 12 (at-i [8 3 4 5 3]))))
                            :scale-size     7
                            :chan           0
                            :offset         60
                            :note           (+ (at-i [19 18 17 18]))}))))
  (ref-rain
   :id :melody2
   :ref :bd
   :tempo 180
   :ratio 1/8
   :durs [4 2 4 3 1 4 2 4 3 1 8]
   :on-event (on-event
              #_(let [{:keys [durs]} data])
              (when-not (= dur 1)
                #_(algo-note {:sink sink
                              :dur (weighted {1 3
                                              1/2 1
                                              1/10 1
                                              1/5 2})
                              :vel (min 127 (int (* 12 (at-i [8 3 4 5 3]))))
                              :chan 1
                              :offset 60
                              :note (+ (at-i [0 3 5 7 8 8]))}))))

  (ref-rain
   :id :hh
   :ref :bd
   :tempo 180
   :ratio 1/4
   :durs [1 2 2 2 2 2 2 2 2 1]
   :on-event (on-event
              (mono-smpl hh
                         :amp (weighted {1 1
                                         2 (if (= 0 (mod i 8)) 2 0)
                                         0.8 2
                                         0.7 3
                                         0.5 4})
                         :pan 0.5)))

  (gp/stop))
