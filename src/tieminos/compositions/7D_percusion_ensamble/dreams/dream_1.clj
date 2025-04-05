(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-1
  "From exploration4"
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [tieminos.compositions.7D-percusion-ensamble.base
    :refer [bh diat->polydori-degree init! mempan my-malgo root stop!] :as *7d-base]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.seq-utils.core :refer [mseq rot xo]]
   [tieminos.compositions.7D-percusion-ensamble.dreams.hydra-client :as hydra]
   [tieminos.utils :refer [rbool rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

;; stereo outs

(def m1-vibes (bh 0))
(def m2-vibes (bh 2))
(def bass (bh 4))
;; midi

(def suave-m1 (midi/midi-out "Bus 1"))
(def suave-m2 (midi/midi-out "Bus 2"))
(def suave-bass (midi/midi-out "Bus 3"))
(def suave-mod (midi/midi-out "Bus 4"))
(def pbell (midi/midi-out "Bus 5"))
(def pbell-mod (midi/midi-out "Bus 6"))
(def sc-m1 (midi/midi-out "Bus 7"))
(def sc-m2 (midi/midi-out "Bus 8"))
(def sc-bass (midi/midi-out "Bus 9"))

;; TODO first two sections (tentatively)
;; TODO define different stereo outputs for the different instruments
;;

(o/defsynth short-plate
  [freq 200
   amp 0.5
   mod-freq 1000
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (o/out out
         (-> (o/range-lin (o/pulse mod-freq) (- freq 200) (+ freq 200))

             (o/pan2 pan)
             (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
             (* amp (o/amp-comp-a freq)))))

(o/defsynth low2
  ;; Has some more harmonics and optional sub-bass
  [freq 85
   amp 0.5
   mod-freq 8300
   sub 0
   sub2 0
   pan 0
   atk 0.01
   dcy 1
   out 0]
  (let [c (fn [freq] (+ (* (lfo-kr 1/2 0.9 1) (o/sin-osc freq))
                        (* sub (lfo-kr 1 0.8 1) (o/sin-osc (/ freq 2)))
                        (* sub2 (lfo-kr 1 0.8 1) (o/sin-osc (/ freq 4)))
                        (* 0.03 (lfo-kr 3 0.5 1) (o/sin-osc (* 2 freq)))))]
    (o/out out (-> (o/range-lin (* 2 (o/sin-osc mod-freq)) (- freq 15) (+ freq 15))
                   c
                   (o/pan2 pan)
                   (* (o/env-gen (o/env-perc atk dcy :curve (o/rand -3 -2))
                                 :time-scale 1
                                 :action o/FREE))
                   (* amp (o/amp-comp-a freq))))))

(defn sc-midi-data [& {:keys [sink deg dur]}]
  (my-malgo {:sink sink
             :deg deg
             :dur dur
             :vel 100}))

(def synths (map (fn [synth]
                   (fn [{:as args
                         :keys [sink deg-data freq-mul]
                         :or {freq-mul 1}}]
                     (when-not (zero? (:amp args))
                       (let [args* (-> args
                                       (dissoc :sink :deg-data)
                                       (assoc :freq (* freq-mul (:freq deg-data)))
                                       seq
                                       flatten)]
                         (apply synth (groups/early) args*)

                         (sc-midi-data :sink sink
                                       :deg (:polydori-degree deg-data)
                                       :dur (+ (:atk args 0.01) (:dcy args 1)))))))
                 [*7d-base/low *7d-base/short-plate low2]))

(defn delay* [ratio delay-ratio f]
  (ref-rain
   :id (random-uuid)
    ;; :ref ref*
   :ratio ratio
   :durs [delay-ratio 1]
   :loop? false
   :on-event (on-event
              (when (= i 1) (f)))))
(comment
  (init!)
  (stop!)
  (gp/stop))

(comment
  :start-section-1
;;;;;;;;;;;;
;;; INTRO
;;;;;;;;;;;;;

  (let [degs (fn [at-i] (at-i [0 #_#_#_2 0 (at-i [2 #_3]) #_(at-i [#_7 4]) #_#_(at-i [4 5]) 3]))
        bprob 0.
        m1t -1
        m2t 0
        mprob 0.
        mmix 0
        ;; base amps
        m1-amp 1
        m2-amp 1
        bass-amp 1
        ;; midi amps
        sbass-amp (fn [at-i] (at-i [100]))
        sm1-amp (fn [at-i] (at-i [80]))
        sm2-amp (fn [at-i] (at-i [100]))]
    (ref-rain
     :id ::1 :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [synth (rand-nth [low2 *7d-base/short-plate *7d-base/low])
                      deg (degs at-i)
                      f1-data (*7d-base/deg->data :base-freq root :scale 0 :degree deg)
                      f2-data (*7d-base/deg->data :base-freq root :scale 2 :degree (degs #(wrap-at (+ 0 i) %)))
                      f1 (:freq f1-data)
                      f2 (:freq f2-data)
                      pan (rrange -1 1)]

                  (synth
                   :freq f1
                   :mod-freq (rrand 6000 10000)
                      ;; :pan (mempan (mod deg 2))
                   :amp (* m1-amp (rrange 0.5 0.8))
                   :out m1-vibes)
                  (sc-midi-data :sink sc-m1 :deg (:polydori-degree f1-data) :dur 1)

                  (when (or #_true (#{1 3} (mod (inc index) 5)))
                    #_(delay*
                       (:ratio data) (at-i [1 1])
                       (fn []))
                    (synth
                     :freq (* 1 f2)
                        ;; :sub 1
                        ;; :sub2 2
                     :atk (rrange 0.01 0.03)
                     :pan (* -1 pan)
                     :mod-freq (rrand 6000 10000)
                        ;; :dcy (* (rrange 2 4) dur-s)
                     :amp (* m2-amp (rrange 0.5 0.8))
                     :out m2-vibes)
                    (sc-midi-data :sink sc-m2 :deg (:polydori-degree f2-data) :dur 1))

                    ;; bass (NOTE the mod 7)

                  (when (or #_true (#{1 3} (mod (inc index) 7)))
                    (synth
                     :freq (* 1 f2)
                     :sub 0.8
                     :sub2 0.5
                     :atk (rrange 0.01 0.03)
                     :pan (* -1 pan)
                     :mod-freq (rrand 6000 10000)
                      ;; :dcy (* (rrange 2 4) dur-s)
                     :amp (* bass-amp (rrange 0.5 0.8))
                     :out bass)
                    #_(delay*
                       1
                       (at-i [4/3 0 0 1/4 0])
                       (fn []))
                    #_(synth
                       :freq (* 1/2 f2)
                       :sub 0.9
                       :sub2 0.7
                       :atk (rrange 0.01 0.03)
                       :pan (* -1 pan)
                       :mod-freq (rrand 6000 10000)
                      ;; :dcy (* (rrange 2 4) dur-s)
                       :amp (* bass-amp (rrange 0.5 0.8))
                       :out bass)
                    #_(synth
                       :freq (* 1/4 f2)
                       :sub 0.9
                       :sub2 0.7
                       :atk (rrange 0.01 0.03)
                       :pan (* -1 pan)
                       :mod-freq (rrand 6000 10000)
                      ;; :dcy (* (rrange 2 4) dur-s)
                       :amp (* bass-amp (rrange 0.5 0.8))
                       :out bass)
                    (sc-midi-data :sink sc-bass :deg (:polydori-degree f2-data) :dur 1)))))
    (ref-rain
     :id ::1-bass :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [deg (degs at-i)]
                  (when (> bprob (rand))
                    (my-malgo {:sink suave-bass
                               :deg (diat->polydori-degree 0 (+ (at-i [4 -4 3 -4 -3]) deg))
                               :dur (weighted {1 9 1.5 8})
                               :vel (sbass-amp at-i)})))))
    (ref-rain
     :id ::1-m :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [deg (+ (degs at-i)
                             (at-i [2 2 3 2 3])
                             (weighted {0 5 4 4 8 3}))]
                  (when (> mprob (rand))
                    (if (> mmix (rand))
                      (my-malgo {:sink suave-m1
                                 :deg (diat->polydori-degree 2 (+ m2t deg) :modal)
                                 :dur (weighted {0.1 9 1 5})
                                 :vel (sm1-amp at-i)})
                      (my-malgo {:sink suave-m2
                                 :deg (diat->polydori-degree 0 (+ m1t  deg) :modal)
                                 :dur (weighted {0.1 9 1 5})
                                 :vel (sm2-amp at-i)})))))))

  :end-section-1)

(comment
  (hydra/moment-1)
  (hydra/play-main)
  (hydra/end)
  :start-section-2
  ;; [0 5 5] 2 [1 -7] 8 [4 7 3] [6 5] [3 3 8 9]
  (let [degs (fn [m]
               (case m
                 1 [0 2 0 [2 3 #_#_8 5] [7 4 #_#_#_#_3 4 2 1] [4 5] 3]
                 2 [[0 5 5]
                    2
                    [1 -7]
                    [8 2 8]
                    [4 7 3]
                    #_[6 5]
                    [3 8 9]]))
        ;; melodía base
        mainm 1
        mainf2m 1
        m2xos (xo "xoxoo")
        m2prob+ 0
        m1-amp 0.
        m2-amp  0
        ;; bass
        bassxos (xo (rot 0 "xoxoooo"))
        bassprob+ 0.
        bass-amp 0
        ;; surge bass
        bm 1
        sbass-amp [60 70]
        bprob 0.
        ;; line
        mm 1
        mprob 0.
        mmix 0.
        m1t 3
        m2t -1
        sm2-sinks [suave-m1 #_(assoc pbell :malgo/vel-amp 0.3)]
        sm1-sinks [suave-m2 #_(assoc pbell :malgo/vel-amp 0.3)]
        sm1-amp [80]
        sm2-amp [80]
        mratio 1/9
        sm-weights {0.1 9 1 5}
        m-reset? false
        ;; to modulate harmonically
        modum 2
        modu-amp (map #(+ % 0) [10 30])
        moduprob 0.
        modut -6
        modulator-reset? false
        ;; dimensions
        m1-space (fn [] [0 0])
        m2-space (fn [] 2)
        bass-space (fn [] 2)
        ;;
        sbass-space (fn [] 2)
        sm1-space (fn [] (weighted {0 1}))
        sm2-space (fn [] (weighted {2 1}))
        modu-space (fn [] (weighted {2 3, 5 1, 9 1, 6 1, 1 0, 8 0}))]
    (ref-rain
     :id ::1 :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [synth (rand-nth synths)
                      pan (rrange -1 1)]
                  (synth {:deg-data (*7d-base/deg->data :base-freq root
                                                        :scale (m1-space)
                                                        :degree (mseq i (degs mainm)))
                          :mod-freq (rrand 6000 10000)
                          :pan pan #_(mempan (mod deg 2))
                          :amp (* m1-amp (rrange 0.5 0.8))
                          :sink sc-m1
                          :out m1-vibes})
                  #_(delay* 1 (mseq i [1 0 0 1])
                            (fn [] (synth {:deg-data (*7d-base/deg->data :base-freq root
                                                                         :scale (m1-space)
                                                                         :degree (mseq (+ 1 i) (degs mainm)))
                                           :freq-mul 2
                                           :mod-freq (rrand 6000 10000)
                                           :pan pan #_(mempan (mod deg 2))
                                           :amp (* m1-amp (rrange 0.1 0.3))
                                           :sink sc-m1
                                           :out m1-vibes})))

                  (when (or (rbool m2prob+) (m2xos i))
                    (synth {:deg-data (*7d-base/deg->data :base-freq root
                                                          :scale (m2-space)
                                                          :degree (mseq i (degs mainf2m)))
                              ;; :sub 1
                              ;; :sub2 2
                            :atk (rrange 0.01 0.03)
                            :pan (* -1 pan)
                            :mod-freq (rrand 6000 10000)
                              ;; :dcy (* (rrange 2 4) dur-s)
                            :amp (* m2-amp (rrange 0.5 0.8))
                            :sink sc-m2
                            :out m2-vibes})

                    #_(delay* 1 (at-i [1])
                              (fn []
                                (synth {:deg-data (*7d-base/deg->data :base-freq root
                                                                      :scale (m2-space)
                                                                      :degree (mseq i (degs mainf2m)))
                                        :freq-mul 2
                                ;; :sub 1
                                ;; :sub2 2
                                        :atk (rrange 0.01 0.03)
                                        :pan (* -1 pan)
                                        :mod-freq (rrand 6000 10000)
                                ;; :dcy (* (rrange 2 4) dur-s)
                                        :amp (* m2-amp (rrange 0.5 0.8))
                                        :sink sc-m2
                                        :out m2-vibes}))))

                  (when (or (rbool bassprob+) (bassxos i))
                    (synth {:deg-data (*7d-base/deg->data :base-freq root :scale (bass-space) :degree (mseq i (degs bassm)))
                            :freq-mul 1
                            :sub 0.9
                            :sub2 0.7
                            :atk (rrange 0.01 0.03)
                            :pan (rrange -1 1)
                            :mod-freq (rrand 6000 10000)
                              ;; :dcy (* (rrange 2 4) dur-s)
                            :amp (* bass-amp (rrange 0.5 0.8))
                            :sink sc-bass
                            :out bass})
                    #_(delay* 1 (at-i [4/3 0 0 1/4 0])
                              (fn []))
                    #_(synth {:deg-data (*7d-base/deg->data :base-freq root :scale (bass-space) :degree (mseq i (degs bassm)))
                              :freq 1/2
                              :sub 0.9
                              :sub2 0.7
                              :atk (rrange 0.01 0.03)
                              :pan 0
                              :mod-freq (rrand 6000 10000)
                                ;; :dcy (* (rrange 2 4) dur-s)
                              :amp (* bass-amp (rrange 0.5 0.8))
                              :sink sc-bass
                              :out bass})
                    #_(synth {:deg-data (*7d-base/deg->data :base-freq root :scale (bass-space) :degree (mseq i (degs bassm)))
                              :freq-mul 1/4
                              :sub 0.9
                              :sub2 0.7
                              :atk (rrange 0.01 0.03)
                              :pan 0
                              :mod-freq (rrand 6000 10000)
                            ;; :dcy (* (rrange 2 4) dur-s)
                              :amp (* bass-amp (rrange 0.5 0.8))
                              :sink sc-bass
                              :out bass})))))
    (ref-rain
     :id ::1-bass :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [deg (mseq i (degs bm))]
                  (when (> bprob (rand))
                    (my-malgo {:sink suave-bass
                               :deg (diat->polydori-degree (sbass-space)
                                                           (+ (at-i [4 -4 3 -4 -3]) deg))
                               :dur (weighted {1 9 1.5 8})
                               :vel (mseq i sbass-amp)})))))

    (when m-reset? (gp/stop ::1-m))
    (ref-rain
     :id ::1-m :ref ::1
     :durs [3 2 2] :ratio mratio
     :on-event (on-event
                (when (> mprob (rand))
                  (let [deg (+ (mseq i (degs mm))
                               (at-i [2 2 3 2 3])
                               (weighted {0 5 4 4 8 3}))]
                    (if (> (rand) mmix)
                      (my-malgo {:sink sm1-sinks
                                 :deg (diat->polydori-degree (sm1-space) (+ m1t deg) :modal)
                                 :dur (weighted sm-weights)
                                 :vel (mseq i sm1-amp)})
                      (my-malgo {:sink sm2-sinks
                                 :deg (diat->polydori-degree (sm2-space) (+ m2t  deg) :modal)
                                 :dur (weighted sm-weights)
                                 :vel (mseq i sm2-amp)}))))))

    (when modulator-reset? (gp/stop ::1-modulator))
    (ref-rain
     :id ::1-modulator :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (when (> moduprob (rand))
                  (let [deg (+ (mseq i (degs modum))
                               (at-i [2 2 3 2 3])
                               (weighted {0 5 4 4 8 3}))]
                    (my-malgo {:sink pbell-mod
                               :deg (diat->polydori-degree
                                     (modu-space)
                                     (+ modut
                                        (at-i [2 3 2])
                                        (weighted {0 5 4 4 -4 4})
                                        deg))
                               :dur (weighted {0.1 3 1 5 2 5})
                               :vel (mseq i modu-amp)}))))))

  :end-section-2)

(comment
  (init!)
  (stop!)
  (gp/stop))
