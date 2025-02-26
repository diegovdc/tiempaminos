(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-1
  "From exploration4"
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [tieminos.compositions.7D-percusion-ensamble.base
    :refer [bh deg->freq diat->polydori-degree init! mempan my-malgo root
            stop!] :as *7d-base]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.utils :refer [rrange wrap-at]]
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

(def synths (map #(partial % (groups/early)) [*7d-base/low *7d-base/short-plate low2]))

(defn delay* [ref* delay-ratio f]
  (ref-rain
   :id (random-uuid)
   :ref ref*
   :durs [delay-ratio 1]
   :loop? false
   :on-event (on-event
              (when (= i 1) (f)))))

(comment
  (init!)
  (stop!)
  (gp/stop))

(defn sc-midi-data [& {:keys [sink deg dur]}]
  (my-malgo {:sink sink
             :deg deg
             :dur dur
             :vel 100}))


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
                       ::1 (at-i [1 1])
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
                  #_(when (or #_true (#{1 3} (mod (inc index) 7)))
                      #_(delay*
                         ::1 (at-i [1 1])
                         (fn []))
                      (synth
                       :freq (* 1/2 f2)
                        ;; :sub 0.9
                        ;; :sub2 0.7
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
  :start-section-2
; [0 5 5] 2 [1 -7] 8 [4 7 3] [6 5] [3 3 8 9]
  (let [degs (fn [at-i m]
               (case m
                 1 (at-i [0 2 0 (at-i [2 3]) (at-i [7 4]) (at-i [4 5]) 3])
                 2 (at-i [(at-i [0 5 5])
                          2
                          (at-i [1 -7])
                          (at-i [8 2 8])
                          (at-i [4 7 3])
                          #_(at-i [6 5])
                          (at-i [3 8 9])])))
        mainm 1
        mainf2m 1
        mainf2m-offset 0
        mamp 1
        m2vibesamp 1
        mbassamp 1
        bm 2
        sbass-amp (fn [at-i] (at-i [100]))
        bprob 0.
        mm 2
        mprob 0.
        mmix 1
        m1t 30
        m2t 10
        sm-amp (fn [at-i] (at-i [80]))
        mratio 1/9
        mdur1w 5
        ;; to modulate harmonically
        modum 2
        pmod-amp (fn [at-i] (+ 20 (at-i [10 30])))
        moduprob 0.
        moduws {2 3 6 1 1 0 8 0} ;; scale 8 also works well
        modut -6]
    (ref-rain
     :id ::1 :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (degs at-i mainm)
                      f1-data (*7d-base/deg->data :base-freq root :scale 0 :degree deg)
                      f2-data (*7d-base/deg->data :base-freq root :scale 2 :degree (degs #(wrap-at (+ mainf2m-offset i) %) mainf2m))
                      f1 (:freq f1-data)
                      f2 (:freq f2-data)
                      pan (rrange -1 1)]
                  (synth
                   :freq f1
                   :mod-freq (rrand 6000 10000)
                   :pan (mempan (mod deg 2))
                   :amp (* mamp (rrange 0.5 0.8))
                   :out m1-vibes)
                  (sc-midi-data :sink sc-m1 :deg (:polydori-degree f1-data) :dur 1)

                  (when (or #_true (#{1 3} (mod (inc index) 5)))
                    #_(delay*
                       ::1 (at-i [1 1])
                       (fn []))
                    (let [atk (rrange 0.01 0.03)
                          dcy 1 #_(* (rrange 2 4) dur-s)]
                      (synth
                       :freq (* 1 f2)
                        ;; :sub 1
                        ;; :sub2 2
                       :atk atk
                       :pan (* -1 pan)
                       :mod-freq (rrand 6000 10000)
                       :dcy dcy
                       :amp (* m2vibesamp (rrange 0.5 0.8))
                       :out m2-vibes)
                      (sc-midi-data :sink sc-m2 :deg (:polydori-degree f2-data) :dur (+ atk dcy))))

                  ;; bass (NOTE the mod 7)
                  #_(when (or #_true (#{1 3} (mod (inc index) 7)))
                      #_(delay*
                         ::1 (at-i [1 1])
                         (fn []))
                      (let [atk (rrange 0.01 0.03)
                            dcy 1 #_(* (rrange 2 4) dur-s)]
                        (synth
                         :freq (* 1/2 f2)
                          ;; :sub 0.9
                          ;; :sub2 0.7
                         :atk atk
                         :pan (* -1 pan)
                         :mod-freq (rrand 6000 10000)
                          ;; :dcy dcy
                         :amp (* mbassamp (rrange 0.5 0.8))
                         :out  bass)
                        (sc-midi-data :sink sc-bass :deg (:polydori-degree f2-data) :dur (+ atk dcy)))))))
    (ref-rain
     :id ::1-bass :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [deg (degs at-i bm)]
                  (when (> bprob (rand))
                    (my-malgo {:sink suave-bass
                               :deg (diat->polydori-degree 0
                                                           (+ (at-i [#_4 -4 3 -4 -3]) deg))
                               :dur (weighted {1 9 1.5 8})
                               :vel (sbass-amp at-i)})))))
    (ref-rain
     :id ::1-m :ref ::1
     :durs [3 2 2] :ratio mratio
     :on-event (on-event
                (let [deg (+ (degs at-i mm)
                             (at-i [2 2 3 2 3])
                             (weighted {0 5 4 4 8 3}))]
                  (when (> mprob (rand))

                    (if (> mmix (rand))
                      (my-malgo {:sinks [suave-m1 pbell]
                                 :deg (diat->polydori-degree 0 (+ m1t  deg))
                                 :dur (weighted {0.1 9 1 mdur1w})
                                 :vel (sm-amp at-i)})
                      (my-malgo {:sink suave-m2
                                 :deg (diat->polydori-degree 2 (+ m2t deg))
                                 :dur (weighted {0.1 9 1 mdur1w})
                                 :vel (sm-amp at-i)}))))))
    #_(gp/stop ::1-modulator)
    (ref-rain
     :id ::1-modulator :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [deg (+ (degs at-i modum)
                             (at-i [2 2 3 2 3])
                             (weighted {0 5 4 4 8 3}))]
                  (when (> moduprob (rand))
                    (my-malgo {:sink pbell-mod
                               :deg (diat->polydori-degree
                                     (weighted moduws)
                                     (+ modut
                                        (at-i [2 3 2])
                                        (weighted {0 5 4 4 -4 4})
                                        deg))
                               :dur (weighted {0.1 3 1 5 2 5})
                               :vel (pmod-amp at-i)}))))))

  :end-section-2)
