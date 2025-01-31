(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-1
  "From exploration4"
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.midi :as midi]
   [tieminos.compositions.7D-percusion-ensamble.base
    :refer [bh deg->freq diat->polydori-degree init! mempan my-malgo root
            stop! synths]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

;; stereo outs

(def bass (bh 0))
(def m1-vibes (bh 2))
(def m2-vibes (bh 4))
;; midi

(def suave-m1 (midi/midi-out "Bus 1"))
(def suave-m2 (midi/midi-out "Bus 2"))
(def suave-bass (midi/midi-out "Bus 3"))
(def suave-mod (midi/midi-out "Bus 4"))
(def pbell (midi/midi-out "Bus 5"))
(def pbell-mod (midi/midi-out "Bus 6"))

(comment
  (init!)
  (stop!)
  (gp/stop))

;; TODO first two sections (tentatively)
;; TODO define different stereo outputs for the different instruments
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
        mmix 0]
    (ref-rain
     :id ::1 :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (degs at-i)
                      f1 (deg->freq :base-freq root :scale 0 :degree deg)
                      f2 (deg->freq :base-freq root :scale 2 :degree deg)]
                  (synth
                   :freq f1
                   :mod-freq (rrand 6000 10000)
                      ;; :pan (mempan (mod deg 2))
                   :amp (rrange 0.5 0.8)
                   :out m1-vibes)

                  #_(when (or #_true (#{1 3} (mod (inc index) 5)))
                      (synth
                       :freq f2
                       :mod-freq (rrand 6000 10000)
                       :amp (rrange 0.5 0.8)
                       :out m2-vibes)))))
    (ref-rain
     :id ::1-bass :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [deg (degs at-i)]
                  (when (> bprob (rand))
                    (my-malgo {:sink suave-bass
                               :deg (diat->polydori-degree 0 (+ (at-i [4 -4 3 -4 -3]) deg))
                               :dur (weighted {1 9 1.5 8})
                               :vel 100})))))
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
                                 :vel 80})
                      (my-malgo {:sink suave-m2
                                 :deg (diat->polydori-degree 0 (+ m1t  deg) :modal)
                                 :dur (weighted {0.1 9 1 5})
                                 :vel 100})))))))

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
        bm 2
        bprob 0.
        mm 2
        mprob 0.
        mmix 1
        m1t 30
        m2t 10
        mratio 1/9
        mdur1w 5
        ;; to modulate harmonically
        modum 2
        moduprob 0.
        moduws {2 3 6 1 1 0 8 0} ;; scale 8 also works well
        modut -6]
    (ref-rain
     :id ::1 :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (degs at-i mainm)
                      f1 (deg->freq :base-freq root :scale 0 :degree deg)
                      f2 (deg->freq :base-freq root :scale 2 :degree deg)]
                  (synth
                   :freq f1
                   :mod-freq (rrand 6000 10000)
                   :pan (mempan (mod deg 2))
                   :amp (rrange 0.5 0.8)
                   :out m1-vibes)

                  (when (or #_true (#{1 3} (mod (inc index) 5)))
                    (synth
                     :freq f2
                     :mod-freq (rrand 6000 10000)
                     :amp (rrange 0.5 0.8)
                     :out m2-vibes)))))
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
                               :vel 100})))))
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
                                 :vel 80})
                      (my-malgo {:sink suave-m2
                                 :deg (diat->polydori-degree 2 (+ m2t deg))
                                 :dur (weighted {0.1 9 1 mdur1w})
                                 :vel 80}))))))
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
                               :vel (+ 20 (at-i [10 30]))}))))))

  :end-section-2)
