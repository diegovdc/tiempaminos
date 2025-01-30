(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-1
  "From exploration4"
  (:require
   [clojure.data.generators :refer [weighted]]
   [tieminos.compositions.7D-percusion-ensamble.base
    :refer [bh deg->freq diat->polydori-degree init! mempan my-malgo pbell
            root ssuave surge-suave synths]]
   [tieminos.midi.core :refer [all-notes-off]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(comment
  (init!)
  (gp/stop)
  (doseq [s [surge-suave pbell]]
    (all-notes-off s)))

;; stereo outs
(def bass (bh 0))
(def m1-vibes (bh 0))
(def m2-vibes (bh 2))

;; TODO first two sections (tentatively)
;; TODO define different stereo outputs for the different instruments
(comment
  :start-section-1
;;;;;;;;;;;;
;;; INTRO
;;;;;;;;;;;;;

  (let [degs (fn [at-i] (at-i [0 2 (at-i [1 0]) (at-i [2 3]) (at-i [7 4]) (at-i [4 5]) 3]))
        bprob 0.9
        m1t 5
        m2t -4
        mprob 1
        mmix 0.6]
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
                   :pan (mempan (mod deg 2))
                   :amp (rrange 0.5 0.8)
                   :out m1-vibes)

                  (when (or #_true (#{1 3 5} (mod (inc index) 7)))
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
                    (my-malgo {:deg (diat->polydori-degree 0 (+ (at-i [4 -4 3 -4 -3]) deg))
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
                      (my-malgo {:deg (diat->polydori-degree 2 (+ m2t deg) :modal)
                                 :dur (weighted {0.1 9 1 5})
                                 :vel 80})
                      (my-malgo {:deg (diat->polydori-degree 0 (+ m1t  deg) :modal)
                                 :dur (weighted {0.1 9 1 5})
                                 :vel 100})))))))

  :end-section-1)

(comment
  :start-section-2
; [0 5 5] 2 [1 -7] 8 [4 7 3] [6 5] [3 3 8 9]
  (let [degs (fn [at-i m]
               (case m
                 1 (at-i [0 2 1 (at-i [2 3]) (at-i [7 4]) (at-i [4 5]) 3])
                 2 (at-i [(at-i [0 5 5])
                          2
                          (at-i [1 -7])
                          (at-i [8 2 8])
                          (at-i [4 7 3])
                          (at-i [6 5])
                          (at-i [3 8 9])])))
        mainprob 0.01
        mainm 2
        bm 2
        bprob 0.01
        mm 2
        mprob 0.3
        mmix 0.5
        m1t 5
        m2t 1
        mratio 1/19
        mdur1w 8
        ;; to modulate harmonically
        modum 1
        moduprob 0.005
        moduws {2 7 6 4 8 4}
        modut 8]
    #_(ref-rain
       :id ::1 :durs [3 2 2] :ratio 1/9
       :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (degs at-i mainm)
                        f1 (deg->freq :base-freq root :scale (weighted moduws) :degree deg)
                        f2 (deg->freq :base-freq root :scale 0 :degree deg)]
                    (when (> mainprob (rand))
                      (synth
                       :freq f1
                       :mod-freq (rrand 6000 10000)
                       :pan (rrange -1.0 1)
                       :atk (weighted {0.01 9 0.4 5})
                       :dcy (weighted {1 9 1.5 8 2 6})
                       :amp (rrange 0.3 0.6)
                       :out m1-vibes)

                      (when (or #_true (#{1 3 5} (mod (inc index) 5)))
                        (synth
                         :freq (/ f2 2)
                         :mod-freq (rrand 6000 10000)
                         :atk (weighted {0.01 9 0.5 8})
                         :dcy (weighted {3 7 1.5 8 2 6})
                         :pan (rrange -1.0 1)
                         :amp (rrange 0.2 0.5)
                         :out m2-vibes))))))
    #_(ref-rain
       :id ::1-bass :ref ::1
       :durs [3 2 2] :ratio 1/9
       :on-event (on-event
                  (let [deg (degs at-i bm)]
                    (when (> bprob (rand))
                      (my-malgo {:deg (diat->polydori-degree 2
                                                             (+ (at-i [4 -4 3 -4 -3]) deg))
                                 :dur (weighted {1 9 1.5 8 2 6})
                                 :vel 100})))))
    #_(ref-rain
       :id ::1-m :ref ::1
       :durs [3 2 2] :ratio mratio
       :on-event (on-event
                  (let [deg (+ (degs at-i mm)
                               (at-i [2 2 3 2 3])
                               (weighted {0 5 4 4 8 3}))]
                    (when (> mprob (rand))

                      (if (> mmix (rand))
                        (my-malgo {:sinks [ssuave pbell]
                                   :deg (diat->polydori-degree 0 (+ m1t  deg))
                                   :dur (weighted {0.1 9 1 mdur1w})
                                   :vel 100})
                        (my-malgo {:deg (diat->polydori-degree 2 (+ m2t deg))
                                   :dur (weighted {0.1 9 1 mdur1w})
                                   :vel 80}))))))
    #_(gp/stop ::1-modulator)
    #_(ref-rain
       :id ::1-modulator :ref ::1
       :durs [3 2 2] :ratio 1/9
       :on-event (on-event
                  (let [deg (+ (degs at-i modum)
                               (at-i [2 2 3 2 3])
                               (weighted {0 5 4 4 8 3}))]
                    (when (> moduprob (rand))
                      (my-malgo {:sink pbell
                                 :deg (diat->polydori-degree
                                       (weighted moduws)
                                       (+ modut
                                          (at-i [2 3 2])
                                          (weighted {0 5 4 4 -4 4})
                                          deg))
                                 :dur (weighted {0.1 3 1 5 2 5})
                                 :vel (+ 20 (at-i [10 30]))}))))))

  :end-section-2)
