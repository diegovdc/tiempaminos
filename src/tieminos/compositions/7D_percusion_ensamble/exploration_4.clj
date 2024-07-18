(ns tieminos.compositions.7D-percusion-ensamble.exploration-4
  (:require
   [clojure.data.generators :refer [weighted]]
   [tieminos.compositions.7D-percusion-ensamble.base
    :refer [vel deg->freq diat->polydori-degree init! mempan my-malgo out
            pbell root ssuave surge-suave synths]]
   [tieminos.midi.core :refer [all-notes-off]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(comment
  (init!)
  (gp/stop)
  (doseq [s [surge-suave pbell]]
    (all-notes-off s)))

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
                   :out (out f1))

                  #_(when (or #_true (#{1 3} (mod (inc index) 5)))
                      (synth
                       :freq f2
                       :mod-freq (rrand 6000 10000)
                       :amp (rrange 0.5 0.8)
                       :out (out f2))))))
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
        moduws {2 3 6 1 1 3}
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
                   :out (out f1))

                  (when (or #_true (#{1 3} (mod (inc index) 5)))
                    (synth
                     :freq f2
                     :mod-freq (rrand 6000 10000)
                     :amp (rrange 0.5 0.8)
                     :out (out f2))))))
    (ref-rain
     :id ::1-bass :ref ::1
     :durs [3 2 2] :ratio 1/9
     :on-event (on-event
                (let [deg (degs at-i bm)]
                  (when (> bprob (rand))
                    (my-malgo {:deg (diat->polydori-degree 0
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
                      (my-malgo {:sinks [ssuave #_pbell]
                                 :deg (diat->polydori-degree 0 (+ m1t  deg))
                                 :dur (weighted {0.1 9 1 mdur1w})
                                 :vel 80})
                      (my-malgo {:deg (diat->polydori-degree 2 (+ m2t deg))
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

(comment
  :transition-1
  (gp/stop ::transition-1)
  ;; on reaper, increase "Reverb deeper" send to about half
  (let [deg* (fn [at-i]
               (+ 12
                  (at-i [0 6])
                  (at-i [#_(at-i [0 5 5])
                         2
                         (at-i [1 -7])
                         #_(at-i [8 2 8])
                         #_(at-i [4])
                         #_(at-i [6 15 10 6])
                         #_(at-i [3 3 8 9])])))
        scalew {6 1 9 2 10 0}
        play? (fn [i]
                (#{0 #_1 2 #_3 5 #_6 8 #_9 10} (mod i 11))
                1)
        amp-range #(rrand 0.01 0.2)
        atk-range #(rrand 0.01 0.1)
        dcy #(rrand 0.1 3)]
    (ref-rain
     :id ::transition-1
     :durs
     #_[2 1 2 2 2 2 2 2]
     [3 3 3 3 3 1 11 1 1 3 3 3 11 3 3 3 3]
     #_[3 3 3 3 11 3 1 11 1 1 3 3 11 3 11 3 3 3 3]
     #_[1 1 1 1 1 1 1 11 1 1 11 1 1 1 1 1 1]
     #_[1]
     :ratio 1/9
     :ref ::1
     :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (deg* at-i)
                      scale-1 (weighted scalew)
                      f1 (deg->freq :base-freq 200 :scale scale-1 :degree deg)]
                  (when (play? i)
                    (synth
                     :freq f1
                     :mod-freq (rrand 6000 10000)
                     :amp (amp-range)
                     :atk (atk-range)
                     :dcy (dcy)
                     :pan (weighted {(rrange -1 -0.8) 5
                                     (rrange 0.8 1) 5
                                     (rrange -08. 0.8) 1})
                     :out (out f1)))))))

  ;; transition 1 - last state, to play with ::3 (next section)
  (let [deg* (fn [at-i] (+ 12 (at-i [0 6]) (at-i [#_(at-i [0 5 5]) 2 (at-i [1 -7]) #_(at-i [8 2 8]) #_(at-i [4]) #_(at-i [6 15 10 6]) #_(at-i [3 3 8 9])]))) scalew {6 1 9 2 10 0} play? (fn [i] (#{0 #_1 2 #_3 5 #_6 8 #_9 10} (mod i 11)) 1) amp-range #(rrand 0.01 0.2) atk-range #(rrand 0.01 0.1) dcy #(rrand 0.1 3)] (ref-rain :id ::transition-1 :durs #_[2 1 2 2 2 2 2 2] [3 3 3 3 3 1 11 1 1 3 3 3 11 3 3 3 3] #_[3 3 3 3 11 3 1 11 1 1 3 3 11 3 11 3 3 3 3] #_[1 1 1 1 1 1 1 11 1 1 11 1 1 1 1 1 1] #_[1] :ratio 1/9 :ref ::1 :on-event (on-event (let [synth (rand-nth synths) deg (deg* at-i) scale-1 (weighted scalew) f1 (deg->freq :base-freq 200 :scale scale-1 :degree deg)] (when (play? i) (synth :freq f1 :mod-freq (rrand 6000 10000) :amp (amp-range) :atk (atk-range) :dcy (dcy) :pan (weighted {(rrange -1 -0.8) 5 (rrange 0.8 1) 5 (rrange -08. 0.8) 1}) :out (out f1))))))))

(comment
  :third-section
  ;; buen momento para echar un solo
  (let [scale-1 (atom 9)
        scale-2 (atom 6)
        scale-1w {0 0 6 4 9 7}
        scale-2w {2 0 6 4 9 0}

;;
        pad? true
        bprob 0.7 ;; 0.3 is good
        midprob 0.]
    (ref-rain
     :id ::3 :durs [3 2 2 3 2 2 3 2 2 3] :ratio 1/9
     :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [(at-i [0 5])
                                 (at-i [2])
                                 #_(at-i [1 -7])
                                 (at-i [8])
                                 (at-i [4 7 5])
                                 (at-i [6 15 10 6])
                                 #_(at-i [3 3 8 9])])
                      f1 (deg->freq :base-freq 200 :scale @scale-1 :degree deg)
                      f2 (deg->freq :base-freq (at-i [100 50 400]) :scale @scale-2 :degree deg)]
                    ;; set scales
                  (reset! scale-1 (weighted scale-1w))
                  (reset! scale-2 (weighted scale-2w))

                  (when (> (rand) 0.4)
                    (synth
                     :freq f1
                     :mod-freq (rrand 6000 10000)
                     :amp (rrand 0.05 0.1)
                     :dcy 3
                     :out (out f1))
                    (when pad?
                      (synth
                       :freq f1
                       :mod-freq (rrand 6000 10000)
                       :amp (rrand 0.1 0.1 #_0.3)
                          ;; NOTE use attack to make padd
                       :atk (rrand 0.01 2)
                       :dcy 3
                       :out (out f1))))
                  (when (or (#{3 6} (mod index 5)))
                    (synth
                     :freq f2
                     :mod-freq (rrand 6000 10000)
                     :amp (rrand 0.1 0.1 #_0.7)
                        ;; :atk (rrand 0.01 5)
                     :dcy 3
                     :out f2))

                  #_(if (> (rand) 0.7)
                        ;; NOTE keep vel 10 at first

                      (my-malgo {:deg (diat->polydori-degree scale-2 (+ 6 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel (at-i [10 30 60])})
                        ;; FIXME has a -2 midi note
                      #_(my-malgo {:deg (diat->polydori-degree 1 (+ 7
                                                                    (at-i [-6 -12 6 0 0 0]) ;; NOTE add later
                                                                    (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5 2 5}) :vel (at-i [10 30])}))
                  #_(my-malgo {:base-midi-chan (case deg -7 1 0) :deg deg :dur 0.1 :vel 100}))))
    (ref-rain
     :id ::3-bass
     :ref ::3
      ;; TODO send this to it's own sink, a suave-like bass with a bit more distortion or something like that
     :on-event (on-event
                (let [deg (at-i [(at-i [0 5])
                                 (at-i [2])
                                 #_(at-i [1 -7])
                                 (at-i [8])
                                 (at-i [4 7 5])
                                 (at-i [6 15 10 6])
                                 #_(at-i [3 3 8 9])])]
                  (when (> bprob (rand))
                    (my-malgo {:deg (diat->polydori-degree @scale-1
                                                           (+ -8 (at-i [2 3 3 2 3])
                                                              (weighted {0 5 4 4 -4 4}) deg))
                               :dur (weighted {0.1 9 2 5 3 1})
                               :vel (vel (* 1.2 (at-i [70 80 85 60 55])))})))))
    (ref-rain
     :id ::3-mid
     :ref ::3
     :on-event (on-event
                (let [deg (at-i [(at-i [0 5])
                                 (at-i [2])
                                 #_(at-i [1 -7])
                                 (at-i [8])
                                 (at-i [4 7 5])
                                 (at-i [6 15 10 6])
                                 (at-i [3 3 8 9])])]
                  (when (> midprob (rand))
                    (my-malgo {:deg (diat->polydori-degree
                                     @scale-2
                                     (+ (weighted {-2 7 4 1 6 1})
                                        (at-i [2 2 3 2 3])
                                        (weighted {0 5 4 4 -4 4})
                                        deg))
                               :dur (weighted {0.1 5 1 5 4 2})
                               :vel (vel (* 60 (at-i [1 0.8 0.7 0.6 0.7])))})))))))
