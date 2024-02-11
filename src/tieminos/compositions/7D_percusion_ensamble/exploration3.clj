(ns tieminos.compositions.7D-percusion-ensamble.exploration3
  (:require
   [clojure.data.generators :refer [weighted]]
   [tieminos.compositions.7D-percusion-ensamble.base
    :refer [bh diat->polydori-degree init! my-malgo]]
   [tieminos.midi.core :refer [all-notes-off]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(comment
  (init!)
  (gp/stop)
  (all-notes-off sink)
;;;;;;;;;;;;
;;; INTRO
;;;;;;;;;;;;;
  (init!)
  
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
                  #_(my-malgo {:deg (diat->polydori-degree 0 deg) :dur 0.1 :vel 100}))))

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
                  #_(when (> (rand ) 0.6)
                      (my-malgo {:deg (diat->polydori-degree 0 (+ (at-i [4 -4 3 -4 -3]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100})))))

  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0 2 1 (at-i [2 #_3]) #_ 4])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :pan (mempan (mod deg 2))
                    :amp (rrange 0.45 0.51)
                    :out (bh 0))
                  ;; TODO agregar silencions con when-not
                  #_(when (> (rand ) 0.6)
                      (my-malgo {:deg (diat->polydori-degree 0 (+ (at-i [0 -4 0 -4 -3]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                  #_(if (> (rand) 0.5)
                      (my-malgo {:base-midi-chan 0 :deg (diat->polydori-degree 0 (+ (at-i [-4  0 -4 -4 0 -4 0]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                  #_(when (> (rand) 0.4))
                  #_(my-malgo {:deg (diat->polydori-degree 0 (+ -1 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 }) deg)) :dur (weighted {0.1 9 1 5}) :vel 100})
                  #_(my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))

  #_(ref-rain
      :id ::1 :durs [3 2 2] :ratio 1/9
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (at-i [0 2 1 (at-i [2 3])])]
                    (synth
                      :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrange 0.45 0.51)
                      :out (bh 0))
                    #_(my-malgo {:base-midi-chan 0 :deg (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4}) deg) :dur (weighted {0.1 9 1 5}) :vel 100})
                    #_(my-malgo {:base-midi-chan 0 :deg (+ 6 deg) :dur 0.1 :vel 100}))))



  #_(ref-rain
      :id ::1 :durs [3 2 2] :ratio 1/9
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (at-i [0 2 1 (at-i [2 3]) 4])]
                    (synth
                      :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrange 0.45 0.51)
                      :out (bh 0))
                    #_(my-malgo {:base-midi-chan 0 :deg (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4}) deg) :dur (weighted {0.1 9 1 5}) :vel 100})
                    #_(my-malgo {:base-midi-chan 0 :deg (+ 6 deg) :dur 0.1 :vel 100})
                    #_(my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Comineza transición 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (ref-rain
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [0 2 1 (at-i [2 3]) (at-i [7 4]) (at-i [4 5 ]) 3])]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale 0 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :out (bh 0))
                  (when (or true ( #{1 3 5} (mod (inc index) 7)))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale 2 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :out (bh (case deg -7 2 0))))
                  #_(when (or #_true (#{5 10 13} (mod (inc index) 14)))
                      (my-malgo {:deg (diat->polydori-degree 2 (+  (weighted {-6 5 }) deg)) :dur (weighted {0.1 9 1 5}) :vel 100}))
                  #_(my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))

  (ref-rain ;; NOTE evol previous
    :id ::1 :durs [3 2 2] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                                        ; [0 5 5] 2 [1 -7] 8 [4 7 3] [6 5] [3 3 8 9]
                      deg (at-i #_[(at-i [0 #_ #_5 5])
                                   2
                                   (at-i [1 -7])
                                   (at-i [8  #_ #_2 8])
                                   (at-i [4 7 #_3])
                                   (at-i [6 5])
                                   (at-i [3  #_ #_ #_3 8 9])]
                                [(at-i [0 #_ #_ 5 5])
                                 2
                                 (at-i [1 -7])
                                 (at-i [8 #_ #_ 2 8])
                                 (at-i [4 7 #_3])
                                 (at-i [6 5])
                                 (at-i [3  #_ #_ #_3 8 9])])]
                  (when (#{1 2 3 4 5} (mod i 5))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale (at-i [0 0 0 0 0]) :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :out (bh (case deg -7 2 0))))
                  (when (or #_true (#{0 } (mod index 7)))
                    (synth
                      :freq (deg->freq :base-freq (at-i [200 100]) :scale (at-i [2]) :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :out (bh (case deg -7 2 0))))
                  (my-malgo {:deg (diat->polydori-degree 0 (+ #_6 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel 10})
                  (if (> (rand) 0.3)
                    ;; NOTE keep vel 10 at first

                    (my-malgo {:deg (diat->polydori-degree (at-i [2]) (+ 2 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg))
                               :dur (weighted {0.1 9 1 5})
                               ;; `+ 20`, add later ;; original [10 30 60]
                               :vel (+ 50 (at-i [70 60 60 70 50]))})
                    ;; FIXME has a -2 midi note
                    ;; NOTE esta modula... ir agregándola lentamente
                    #_(my-malgo {:deg (diat->polydori-degree (weighted {1 0 6 5})
                                                             (+ 8
                                                                (at-i [-6 -12 6 0 0 0 ]) ;; NOTE add later
                                                                (weighted {0 5 4 4 -4 4}) deg))
                                 :dur (weighted {0.1 3 1 5 2 5})
                                 :vel (+ 20 (at-i [10 30]))}))
                  #_(my-malgo {:base-midi-chan (case deg -7 1 0) :deg deg :dur 0.1 :vel 100}))))

  
  (gp/stop ::2)

  ;; NOTE not sure about this
  (ref-rain
    :id ::2 :durs [3 2 2] :ratio 1/9
    :ref ::1
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [(at-i [0 5 5])
                                 2
                                 (at-i [1 -7])
                                 (at-i [8 2 8])
                                 (at-i [4 7 3])
                                 #_(at-i [6 15 10 6])
                                 (at-i [3 3 8 9])])
                      scale-1 (weighted {9 1})]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale scale-1 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :amp 0.3
                    :out (bh (case deg -7 2 0)))
                  (when (or #_true (#{3 6} (mod index 7)))
                    (synth
                      :freq (deg->freq :base-freq (at-i [200 #_100]) :scale 2 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :out (bh (case deg -7 2 0))))
                  #_(my-malgo {:deg (diat->polydori-degree scale-1 (+ 6 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel 10})
                  #_(if (> (rand) 0.7)
                      ;; NOTE keep vel 10 at first

                      (my-malgo {:deg (diat->polydori-degree 2 (+ 6 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel (at-i [10 30 60])})
                      ;; FIXME has a -2 midi note
                      #_ (my-malgo {:deg (diat->polydori-degree 1 (+ 7
                                                                     (at-i [-6 -12 6 0 0 0]) ;; NOTE add later
                                                                     (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5 2 5}) :vel (at-i [10 30])}))
                  #_ (my-malgo {:base-midi-chan (case deg -7 1 0) :deg deg :dur 0.1 :vel 100}))))


  (ref-rain
    :id ::3
    :durs
    #_[2 1 2 2 2 2 2 2]
    #_[3 3 3 3 3 1 11 1 1 3 3 3 11 3 3 3 3 ]
    #_[3 3 3 3 11 3 1 11 1 1 3 3 11 3 11 3 3 3 3]
    #_[1 1 1 1 1 1 1 11 1 1 11 1 1 1 1 1 1]
    [1]
    :ratio 1/9
    :ref ::1
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (+ 12  (at-i [(at-i [0 5 5])
                                        2
                                        #_(at-i [1 -7])
                                        #_(at-i [8 2 8])
                                        (at-i [4])
                                        (at-i [6 15 10 6])
                                        #_(at-i [3 3 8 9])]))
                      scale-1 (weighted {6 0 9 2 10 2})]
                  (when-not (or #_false (#{0 1 2 3 5 6 8 9 10} (mod i 11)))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale scale-1 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrand 0.02 0.1)
                      ;; :atk (rrand 0.01 0.3)
                      :dcy (rrand 0.1 2)
                      :pan (rrand -1.0 1)
                      :out (bh (case deg -7 2 0)))))))



  (ref-rain
    :id ::3
    :durs
    #_[2 1 2 2 2 2 2 2]
    #_[3 3 3 3 3 1 11 1 1 3 3 3 11 3 3 3 3 ]
    #_[3 3 3 3 11 3 1 11 1 1 3 3 11 3 11 3 3 3 3]
    #_[1 1 1 1 1 1 1 11 1 1 11 1 1 1 1 1 1]
    [1]
    :ratio 1/9
    :ref ::1
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (+ 12  (at-i [(at-i [0 5 5])
                                        2
                                        #_(at-i [1 -7])
                                        #_(at-i [8 2 8])
                                        (at-i [4])
                                        (at-i [6 15 10 6])
                                        #_(at-i [3 3 8 9])]))
                      scale-1 (weighted {6 0 9 2 10 2})]
                  (when-not (or #_false (#{0 1 2 3 5 6 8 9 10} (mod i 11)))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale scale-1 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrand 0.02 0.1)
                      ;; :atk (rrand 0.01 0.3)
                      :dcy (rrand 0.1 2)
                      :pan (rrand -1.0 1)
                      :out (bh (case deg -7 2 0)))))))

;;;;;;;;;;;;;;;;
;;;;Second transition
;;;;;;;;;;;;;;;;

  ;; NOTE maybe use before ... still need to figure out pads
  (ref-rain
    :id ::1 :durs [3 2 2 3 2 2 3 2 2 3] :ratio 1/9
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [(at-i [0 5])
                                 (at-i [2 ])
                                 #_(at-i [1 -7])
                                 (at-i [8])
                                 #_(at-i [4 7 5])
                                 #_(at-i [6 15 10 6])
                                 #_(at-i [3 3 8 9])])
                      scale-1 (weighted {0 0 6 0 9 7})
                      scale-2 (weighted {2 0 6 4 9 0})]
                  (when (> (rand) 0.3 )
                    (synth
                      :freq (deg->freq :base-freq 200 :scale scale-1 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrand 0.1 0.1 #_0.3)
                      ;; NOTE use attack to make padd
                      ;; :atk (rrand 0.01 2)
                      :dcy 3
                      :out (bh (case deg -7 2 0))))
                  (when (or (#{3 6} (mod index 5)))
                    (synth
                      :freq (deg->freq :base-freq (at-i [100 50 400]) :scale scale-2 :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :amp (rrand 0.1 0.1 #_0.7)
                      ;; :atk (rrand 0.01 5)
                      :dcy 3
                      :out (bh (case deg -7 2 0))))
                  #_(when (> (rand) 0.4 )
                      (my-malgo {:deg (diat->polydori-degree scale-1 (+ 0 (at-i [2 3 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 2 5 3 1}) :vel 100}))
                  #_(when (> (rand) 0.4 )
                      (my-malgo {:deg (diat->polydori-degree scale-2 (+ -2 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 5 1 5 4 2}) :vel 60}))
                  #_(if (> (rand) 0.7)
                      ;; NOTE keep vel 10 at first

                      (my-malgo {:deg (diat->polydori-degree scale-2 (+ 6 (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel (at-i [10 30 60])})
                      ;; FIXME has a -2 midi note
                      #_ (my-malgo {:deg (diat->polydori-degree 1 (+ 7
                                                                     (at-i [-6 -12 6 0 0 0]) ;; NOTE add later
                                                                     (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5 2 5}) :vel (at-i [10 30])}))
                  #_ (my-malgo {:base-midi-chan (case deg -7 1 0) :deg deg :dur 0.1 :vel 100}))))
  (gp/stop ::1)


  (gp/stop ::4)

  (let [tr 2] ;; expand let to enclose the other refrain
    (ref-rain
      :id ::2 :durs [3 2 2] :ratio 1/9
      :ref ::1
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (at-i [(at-i [0 5 5])
                                   2
                                   (at-i [1 -7])
                                   (at-i [8 2 8])
                                   (at-i [4 7 5 5 3])
                                   (at-i [6 15 10 6])
                                   (at-i [3 3 8 9])])
                        scale-1 (weighted {4 5 })]
                    (synth
                      :freq (deg->freq :base-freq 100 :scale scale-1 :degree (+ (min 5 tr) deg))
                      :mod-freq (rrand 6000 10000)
                      :amp 0.3
                      :out (bh (case deg -7 2 0)))
                    (synth
                      :freq (deg->freq :base-freq 100 :scale scale-1 :degree (+ tr deg))
                      :mod-freq (rrand 6000 10000)
                      :amp 0.3
                      :out (bh (case deg -7 2 0)))
                    (when (or #_true (#{3 6} (mod index 7)))
                      (synth
                        :freq (deg->freq :base-freq (at-i [100 #_100]) :scale 2 :degree deg)
                        :mod-freq (rrand 6000 10000)
                        :out (bh (case deg -7 2 0))))
                    ))))

  (ref-rain
    :id ::4 :durs [3 2 2] :ratio 1/9
    :ref ::2
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [(at-i [0 10 8])
                                 2
                                 (at-i [1 -7])
                                 (at-i [8 2 8])
                                 (at-i [4 7 3])
                                 (at-i [6 15 10 6])
                                 (at-i [3 3 8 9])])
                      scale-1 (weighted {4 5})
                      tr (+ #_tr 6)]
                  (if (> (rand) 0.1)
                    ;; NOTE keep vel 10 at first

                    (my-malgo {:deg (diat->polydori-degree 4 (+ tr (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg))
                               :dur (weighted {0.1 9 1 5 2 5})
                               :vel (at-i [100 100 100])})
                    ;; FIXME has a -2 midi note
                    (my-malgo {:deg (diat->polydori-degree 4 (+ tr
                                                                (at-i [-6 -12 6 0 0 0]) ;; NOTE add later
                                                                (weighted {0 5 4 4 -4 4}) deg))
                               :dur (weighted {0.1 9 1 5 2 5})
                               :vel (at-i [100 80])}))
                  )))


  ;; ESTADIO #3
  ;; NOTE construir esto
  ;; [0 5 5] 2 [1 -7] [9 8] [4 -3 7 3] [6 5] [3 3 8 -5 9] [0 5 11]
  ;; estaría chidos unos arpegios ascendentes, quizá en el luma - grabar y repetir loop, en silencio, en el performance subirle para denotar el momento -,
  ;;         algo que construya intensidad
  ;;
  (ref-rain :id ::4 :durs [3 2 2] :ratio 1/9 :ref ::2 :on-event (on-event (let [synth (rand-nth synths) deg (at-i [(at-i [0 5 5]) 2 (at-i [1 -7]) (at-i [9 8]) (at-i [4 -3 7 3]) (at-i [6 5]) (at-i [3 3 8 -5 9]) (at-i [0 5 11])]) scale-1 (weighted {4 5}) tr 0] (synth :freq (deg->freq :base-freq 200 :scale scale-1 :degree deg) :mod-freq (rrand 6000 10000) :dcy 3 :out (bh (case deg -7 2 0))) (if (> (rand) 0.1) (my-malgo {:deg (diat->polydori-degree 4 (+ tr (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5 2 5}) :vel (at-i [100 100 100])}) (my-malgo {:deg (diat->polydori-degree 4 (+ tr (at-i [-6 -12 6 0 0 0]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5 2 5}) :vel (at-i [100 80])})))))

  ;; development-exploration of above (same deg seq)
  ;; poco a poco ir introduciendo la scala 5 que en paralelo con el tema actual da un contratema muy bonito y figurativo
  ;; also adding 7 works well
  (ref-rain
    :id ::4 :durs [3 2 2] :ratio 1/9
    :ref ::2
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg (at-i [
                                 (at-i [0 5 5]) 2
                                 (at-i [1 -7])
                                 (at-i [9 8])
                                 (at-i [4 -3 7 3])
                                 (at-i [6 5])
                                 (at-i [3 3 8 -5 9])
                                 (at-i [0 5 11])])
                      scale-1 (weighted {4 5 5 2})
                      scale-2 (weighted {5 3 7 0})
                      tr 0]
                  (synth
                    :freq (deg->freq :base-freq 200 :scale scale-1 :degree (+ deg tr) )
                    :mod-freq (rrand 6000 10000)
                    :dcy 3
                    :amp 0.5
                    :out (bh (case deg -7 2 0)))
                  (synth
                    :freq (deg->freq :base-freq 400 :scale scale-2 :degree deg)
                    :mod-freq (rrand 6000 10000)
                    :dcy (at-i [2 2 1 1 1 2 3])
                    :pan -1
                    :amp 0.5
                    :out (bh (case deg -7 2 0)))
                  (synth
                    :freq (deg->freq :base-freq (at-i [800 400 800 400 800 400 800 400 800 400 800]) :scale 7 :degree (+ 0 deg))
                    :mod-freq (rrand 6000 10000)
                    :dcy (at-i [2 2 1 1 1 2 3])
                    :pan 1
                    :amp 0.5
                    :out (bh (case deg -7 2 0)))
                  (when (#{0 3 5} (mod i 7))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale 7 :degree (- deg 2))
                      :mod-freq (rrand 6000 10000)
                      :dcy (at-i [2 2 1 1 1 2 3])
                      :pan 1
                      :amp 0.6
                      :out (bh (case deg -7 2 0))))
                  (if (> (rand) 0.3)
                    (my-malgo {:deg (diat->polydori-degree scale-1
                                                           (+ tr (at-i [2 2 3 2 3])
                                                              (weighted {0 5 4 4 -4 4}) deg))
                               :dur (weighted {0.1 9 1 5 2 5})
                               :vel (at-i [60 100 40])})
                    (my-malgo {:deg (diat->polydori-degree scale-2
                                                           (+ tr
                                                              (at-i [-6 6 0 -6 0]) ;; NOTE add later
                                                              (weighted {0 5 4 4 -4 4}) deg))
                               :dur (weighted {0.1 9 1 5 })
                               :vel (at-i [100 80])}))
                  )))

  ;; Estadio 4?
  ;; NOTE sucede en 11, pero viene del 9 ---- TODO cómo movernos del  4+5+7 (estadio 3) al 9 o al 11?
  (do
    (ref-rain
      :id ::4 :durs [3 2 2] :ratio 1/9
      :ref ::2
      :on-event (on-event

                  (let [synth (rand-nth synths)
                        deg (at-i [(at-i [0 15 5])
                                   (at-i [14 13])
                                   (at-i [1 -7 12])
                                   (at-i [9 8])
                                   (at-i [4 -3 7 13])
                                   (at-i [6 5 11])
                                   (at-i [3 3 8 -5 9])
                                   (at-i [0 5 11])])
                        scale-1 (at-i [11])
                        scale-2 (at-i [11])
                        tr 0]
                    (synth
                      :freq (deg->freq :base-freq 200 :scale scale-1 :degree (+ deg tr) )
                      :mod-freq (rrand 600 10000)
                      :dcy 3
                      :amp 0.5
                      :out (bh (case deg -7 2 0)))

                    (synth
                      :freq (deg->freq :base-freq (at-i [200 400]) :scale scale-2 :degree (+ deg tr) )
                      :mod-freq (rrand 6000 10000)
                      :dcy 3
                      :amp 0.5
                      :out (bh (case deg -7 2 0))))))
    (ref-rain
      :id ::4-pad :durs [5 3 3] :ratio 1/9
      :ref ::4
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (at-i [(at-i [0 5])
                                   (at-i [1 -7])
                                   (at-i [3 3 9])
                                   (at-i [0 11])])]
                    (synth
                      :freq (deg->freq :base-freq 400 :scale 11 :degree deg )
                      :mod-freq (rrand 600 10000)
                      :atk (rand 0.2)
                      :dcy 3
                      :amp (rand 0.3)
                      :out (bh (case deg -7 2 0)))

                    (synth
                      :freq (deg->freq :base-freq 800 :scale 11 :degree deg )
                      :mod-freq (rrand 6000 10000)
                      :atk 3
                      :dcy 3
                      :amp (rand 0.2)
                      :out (bh (case deg -7 2 0)))


                    #_(if (> (rand) 0.3)
                        (my-malgo {:deg (diat->polydori-degree scale-1
                                                               (+ tr (at-i [2 2 3 2 3])
                                                                  (weighted {0 5 4 4 -4 4}) deg))
                                   :dur (weighted {0.1 9 1 5 2 5})
                                   :vel (at-i [60 100 40])})
                        (my-malgo {:deg (diat->polydori-degree scale-2
                                                               (+ tr
                                                                  (at-i [-6 6 0 -6 0]) ;; NOTE add later
                                                                  (weighted {0 5 4 4 -4 4}) deg))
                                   :dur (weighted {0.1 9 1 5 })
                                   :vel (at-i [100 80])}))))))

  ;; Estadio 5?
  ;;
  (do
    (ref-rain
      :id ::4 :durs [3 2 2] :ratio 1/9
      :ref ::2
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (at-i [(at-i [10 0 15 5])
                                   (at-i [14 13])
                                   (at-i [1 -7 12])
                                   (at-i [9 8 10])
                                   (at-i [4 -3 7 13])
                                   (at-i [6 5 11])
                                   (at-i [3 3 8 -5 9])
                                   (at-i [0 5 11])])
                        ;; harmonies that go with 14: 10, 7, 4, 5 (12 just a little spice)
                        scale-1 (weighted {14 10
                                           ;; 10 2
                                           ;; 12 1
                                           })
                        scale-2 (weighted {
                                           ;; just using 14 o 10 works great
                                           ;; 14 10
                                           ;; 7 1
                                           })
                        tr -0]
                    (synth
                      :freq (deg->freq :base-freq (at-i [200 200 200 100]) :scale scale-1 :degree (+ deg tr) )
                      :mod-freq (rrand 600 10000)
                      :dcy 3
                      :amp 0.5
                      :out (bh (case deg -7 2 0)))

                    (synth
                      :freq (deg->freq :base-freq (at-i [200 400]) :scale scale-2 :degree (+ deg tr #_3) )
                      :mod-freq (rrand 6000 10000)
                      :dcy 3
                      :amp 0.5
                      :out (bh (case deg -7 2 0)))
                    )))
    (ref-rain
      :id ::4-pad :durs [5 3 3] :ratio 1/9
      :ref ::4
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (at-i [(at-i [0 5])
                                   (at-i [1 -7])
                                   (at-i [3 3 9])
                                   (at-i [0 11])])]
                    (synth
                      :freq (deg->freq :base-freq 400 :scale 14 :degree deg )
                      :mod-freq (rrand 600 10000)
                      :atk (rand 0.2)
                      :dcy 3
                      :amp (rand 0.3)
                      :out (bh (case deg -7 2 0)))

                    (synth
                      :freq (deg->freq :base-freq 800 :scale 14 :degree deg )
                      :mod-freq (rrand 6000 10000)
                      :atk 3
                      :dcy 3
                      :amp (rand 0.2)
                      :out (bh (case deg -7 2 0)))

                    
                    #_(if (> (rand) 0.3)
                        (my-malgo {:deg (diat->polydori-degree scale-1
                                                               (+ tr (at-i [2 2 3 2 3])
                                                                  (weighted {0 5 4 4 -4 4}) deg))
                                   :dur (weighted {0.1 9 1 5 2 5})
                                   :vel (at-i [60 100 40])})
                        (my-malgo {:deg (diat->polydori-degree scale-2
                                                               (+ tr
                                                                  (at-i [-6 6 0 -6 0]) ;; NOTE add later
                                                                  (weighted {0 5 4 4 -4 4}) deg))
                                   :dur (weighted {0.1 9 1 5 })
                                   :vel (at-i [100 80])}))
                    ))))

  ;; Estadio 6?
  ;; NOTE `#20` then transition to `#1`, play themes from the begining
  ;; Idea:
  ;;   start with pad alone on scale #20
  ;;   then add the other scales (harmonic cycle)
  ;;   then crescendo in `::4` (see note in `deg` definition)
  ;;        `::4` is on #20, then add #1... and perhaps #16?
  ;;   then add polytemporal `::5` (`1/8` is used now, but I also experimented with `1/13` using rhythm [8/5 1 1] (or [8 5 5]/5))
  ;;        all this is still unclear
  ;;        NOTE `::5` is the one of the themes of the begining, from the `#0` - `#2` scales.
  (let [scale* (atom 20)]
    #_(gp/stop )

    ;; `::4-pad` TEMPLATE
    #_(ref-rain :id ::4-pad :durs [5 3 3] :ratio 1/9 :ref ::4 :on-event (let [scales (concat (repeat 20 20) (repeat 20 1) (repeat 20 16))] (on-event (let [synth (rand-nth synths) deg (at-i [(at-i [0 5]) (at-i [1 -7]) (at-i [3 3 9]) (at-i [0 11])]) scale (at-i scales)] (synth :freq (deg->freq :base-freq 400 :scale scale :degree deg) :mod-freq (rrand 600 10000) :atk (rand 0.2) :dcy 3 :amp (rand 0.3) :out (bh (case deg -7 2 0))) (synth :freq (deg->freq :base-freq 800 :scale scale :degree deg) :mod-freq (rrand 6000 10000) :atk 3 :dcy 3 :amp (rand 0.2) :out (bh (case deg -7 2 0))) #_ (if (> (rand) 0.3) (my-malgo {:deg (diat->polydori-degree scale-1 (+ tr (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5 2 5}) :vel (at-i [60 100 40])}) (my-malgo {:deg (diat->polydori-degree scale-2 (+ tr (at-i [-6 6 0 -6 0]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel (at-i [100 80])}))))))

    (ref-rain
      :id ::4-pad :durs [5 3 3] :ratio 1/9
      :ref ::4
      :on-event (let [scales (concat (repeat 40 20)
                                     (repeat 40 1)
                                     (repeat 40 16)
                                     (repeat 10 7)
                                     (repeat 10 16)
                                     (repeat 40 1)
                                     (repeat 20 16)
                                     )]
                  (on-event
                    (let [synth (rand-nth synths)
                          deg (at-i [(at-i [0 5])
                                     (at-i [1 -7])
                                     (at-i [3 3 9])
                                     (at-i [0 11])])
                          scale (at-i scales)]
                      (reset! scale* scale)
                      (synth
                        :freq (deg->freq :base-freq 400 :scale scale :degree deg )
                        :mod-freq (rrand 600 10000)
                        :atk (rand 0.2)
                        :dcy 3
                        :amp (rand 0.3)
                        :out (bh (case deg -7 2 0)))

                      (synth
                        :freq (deg->freq :base-freq 800 :scale scale :degree deg )
                        :mod-freq (rrand 6000 10000)
                        :atk 3
                        :dcy 3
                        :amp (rand 0.2)
                        :out (bh (case deg -7 2 0)))


                      #_(if (> (rand) 0.3)
                          (my-malgo {:deg (diat->polydori-degree scale-1
                                                                 (+ tr (at-i [2 2 3 2 3])
                                                                    (weighted {0 5 4 4 -4 4}) deg))
                                     :dur (weighted {0.1 9 1 5 2 5})
                                     :vel (at-i [60 100 40])})
                          (my-malgo {:deg (diat->polydori-degree scale-2
                                                                 (+ tr
                                                                    (at-i [-6 6 0 -6 0]) ;; NOTE add later
                                                                    (weighted {0 5 4 4 -4 4}) deg))
                                     :dur (weighted {0.1 9 1 5 })
                                     :vel (at-i [100 80])}))
                      ))))



    ;; `::4` TEMPLATE
    #_(ref-rain :id ::4 :durs [3 2 2] :ratio 1/9 :ref ::2 :on-event (on-event (let [synth (rand-nth synths) deg (at-i [(at-i [10 0 15 5]) (at-i [14 13]) (at-i [1 -7 12]) (at-i [9 8 10]) (at-i [4 -3 7 13]) (at-i [6 5 11]) (at-i [3 3 8 -5 9]) (at-i [0 5 11]) (at-i [10 15 11 15 16 17 18 19 20]) (at-i [10 5 11 12 13 14 15 16 17 18 19 20]) (at-i [10 5 9 8 11 12 13 14 15 16 17 18 19 20])]) scale-1 (weighted {20 5}) scale-2 (weighted {20 10 1 10}) tr -0 amp 0.2] (when-not (#{0 1} (mod i 30)) (synth :freq (deg->freq :base-freq 200 :scale scale-1 :degree (+ deg tr)) :mod-freq (rrand 600 10000) :dcy 3 :amp (* amp (rrand 0.5 0.9)) :out (bh (case deg -7 2 0)))) (when (#{0 3} (mod i 7)) (synth :freq (deg->freq :base-freq (at-i [200]) :scale scale-2 :degree (+ deg tr 3)) :mod-freq (rrand 6000 10000) :dcy 3 :amp (* amp 1.1 (rrand 0.5 0.9)) :out (bh (case deg -7 2 0)))))))

    (ref-rain
      :id ::4 :durs [3 2 2] :ratio 1/9
      :ref ::2
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (at-i [(at-i [10 0 15 5])
                                   (at-i [14 13])
                                   (at-i [1 -7 12])
                                   (at-i [9 8 10])
                                   (at-i [4 -3 7 13])
                                   (at-i [6 5 11])
                                   (at-i [3 3 8 -5 9])
                                   (at-i [0 5 11])
                                   ;; NOTE add this or remove ?
                                   (at-i [10 15 11 15 16 17 18 19 20])
                                   (at-i [10 5 11 12 13 14 15 16 17 18 19 20])
                                   (at-i [10 5 9 8 11 12 13 14 15 16 17 18 19 20])
                                   ])

                        scale-1  (weighted (merge
                                             {
                                              20 5
                                              1 5
                                              16 1
                                              }
                                             {@scale* 15}))
                        scale-2  (weighted (merge {
                                                   20 10
                                                   1 10
                                                   16 1
                                                   }
                                                  {@scale* 20}))
                        tr -0
                        amp 0.4]
                    (when-not (#{0 1} (mod i 30))
                      (synth
                        :freq (deg->freq :base-freq 200 :scale scale-1 :degree (+ deg tr) )
                        :mod-freq (rrand 600 10000)
                        :dcy (weighted {3 6 7 1})
                        :pan (rrand -1.0 1)
                        :amp (* amp (rrand 0.5 0.9))
                        :out (bh (case deg -7 2 0))))

                    (when (#{0 3} (mod i 7))
                      (synth
                        :freq (deg->freq :base-freq (at-i [200]) :scale scale-2 :degree (+ deg tr 3) )
                        :mod-freq (rrand 6000 10000)
                        :dcy 3
                        :pan (rrand -1.0 1)
                        :amp (* amp 1.1 (rrand 0.5 0.9))
                        :out (bh (case deg -7 2 0))))
                    )))

    (gp/stop ::5)
    ;;  NOTE start when on scale #1
    ;; `::5` TEMPLATE:  explorar polyrritmos en los bajos, i.e. 1/8
    #_(ref-rain :id ::5 :ref ::4 :durs [3 2 2] :ratio 1/8 :on-event (on-event (let [synth (rand-nth synths) deg #_ (at-i [0 2 1 (at-i [2 3]) 4]) (at-i [(at-i [0 5 5]) 2 (at-i [1 -7]) (at-i [8 2 8]) (at-i [4 7 3]) (at-i [6 5]) (at-i [3 3 8 9])])] #_ (synth :freq (deg->freq :base-freq 100 :scale 1 :degree deg) :mod-freq (rrand 6000 10000) :pan (mempan (mod deg 2)) :amp (rrange 0.45 0.7) :out (bh 0)) (synth :freq (deg->freq :base-freq 200 :scale 1 :degree deg) :mod-freq (rrand 6000 10000) :pan (mempan (mod deg 2)) :amp (rrange 0.45 0.7) :out (bh 0)) #_ (when (> (rand) 0.6) (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [0 -4 0 -4 -3]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100})) #_ (if (> (rand) 0.5) (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [-4 0 -4 -4 0 -4 0]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100})) #_ (when (> (rand) 0.4)) (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel 100}) #_ (my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))

    #_(ref-rain
      :id ::5
      :ref ::4
      :durs [6] :ratio 1/4 ;; explorar polyrritmos en los bajos, i.e. 1/8
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (at-i [0 2 1 (at-i [2 3]) 4])
                        #_(at-i [(at-i [0 5 5])
                                 2
                                 (at-i [1 -7])
                                 (at-i [8 2 8])
                                 (at-i [4 7 3])
                                 (at-i [6 5])
                                 (at-i [3 3 8 9])])]
                    (synth
                      :freq (deg->freq :base-freq 100 :scale @scale* :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :pan (mempan (mod deg 2))
                      :amp (at-i [0.7 0.5 0.7])
                      :out (bh 0))
                    (synth
                      :freq (deg->freq :base-freq 200 :scale @scale* :degree (+ 3 deg))
                      :mod-freq (rrand 6000 10000)
                      :pan (mempan (mod deg 2))
                      :dcy (if (= dur 3) 6 2)
                      :amp (at-i [0.7 0.5 0.7])
                      :out (bh 0))
                    ;; TODO agregar silencions con when-not
                    #_(when (> (rand ) 0.6)
                        (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [0 -4 0 -4 -3]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                    #_(if (> (rand) 0.5)
                        (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [-4  0 -4 -4 0 -4 0]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                    #_(when (> (rand) 0.4))
                    (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4 }) deg)) :dur (weighted {0.1 9 1 5}) :vel 100})
                    #_(my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))
    (gp/stop ::5b)
    (ref-rain
      :id ::5b
      :ref ::4
      :durs (concat (repeat 20 3)
                    (repeat 1 6)
                    (repeat 26 3)
                    (repeat 1 6)) :ratio 1/8 ;; explorar polyrritmos en los bajos, i.e. 1/8
      :on-event (let [transps (concat (repeat 8 0)
                                      (repeat 2 8)
                                      (repeat 3 1)
                                      (repeat 2 8)
                                      (repeat 3 1)
                                      (repeat 2 8)
                                      (repeat 3 1))]
                  (on-event
                    (let [synth (rand-nth synths)
                          deg (+ (at-i [(at-i [0 5 5])
                                        2
                                        (at-i [1 -7])
                                        #_(at-i [1 2 8])
                                        #_(at-i [4 7 3])
                                        #_(at-i [6 5])
                                        #_(at-i [3 3 8 9])])
                                 (at-i transps))
                          pan (rrand -1.0 1)]
                      (when (#{3 5 0 1 2 4 6} (mod i 7))
                        (synth
                          :freq (deg->freq :base-freq 400 :scale @scale* :degree deg)
                          :mod-freq (rrand 6000 10000)
                          :pan pan
                          :dcy 2
                          :amp (at-i [0.9 0.8 0.7 0.5])
                          :out (bh 0))
                        (synth
                          :freq (deg->freq :base-freq 800 :scale @scale* :degree (+ 3 deg))
                          :mod-freq (rrand 6000 10000)
                          :pan pan
                          :dcy 2
                          :amp (at-i [0.9 0.8 0.7 0.5])
                          :out (bh 0)))
                      ;; TODO agregar silencions con when-not
                      #_(when (> (rand ) 0.6)
                          (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [0 -4 0 -4 -3]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                      #_(if (> (rand) 0.5)
                          (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [-4  0 -4 -4 0 -4 0]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                      #_(when (> (rand) 0.4))
                      (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4 }) deg)) :dur (weighted {0.1 9 1 5}) :vel 100})
                      #_(my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100})))))
    (ref-rain
      :id ::5c
      :ref ::4
      :durs (concat (repeat 7 2)
                    (repeat 1 3)
                    (repeat 6 2)
                    (repeat 1 5)) :ratio 1/8 ;; explorar polyrritmos en los bajos, i.e. 1/8
      :on-event (let [transps (concat (repeat 8 0)
                                      (repeat 2 8)
                                      (repeat 3 1)
                                      (repeat 2 8)
                                      (repeat 3 1)
                                      (repeat 2 8)
                                      (repeat 3 1))]
                  (on-event
                    (let [synth (rand-nth synths)
                          deg (+ (at-i [(at-i [0 5 5])
                                        2
                                        (at-i [1 -7])
                                        (at-i [1 2 8])
                                        #_(at-i [4 7 3])
                                        #_(at-i [6 5])
                                        #_(at-i [3 3 8 9])])
                                 (at-i transps))
                          pan (rrand -1.0 1)]
                      (when ((set (concat (range 7)
                                          (range 21 38)
                                          (range  14 19)))
                             (mod i 35))
                        (if (odd? index)
                          (synth
                            :freq (deg->freq :base-freq 1600 :scale @scale* :degree deg)
                            :mod-freq (rrand 6000 10000)
                            :pan pan
                            :dcy 2
                            :amp (at-i [0.2])
                            :out (bh 0))
                          (synth
                            :freq (deg->freq :base-freq 3200 :scale @scale* :degree (+ 3 deg))
                            :mod-freq (rrand 6000 10000)
                            :pan pan
                            :dcy 2
                            :amp (at-i [0.2])
                            :out (bh 0))))

                      (synth
                        :freq (deg->freq :base-freq (at-i [3200]) :scale @scale* :degree (at-i (concat (repeat 0 40)
                                                                                                       (range -16 0))))
                          :mod-freq (rrand 6000 10000)
                          :pan pan
                          :dcy 2
                          :amp (at-i [0.2])
                          :out (bh 0)))))))

  ;; NOTE adelgazar todo
  ;; Transición a Estadio 7
  (let [scale* (atom 20)]
    #_(gp/stop )

    ;; `::4-pad` TEMPLATE
    #_(ref-rain :id ::4-pad :durs [5 3 3] :ratio 1/9 :ref ::4 :on-event (let [scales (concat (repeat 20 20) (repeat 20 1) (repeat 20 16))] (on-event (let [synth (rand-nth synths) deg (at-i [(at-i [0 5]) (at-i [1 -7]) (at-i [3 3 9]) (at-i [0 11])]) scale (at-i scales)] (synth :freq (deg->freq :base-freq 400 :scale scale :degree deg) :mod-freq (rrand 600 10000) :atk (rand 0.2) :dcy 3 :amp (rand 0.3) :out (bh (case deg -7 2 0))) (synth :freq (deg->freq :base-freq 800 :scale scale :degree deg) :mod-freq (rrand 6000 10000) :atk 3 :dcy 3 :amp (rand 0.2) :out (bh (case deg -7 2 0))) #_ (if (> (rand) 0.3) (my-malgo {:deg (diat->polydori-degree scale-1 (+ tr (at-i [2 2 3 2 3]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5 2 5}) :vel (at-i [60 100 40])}) (my-malgo {:deg (diat->polydori-degree scale-2 (+ tr (at-i [-6 6 0 -6 0]) (weighted {0 5 4 4 -4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel (at-i [100 80])}))))))

    (ref-rain
      :id ::4-pad :durs [5 3 3] :ratio 1/9
      :ref ::4
      :on-event (let [scales (concat #_(repeat 40 20)
                                     #_(repeat 40 1)
                                     #_(repeat 40 16)
                                     (repeat 10 7)
                                     #_(repeat 10 16)
                                     #_(repeat 40 1)
                                     #_(repeat 20 16)
                                     )]
                  (on-event
                    (let [synth (rand-nth synths)
                          deg (at-i [(at-i [0 5])
                                     (at-i [1 -7])
                                     (at-i [3 3 9])
                                     (at-i [0 11])])
                          scale (at-i scales)]
                      (reset! scale* scale)
                      #_(synth
                        :freq (deg->freq :base-freq 400 :scale scale :degree deg )
                        :mod-freq (rrand 600 10000)
                        :atk (rand 0.2)
                        :dcy 3
                        :amp (rand 0.3)
                        :out (bh (case deg -7 2 0)))

                      #_(synth
                        :freq (deg->freq :base-freq 800 :scale scale :degree deg )
                        :mod-freq (rrand 6000 10000)
                        :atk 3
                        :dcy 3
                        :amp (rand 0.2)
                        :out (bh (case deg -7 2 0)))


                      #_(if (> (rand) 0.3)
                          (my-malgo {:deg (diat->polydori-degree scale-1
                                                                 (+ tr (at-i [2 2 3 2 3])
                                                                    (weighted {0 5 4 4 -4 4}) deg))
                                     :dur (weighted {0.1 9 1 5 2 5})
                                     :vel (at-i [60 100 40])})
                          (my-malgo {:deg (diat->polydori-degree scale-2
                                                                 (+ tr
                                                                    (at-i [-6 6 0 -6 0]) ;; NOTE add later
                                                                    (weighted {0 5 4 4 -4 4}) deg))
                                     :dur (weighted {0.1 9 1 5 })
                                     :vel (at-i [100 80])}))
                      ))))



    ;; `::4` TEMPLATE
    #_(ref-rain :id ::4 :durs [3 2 2] :ratio 1/9 :ref ::2 :on-event (on-event (let [synth (rand-nth synths) deg (at-i [(at-i [10 0 15 5]) (at-i [14 13]) (at-i [1 -7 12]) (at-i [9 8 10]) (at-i [4 -3 7 13]) (at-i [6 5 11]) (at-i [3 3 8 -5 9]) (at-i [0 5 11]) (at-i [10 15 11 15 16 17 18 19 20]) (at-i [10 5 11 12 13 14 15 16 17 18 19 20]) (at-i [10 5 9 8 11 12 13 14 15 16 17 18 19 20])]) scale-1 (weighted {20 5}) scale-2 (weighted {20 10 1 10}) tr -0 amp 0.2] (when-not (#{0 1} (mod i 30)) (synth :freq (deg->freq :base-freq 200 :scale scale-1 :degree (+ deg tr)) :mod-freq (rrand 600 10000) :dcy 3 :amp (* amp (rrand 0.5 0.9)) :out (bh (case deg -7 2 0)))) (when (#{0 3} (mod i 7)) (synth :freq (deg->freq :base-freq (at-i [200]) :scale scale-2 :degree (+ deg tr 3)) :mod-freq (rrand 6000 10000) :dcy 3 :amp (* amp 1.1 (rrand 0.5 0.9)) :out (bh (case deg -7 2 0)))))))

    (ref-rain
      :id ::4 :durs [3 2 2] :ratio 1/9
      :ref ::2
      :on-event (on-event
                  #_(let [synth (rand-nth synths)
                          deg (at-i [(at-i [10 0 15 5])
                                     (at-i [14 13])
                                     (at-i [1 -7 12])
                                     (at-i [9 8 10])
                                     (at-i [4 -3 7 13])
                                     (at-i [6 5 11])
                                     (at-i [3 3 8 -5 9])
                                     (at-i [0 5 11])
                                     ;; NOTE add this or remove ?
                                     (at-i [10 15 11 15 16 17 18 19 20])
                                     (at-i [10 5 11 12 13 14 15 16 17 18 19 20])
                                     (at-i [10 5 9 8 11 12 13 14 15 16 17 18 19 20])
                                     ])

                          scale-1  (weighted (merge
                                               {
                                                20 5
                                                1 5
                                                16 1
                                                }
                                               {@scale* 15}))
                          scale-2  (weighted (merge {
                                                     20 10
                                                     1 10
                                                     16 1
                                                     }
                                                    {@scale* 20}))
                          tr -0
                          amp 0.4]
                      (when-not (#{0 1} (mod i 30))
                        (synth
                          :freq (deg->freq :base-freq 200 :scale scale-1 :degree (+ deg tr) )
                          :mod-freq (rrand 600 10000)
                          :dcy (weighted {3 6 7 1})
                          :pan (rrand -1.0 1)
                          :amp (* amp (rrand 0.5 0.9))
                          :out (bh (case deg -7 2 0))))

                      (when (#{0 3} (mod i 7))
                        (synth
                          :freq (deg->freq :base-freq (at-i [200]) :scale scale-2 :degree (+ deg tr 3) )
                          :mod-freq (rrand 6000 10000)
                          :dcy 3
                          :pan (rrand -1.0 1)
                          :amp (* amp 1.1 (rrand 0.5 0.9))
                          :out (bh (case deg -7 2 0))))
                      )))

    (gp/stop ::5)
    ;;  NOTE start when on scale #1
    ;; `::5` TEMPLATE:  explorar polyrritmos en los bajos, i.e. 1/8
    #_(ref-rain :id ::5 :ref ::4 :durs [3 2 2] :ratio 1/8 :on-event (on-event (let [synth (rand-nth synths) deg #_ (at-i [0 2 1 (at-i [2 3]) 4]) (at-i [(at-i [0 5 5]) 2 (at-i [1 -7]) (at-i [8 2 8]) (at-i [4 7 3]) (at-i [6 5]) (at-i [3 3 8 9])])] #_ (synth :freq (deg->freq :base-freq 100 :scale 1 :degree deg) :mod-freq (rrand 6000 10000) :pan (mempan (mod deg 2)) :amp (rrange 0.45 0.7) :out (bh 0)) (synth :freq (deg->freq :base-freq 200 :scale 1 :degree deg) :mod-freq (rrand 6000 10000) :pan (mempan (mod deg 2)) :amp (rrange 0.45 0.7) :out (bh 0)) #_ (when (> (rand) 0.6) (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [0 -4 0 -4 -3]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100})) #_ (if (> (rand) 0.5) (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [-4 0 -4 -4 0 -4 0]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100})) #_ (when (> (rand) 0.4)) (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4}) deg)) :dur (weighted {0.1 9 1 5}) :vel 100}) #_ (my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))

    #_(ref-rain
        :id ::5
        :ref ::4
        :durs [6] :ratio 1/4 ;; explorar polyrritmos en los bajos, i.e. 1/8
        :on-event (on-event
                    (let [synth (rand-nth synths)
                          deg (at-i [0 2 1 (at-i [2 3]) 4])
                          #_(at-i [(at-i [0 5 5])
                                   2
                                   (at-i [1 -7])
                                   (at-i [8 2 8])
                                   (at-i [4 7 3])
                                   (at-i [6 5])
                                   (at-i [3 3 8 9])])]
                      (synth
                        :freq (deg->freq :base-freq 100 :scale @scale* :degree deg)
                        :mod-freq (rrand 6000 10000)
                        :pan (mempan (mod deg 2))
                        :amp (at-i [0.7 0.5 0.7])
                        :out (bh 0))
                      (synth
                        :freq (deg->freq :base-freq 200 :scale @scale* :degree (+ 3 deg))
                        :mod-freq (rrand 6000 10000)
                        :pan (mempan (mod deg 2))
                        :dcy (if (= dur 3) 6 2)
                        :amp (at-i [0.7 0.5 0.7])
                        :out (bh 0))
                      ;; TODO agregar silencions con when-not
                      #_(when (> (rand ) 0.6)
                          (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [0 -4 0 -4 -3]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                      #_(if (> (rand) 0.5)
                          (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [-4  0 -4 -4 0 -4 0]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                      #_(when (> (rand) 0.4))
                      (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4 }) deg)) :dur (weighted {0.1 9 1 5}) :vel 100})
                      #_(my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100}))))
    (gp/stop ::5b)
    (ref-rain
      :id ::5b
      :ref ::4
      :durs (concat (repeat 20 3)
                    (repeat 1 6)
                    (repeat 26 3)
                    (repeat 1 6)) :ratio 1/8 ;; explorar polyrritmos en los bajos, i.e. 1/8
      :on-event (let [transps (concat (repeat 8 0)
                                      (repeat 2 8)
                                      (repeat 3 1)
                                      (repeat 2 8)
                                      (repeat 3 1)
                                      (repeat 2 8)
                                      (repeat 3 1))]
                  (on-event
                    #_(let [synth (rand-nth synths)
                            deg (+ (at-i [(at-i [0 5 5])
                                          2
                                          (at-i [1 -7])
                                          #_(at-i [1 2 8])
                                          #_(at-i [4 7 3])
                                          #_(at-i [6 5])
                                          #_(at-i [3 3 8 9])])
                                   (at-i transps))
                            pan (rrand -1.0 1)]
                        (when (#{3 5 0 1 2 4 6} (mod i 7))
                          (synth
                            :freq (deg->freq :base-freq 400 :scale @scale* :degree deg)
                            :mod-freq (rrand 6000 10000)
                            :pan pan
                            :dcy 2
                            :amp (at-i [0.9 0.8 0.7 0.5])
                            :out (bh 0))
                          (synth
                            :freq (deg->freq :base-freq 800 :scale @scale* :degree (+ 3 deg))
                            :mod-freq (rrand 6000 10000)
                            :pan pan
                            :dcy 2
                            :amp (at-i [0.9 0.8 0.7 0.5])
                            :out (bh 0)))
                        ;; TODO agregar silencions con when-not
                        #_(when (> (rand ) 0.6)
                            (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [0 -4 0 -4 -3]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                        #_(if (> (rand) 0.5)
                            (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [-4  0 -4 -4 0 -4 0]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                        #_(when (> (rand) 0.4))
                        (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4 }) deg)) :dur (weighted {0.1 9 1 5}) :vel 100})
                        #_(my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100})))))
    (ref-rain
      :id ::5c
      :ref ::4
      :durs (concat (repeat 7 2)
                    (repeat 1 3)
                    (repeat 6 2)
                    (repeat 1 5)) :ratio 1/8 ;; explorar polyrritmos en los bajos, i.e. 1/8
      :on-event (let [transps (concat (repeat 8 0)
                                      (repeat 2 8)
                                      (repeat 3 1)
                                      (repeat 2 8)
                                      (repeat 3 1)
                                      (repeat 2 8)
                                      (repeat 3 1))]
                  (on-event
                    #_(let [synth (rand-nth synths)
                            deg (+ (at-i [(at-i [0 5 5])
                                          2
                                          (at-i [1 -7])
                                          (at-i [1 2 8])
                                          #_(at-i [4 7 3])
                                          #_(at-i [6 5])
                                          #_(at-i [3 3 8 9])])
                                   (at-i transps))
                            pan (rrand -1.0 1)]
                        (when ((set (concat (range 7)
                                            (range 21 38)
                                            (range  14 19)))
                               (mod i 35))
                          (if (odd? index)
                            (synth
                              :freq (deg->freq :base-freq 1600 :scale @scale* :degree deg)
                              :mod-freq (rrand 6000 10000)
                              :pan pan
                              :dcy 2
                              :amp (at-i [0.2])
                              :out (bh 0))
                            #_(synth
                                :freq (deg->freq :base-freq 3200 :scale @scale* :degree (+ 3 deg))
                                :mod-freq (rrand 6000 10000)
                                :pan pan
                                :dcy 2
                                :amp (at-i [0.1])
                                :out (bh 0))))

                        #_(synth
                            :freq (deg->freq :base-freq (at-i [3200]) :scale @scale* :degree (at-i (concat (repeat 0 40)
                                                                                                           (range -16 0))))
                            :mod-freq (rrand 6000 10000)
                            :pan pan
                            :dcy 2
                            :amp (at-i [0.1])
                            :out (bh 0))))))


    ;; NOTE adelgazar todo lo de arriba

    (ref-rain
      :id ::5d
      :durs [3 2 2] :ratio 1/10 ;; explorar polyrritmos en los bajos, i.e. 1/8
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (+ (at-i [(at-i [0 5 5])
                                      2
                                      #_(at-i [1 -7])
                                      (at-i [1 5 2])
                                      2
                                      #_(at-i [4 7 3])
                                      #_(at-i [6 5])
                                      #_(at-i [3 3 8 9])])
                               #_(at-i transps))
                        pan (rrand -1.0 1)]
                    (synth
                      :freq (deg->freq :base-freq 200 :scale @scale* :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :pan pan
                      :dcy 2
                      :amp (at-i [0.6])
                      :out (bh 0))
                    ;; NOTE add later
                    (my-malgo {:deg (diat->polydori-degree @scale* (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4 }) deg)) :dur (weighted {0.1 9 1 5}) :vel 100})))))

  ;; estadio 7
  (let [scale* (atom 7)]
    (ref-rain
      :id ::5d
      :durs [3 2 2] :ratio 1/10
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (+ (at-i [(at-i [0 5 5 0 5 6])
                                      2
                                      #_(at-i [1 -7])
                                      (at-i [1 5 2])
                                      (at-i [2 2 2 3 2 2 3])
                                      #_(at-i [4 7 3])
                                      #_(at-i [6 5])
                                      #_(at-i [3 3 8 9])])
                               #_(at-i transps))
                        pan (rrand -1.0 1)]
                    (synth
                      :freq (deg->freq :base-freq 200 :scale @scale* :degree deg)
                      :mod-freq (rrand 6000 10000)
                      :pan pan
                      :amp (at-i [0.6])
                      :out (bh 0))
                    ;; NOTE add later
                    (my-malgo {:deg (diat->polydori-degree @scale* (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4 }) deg)) :dur (weighted {0.1 9 1 5}) :vel 100})

                    ;; NOTE evolve: freq 200 -> weighted ; deg +1 -> lo que hay
                    (synth
                      :freq (deg->freq :base-freq (weighted {200 5 400 200}) :scale 10 :degree (+ deg 4 ((rand-nth [+ -]) (at-i [1 1 2 1 3]))))
                      :mod-freq (rrand 6000 10000)
                      :pan pan
                      :amp (at-i [0.6])
                      :out (bh 0))
                    (my-malgo {:deg (diat->polydori-degree 10 (+ deg 4 ((rand-nth [+ -]) (at-i [1 1 2 1 3])))) :dur (weighted {0.1 9 1 5}) :vel 100}))))
  
    (ref-rain
      :id ::5e
      :durs (flatten [(repeat 3 [3 2 2 3 2 2 3 2 2 3 2 2 3 4 3 4 7])
                      [3 2]
                      [3 2 2 3 2 2 2 7]]) :ratio 1/10
      :on-event (on-event
                  (let [synth (rand-nth synths)
                        deg (+ (at-i [(at-i [0 5 5 0 5 6])
                                      2
                                      (at-i [1 -7])
                                      (at-i [1 5 2])
                                      (at-i [2 2 2 3 2 2 3])
                                      (at-i [4 7 3])
                                      (at-i [6 5])
                                      (at-i [3 3 8 9])])
                               #_(at-i transps))
                        pan (rrand -1.0 1)]
                    (if (> 0.8 (rand))
                      (synth
                        :freq (deg->freq :base-freq 800 :scale @scale* :degree deg)
                        :mod-freq (rrand 6000 10000)
                        :pan pan
                        :amp (at-i [0.6])
                        :out (bh 0))
                      (do
                        (synth
                          :freq (deg->freq :base-freq 800 :scale 14 :degree deg)
                          :mod-freq (rrand 6000 10000)
                          :pan pan
                          :amp (at-i [0.6])
                          :out (bh 0))
                        (when (#{0 3} (mod i 5))
                          (synth
                            :freq (deg->freq :base-freq 800 :scale 5 :degree (+ 2 deg))
                            :mod-freq (rrand 6000 10000)
                            :pan pan
                            :dcy 3
                            :amp (at-i [0.6])
                            :out (bh 0)))))
                    ;; NOTE add later
                    (when (#{0 3} (mod i 5))
                      (synth
                        :freq (deg->freq :base-freq 800 :scale 5
                                         :degree (+ (at-i [2 2 3 2 3])
                                                    (weighted {0 5 4 4 }) deg))
                        :mod-freq (rrand 6000 10000)
                        :pan pan
                        :dcy 3
                        :amp (at-i [0.6])
                        :out (bh 0))
                      (my-malgo {:deg (diat->polydori-degree 5
                                                             (+ (at-i [2 2 3 2 3])
                                                                (weighted {0 5 4 4 }) deg))
                                 :dur (weighted {0.1 9 1 5}) :vel 100})
                      (my-malgo {:deg (diat->polydori-degree 5
                                                             (+ -2 (at-i [2 2 3 2 3])
                                                                (weighted {0 5 4 4 }) deg))
                                 :dur (weighted {0.1 9 1 5}) :vel 100}))

                    ;; NOTE evolve: freq 200 -> weighted ; deg +1 -> lo que hay
                    #_(synth
                        :freq (deg->freq :base-freq (weighted {200 5 400 200}) :scale 10 :degree (+ deg 4 ((rand-nth [+ -]) (at-i [1 1 2 1 3]))))
                        :mod-freq (rrand 6000 10000)
                        :pan pan
                        :amp (at-i [0.6])
                        :out (bh 0))
                    #_(my-malgo {:deg (diat->polydori-degree 10 (+ deg 4 ((rand-nth [+ -]) (at-i [1 1 2 1 3])))) :dur (weighted {0.1 9 1 5}) :vel 100})))))

  :7d-perc)

(comment
  (init!)
  (gp/stop)
  (all-notes-off sink)
  )
