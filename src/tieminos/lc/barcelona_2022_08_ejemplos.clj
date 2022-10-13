(ns tieminos.lc.barcelona-2022-08-ejemplos
  (:require
   [erv.cps.core :as cps]
   [erv.edo.core :as edo]
   [erv.mos.mos :as mos]
   [erv.scale.core :as scale]
   [tieminos.synths :as syn]
   [tieminos.utils :refer [xrepeat wrap-at]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain stop]]
   [time-time.standard :refer [rrand]]
   [overtone.core :as o]))

(comment
  (ref-rain
   :id :ejemplo/serie-armonica-1
   :durs [1/2]
   :tempo 180
   :on-event (on-event
              (let [serie (range 1 17)]                    ;; armonicos 1 - 16
                (syn/soft-saw :freq (* 100 (at-index serie)) ;; frecuencia base * # de armónico
                              :dcy 2))))

  (stop)

  (ref-rain
   :id :ejemplo/serie-armonica-1
   :durs [1/2]
   :tempo 180
   :on-event (on-event
              (let [serie (range 1 17)]                    ;; armonicos 1 - 16
                (syn/soft-saw :freq (* 100 (at-index serie)) ;; frecuencia base * # de armónico
                              :dcy 2))))
  (stop)

  (ref-rain
   :id :ejemplo/serie-armonica-2-melodia
   :durs [1/4]
   :tempo 60
   :on-event (on-event
              (let [armonico 12
                    serie (range armonico (inc (* 2 armonico)))]
                (syn/soft-saw :freq (/ (* 261 (at-index serie)) ;; se puede cambiar at-index por rand-nth, para generar melodías
                                       armonico)
                              :amp 0.7
                              :atk 0.1
                              :dcy 0.5 #_(max 2 dur)))))
  (stop)

  (ref-rain
   :id :ejemplo/serie-armonica-3-acordes
   :durs [5 5 5 6]
   :tempo 60
   :on-event (on-event
              (let [armonico 5
                    serie (range armonico (inc (* 2 armonico)))]
                (doall (for [n [0 3 5]] ;; acorde: grados 0 3 5 a partir de grado de la escala
                         (syn/soft-saw :freq (/ (* 200
                                                   (wrap-at (+ (at-index [0 1 3 2]) ;; grado de la escala
                                                               n) ;; nota del acorde
                                                            serie)
                                                   (rand-nth [1 2 1/2 4]) ;; seleccionar octava de cada nota al azar
                                                   )
                                                armonico)
                                       :amp 0.2
                                       :atk 4
                                       :dcy 4))))))
  (stop)

  (let [scale (:scale (edo/from-pattern
                        ;; Escala diatonica 5L2s
                       #_[2 2 1 2 2 2 1] ;; 12edo
                       #_[5 5 3 5 5 5 3]
                       #_[5 5 3 5 5 5 3] ;; 31edo
                       #_[4 4 1 4 4 4 1] ;; 22edo
                        ;; Antidiatonica 2L5s
                       #_[2 2 6 2 2 2 6] ;; 22edo
                       #_[2 2 3 2 2 2 3] ;;16edo
                        ;; OTROS Temperamentos
                        ;; miracle[7] 3L4s
                       #_[5 4 5 4 5 4 4] ;; 31 edo
                       #_[5 3 5 3 5 3 3] ;; 27 edo
                       #_[6 1 6 1 6 1 1] ;; 22 edo
                        ;; procupine[7] 1L6s
                       [3 3 3 3 3 3 4]  ;; 22edo
                       #_[3 3 3 3 3 3 7] ;; 25edo
                       #_[4 4 4 4 4 4 7] ;;
                       ))]
    (ref-rain
     :id :ejemplo/mos-1
     :durs (repeat (inc (count scale)) 1)
     :tempo 280
     :ratio 1
      ;; :loop? false
     :on-event (on-event
                (let [chord (xrepeat [4]
                                     [[0 3 5 7]
                                      [-1 2 5 6]
                                      [-2 2 4 6]
                                      [-3 3 5 6]
                                      [-4 2 5 7]])]
                  (syn/soft-saw
                   :freq (scale/deg->freq scale 200 #_(mod index 8)
                                          (at-index chord))
                   :amp 0.2
                   :atk 0.1
                   :dcy 1)))))
  (stop)

  ;; improv
  (let [scale (:scale (edo/from-pattern [3 3 3 3 3 3 4]))]
    (ref-rain
     :id :ejemplo/mos-2
     :durs (repeat (inc (count scale)) 1)
     :tempo 280
     :ratio 1
      ;; :loop? false
     :on-event (on-event
                #_(syn/soft-saw
                   :freq (scale/deg->freq scale 200
                                          (at-index
                                           (xrepeat [1]
                                                    [21 20])))
                   :amp (at-index [0.2 0.3  0.1 0.5])
                   :atk 0.1
                   :dcy 1)
                (syn/soft-saw
                 :freq (scale/deg->freq scale (at-index [100])
                                        (at-index
                                         (xrepeat [1]
                                                  [8])))
                 :amp (at-index [0.2 0.3 0.2 0.1])
                 :atk 0.1
                 :dcy 1))))
  (stop)

  (let [hexany (cps/make 2 [1 3 5 7])
        scale (:scale hexany)
        last-note (atom (rand-nth scale))
        graph (-> hexany :graphs :full)
        get-next-note (fn [last-note]
                        (-> graph
                            (get last-note) vec
                            rand-nth))]
    (ref-rain
     :id :ejemplo/cps-1
     :durs [1/4]
     :tempo 60
     :ratio 1
      ;; :loop? false
     :on-event (on-event
                (case (mod index 5)
                  0
                  ((rand-nth [syn/sharp-plate syn/low])
                   :freq (* 400
                            (at-index (xrepeat [5 3 1] [4 2 3 1]))
                            (:bounded-ratio (swap! last-note get-next-note)))
                   :amp (at-index [0.15 0.1 0.16 0.15 0.06])
                   :atk (rrand 2.0 4)
                   :dcy (rrand 5.0 7)
                     ;; :mod-amp (+ 0.1 (* (rand-int 120) 140))
                   :mod-freq (* (rand-nth (map :ratio scale)) 400))
                  nil)
                (syn/short-plate
                 :freq (scale/deg->freq scale 200
                                        (+ (at-index [15 13 17 18 9 11]) ;; arpegio
                                           (at-index (xrepeat [16 15 14 13 12 11]
                                                                 ;; transposiciones
                                                              [0 1 0 1 2 0 3 1 4 1 5]))))
                 :amp 0.1
                 :atk 2
                 :dcy 1
                 :mod-amp (+ 0.1 (* (rand-int 120) 140))
                 :mod-freq (* (rand-nth (map :ratio scale)) 400))
                (syn/short-plate
                 :freq (scale/deg->freq scale 200
                                        (+ (at-index [15 13 17 18 9 11]) ;; arpegio
                                           (at-index (xrepeat [16 15 14 13 12 11]
                                                                 ;; transposiciones
                                                              [0 1 0 1 2 0 3 1 4 1 5]))))
                 :amp 0.2
                 :atk 0.2
                 :dcy 1
                 :mod-amp (+ 0.1 (* (rand-int 120) 140))
                 :mod-freq (* (rand-nth (map :ratio scale)) 400)))))

  (stop))

(comment

  (o/defsynth)

  (let [hexany (cps/make 2 [1 3 5 7 9])
        scale (:scale hexany)
        last-note (atom (rand-nth scale))
        graph (-> hexany :graphs :full)
        get-next-note (fn [last-note]
                        (-> graph
                            (get last-note) vec
                            rand-nth))]
    (ref-rain
     :id :ejemplo/cps-1
     :durs [1/4]
     :tempo 60
     :ratio 1
      ;; :loop? false
     :on-event (on-event
                ((rand-nth [syn/low])
                 :freq (scale/deg->freq scale 266.6
                                        (at-index [0])
                                        :period -1)
                 :amp (at-index [0.15 0.1 0.16 0.15 0.06])
                 :atk (rrand 2.0 4)
                 :dcy (rrand 5.0 7)
                   ;; :mod-amp (+ 0.1 (* (rand-int 120) 140))
                 :mod-freq (* (rand-nth (map :ratio scale)) 400))))))
