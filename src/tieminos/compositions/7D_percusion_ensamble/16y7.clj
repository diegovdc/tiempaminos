(ns tieminos.compositions.7D-percusion-ensamble.16y7)

(comment
  (ref-rain
    :id ::5
    :ref ::4
    :durs [3 2 2 ] :ratio 1/9 ;; explorar polyrritmos en los bajos, i.e. 1/8
    :on-event (on-event
                (let [synth (rand-nth synths)
                      deg #_(at-i [0 2 1 (at-i [2 3]) 4])
                      (+ 0 (at-i [(at-i [0 5 5])
                                  2
                                  (at-i [1 -7])
                                  #_(at-i [8 2 8])
                                  #_(at-i [4 7 3])
                                  #_(at-i [6 5])
                                  #_(at-i [3 3 8 9])]))]
                  (if (odd? i)
                    (synth
                      :freq (deg->freq :base-freq 200 :scale 16 :degree (+ #_-1 deg))
                      :mod-freq (rrand 6000 10000)
                      :pan (rrand -1.0 1)
                      :dcy (rrand 1 3)
                      :amp (rrange 0.45 0.7)
                      :out (bh 0))

                    (synth
                      :freq (deg->freq :base-freq 200 :scale 7 :degree (+ 2 deg #_(at-i [0 2 1 (at-i [2 3]) 4])))
                      :mod-freq (rrand 6000 10000)
                      :pan (rrand -1.0 1)
                      :dcy (rrand 1 3)
                      :amp (rrange 0.45 0.7)
                      :out (bh 0)))

                  ;; TODO agregar silencions con when-not
                  #_(when (> (rand ) 0.6)
                      (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [0 -4 0 -4 -3]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                  #_(if (> (rand) 0.5)
                      (my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [-4  0 -4 -4 0 -4 0]) deg)) :dur (weighted {1 9 1.5 8}) :vel 100}))
                  #_(when (> (rand) 0.4))
                  #_(my-malgo {:deg (diat->polydori-degree 1 (+ (at-i [2 2 3 2 3]) (weighted {0 5 4 4 }) deg)) :dur (weighted {0.1 9 1 5}) :vel 100})
                  #_(my-malgo {:base-midi-chan 0 :deg deg :dur 0.1 :vel 100})))))
