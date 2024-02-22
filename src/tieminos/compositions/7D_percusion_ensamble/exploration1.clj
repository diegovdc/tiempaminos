(ns tieminos.compositions.7D-percusion-ensamble.exploration1
  (:require
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.synths :as synth]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(def sink (midi/midi-out "VirMIDI"))

(comment

  (gp/stop)

  (let [my-malgo #(malgo-note (merge {:sink sink
                                      :scale-size 6
                                      :base-midi-deg 60}
                                     %))]
    (ref-rain
     :id :bass-mid1
     :durs [4 3 3]
     :tempo 160
     :ratio 1/6
     :on-event (on-event
                (let [deg (+  (at-i [(at-i [0 -6]) (at-i [1 2 0 7 8])
                                     (at-i [1 5 -5 3 9 -10 4])]))]
                  (my-malgo {:base-midi-chan 0
                             :deg deg
                             :dur (if (> deg -1) (* (rand) dur-s)
                                      2)
                             :vel (int (* (rand) 127))}))))
    (ref-rain
     :id :bass-mid2
     :durs [3 4 3]
     :tempo 160
     :ratio 1/6
     :on-event (on-event
                #_(let [deg (+ 14 (at-i [(at-i [0 -6]) (at-i [1 2 0])
                                         (at-i [1 5 -5 3 -10])]))]
                    (when (> (rand) 0.5)
                      (my-malgo {:base-midi-chan 0
                                 :deg deg
                                 :dur (* (rand 3) dur-s)
                                 :vel (int (* (rand) 27))})))))

    #_(ref-rain
       :id :highs
       :ref :bass-mid
       :durs [18 8 8 4 8 8 8 8]
       :tempo 160
       :ratio 1/8
       :on-event (on-event
                  (let [deg (+ (at-i [6]) (at-i [1 2]))]
                    (when deg
                      (my-malgo {:base-midi-chan 1
                                 :deg deg
                                 :dur (* dur (at-i [(at-i [2 5 1])
                                                    (at-i [2 5 1])]))
                                 :vel (rand-int 128)})))))
    #_(ref-rain
       :id :highs2
       :ref :bass-mid
       :durs [3 3 3 3 3 1.5]
       :tempo 160
       :ratio 1/8
       :on-event (on-event
                  (let [deg (+ (at-i [14]) (at-i [1 (at-i [4 nil])
                                                  2 (at-i [4 8 nil 5])
                                                  3
                                                  nil
                                                  (at-i [4 nil 1 5])]))]
                    (when deg
                      (my-malgo {:base-midi-chan 2
                                 :deg deg
                                 :dur (rand-nth [0.5 1])
                                 :vel (rand-nth [80 120 80])}))))))

  (gp/stop))
