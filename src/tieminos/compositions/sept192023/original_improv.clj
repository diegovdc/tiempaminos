(ns tieminos.compositions.sept192023.original-improv
  (:require
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(def sink (midi/midi-out "VirMIDI"))

(comment

  (let [my-malgo #(malgo-note (merge {:sink sink
                                      :scale-size 6
                                      :base-midi-deg 60
                                      :base-midi-chan 2}
                                     %))]
    (ref-rain
      :id :pad
      :durs [2 2 3 2 2]
      :tempo 160
      :on-event (on-event
                  (my-malgo {:deg (at-i [0 6 5 4 3])
                             :dur 0.5
                             :vel (at-i [80 90 120])})
                  (case (mod i 3)
                    0 (my-malgo {:deg (+ 5 (at-i [0 6 5 4 3]))
                                 :dur 2
                                 :vel (at-i [10 30 50 30])})
                    2 (my-malgo {:deg (+ 3 (at-i [0 6 5 4 3]))
                                 :dur 1
                                 :vel (at-i [10 30 50 30])})
                    nil)
                  #_(case (mod i 10)
                      0 (my-malgo {:deg (+ 10 (at-i [0 6 5 4 3 7]))
                                   :dur (inc (rand-int 15))
                                   :vel (at-i [50 70])})
                      5 (my-malgo {:deg (- 10 (at-i [0 1 6 5 4 3]))
                                   :dur (inc (rand-int 15))
                                   :vel (at-i [80 50])})
                      nil))))

  (let [my-malgo #(malgo-note (merge {:sink sink
                                      :scale-size 29
                                      :base-midi-deg 0
                                      :base-midi-chan 1}
                                     %))]
    (ref-rain
      :id :hh
      :durs (flatten (concat (repeat 2 (concat (repeat 8 2)
                                               (repeat 10 1/2)
                                               (repeat 4 4)
                                               (repeat 10 1/2)))
                             (repeat 8 1/3)
                             (repeat 2 (concat (repeat 8 2)
                                               (repeat 10 1/2)
                                               (repeat 4 1/14)
                                               (repeat 20 1/2)))
                             (repeat 16 1/3)))
      :tempo 160
      :ratio 4
      :on-event (on-event
                  (when (> (rand) 0.7)
                    (my-malgo {:deg (->> (+ (at-i [0 60 30 0 0 0 0 0 0 0 0 0 20 70 0  80 20])
                                            (at-i (flatten [0 16
                                                            (rand-int 60) 15 4
                                                            31
                                                            (rand-int 20) 10
                                                            (repeat 8 60)
                                                            10
                                                            59 69
                                                            (rand-int 20)
                                                            (rand-int 120)
                                                            (repeat 3 60)
                                                            (rand-int 20)])))
                                         (min 127)
                                         (max 0))
                               :dur (rand-nth [0.5 1 4 (at-i [23 0.5 0.5 0.4 1])])
                               :vel (at-i [30 50 80 20 10 70 16 23])})))))

  (let [my-malgo #(malgo-note (merge {:sink sink
                                      :scale-size 6
                                      :base-midi-deg 60
                                      :base-midi-chan 3}
                                     %))]
    (ref-rain
      :id :lead
      :durs [3 2 3 2 3 3]
      :tempo 160
      :ratio 1
      :on-event (on-event
                  (when (> (rand) 0.3)
                    (my-malgo {:deg (+ 12 (rand-int 8))
                               :dur (* (rand 10) dur-s)
                               :vel (+ 80 (rand-int 30))}))))
    (ref-rain
      :id :lead2
      :durs [3 2 3 2 3 3]
      :tempo 160
      :ratio 3/4
      :on-event (on-event
                  (when (> (rand) 0.5)
                    (my-malgo {:deg (+ 18 (rand-int 9))
                               :dur (* (rand 10) dur-s)
                               :vel (+ 80 (rand-int 30))}))))
    (ref-rain
      :id :lead-ctl
      :durs [3 2 3 2 3 3]
      :tempo 160
      :ratio 3/4
      :on-event (on-event
                  (case (mod i 5)
                    1 (midi/midi-control sink 90 (int (rrange 30 120)) 3)
                    2 (midi/midi-control sink 89 (int (rrange 10 30)) 3)
                    4 (midi/midi-control sink 70 (int (rrange 0 50)) 3)
                    nil)))
    (ref-rain
      :id :lead-ctl-modwheel
      :durs [3 2 3 2 3 3]
      :tempo 160
      :ratio 3/4
      :on-event (on-event
                  (case (mod i 5)
                    1 (midi/midi-control sink 33 (int 64) 3)
                    4 (midi/midi-control sink 33 (int 127) 3)
                    nil))))

  (gp/stop))
