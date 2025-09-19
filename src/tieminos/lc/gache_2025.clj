(ns tieminos.lc.gache-2025
  (:require
   [clojure.math :refer [sin]]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [algo-note]]
   [tieminos.osc.surge :as surge]
   [tieminos.seq-utils.core :refer [** ++ lin mirror rainseq ret]]
   [tieminos.seq-utils.utils :refer [repcat]]
   [time-time.dynacan.players.refrain.v2 :refer [on-event ref-rain]]
   [time-time.dynacan.players.refrain.v2 :as rain2]))

(defn init!
  []
  (midi/midi-sinks)
  (def sink (midi/midi-out "VirMIDI"))
  (def sink2 (midi/midi-out "VirMIDI Bus 2"))
  (def sink3 (midi/midi-out "VirMIDI Bus 3"))
  (surge/init))

(comment
  (init!)
  (rain2/stop)
  (ref-rain
   :id :pulse1
   :ratio 3
   :durs [1 1 1 1]
   :on-event
   (on-event
    (algo-note {:sink sink
                :dur (* 0.99 (min 2 dur-s))
                :vel (min 127 (int (* 12 (at-i [3 4 5 3]))))
                :chan 0
                :offset 50
                :tempo 120
                :note 0})))

  (ref-rain
   :id :pulse2
   :ref :pulse1
   :ratio 1/14
   :durs (fn [data]
           (let [i (:index data)]
             (rainseq (** 1/14 (repcat [4 (concat [7 7 7 4 7]
                                                  [7 7 7 4 7]
                                                  (map #(/ % 2) [7 7 7]))]
                                       [4 (map #(/ % 2) [7 7 7])])))))
   :on-event
   (on-event
    (algo-note {:sink sink
                :dur  (* 0.99 #_2 (min 2 dur-s)
                         #_(rainseq [1 1.2 [1 2] {1.3 20 3 7}]))
                :vel (min 127 (int (rainseq (++ 0 (**
                                                   (repcat [10 1] [1 0])
                                                   [[12 10 9 12 7]
                                                    [11 10]
                                                    [8 8 13 8]
                                                    [11 10]
                                                    [8 8 13 8]]
                                                   [3 4 5 3 3])))))
                :chan (rainseq [1 (lin 1 (ret 1 #_0) 2) 1 (lin 1 #_0) 1])
                :tempo 120
                :offset (rainseq (++ -7 70
                                     #_(repcat [20 0]
                                               [5 2]
                                               [20 0]
                                               [5 1])))
                #_(comment (lin 0 (lin 3 9  7) 4 [1 0  5] 0))
                :note (rainseq (concat (repcat [4 [0 (lin 2 0) (lin -5 -2 6 3 -3)]]
                                               [1 [0 0 (lin -5 7 -3) 2]])))})
    (when (#{1} (mod i 3))
      (algo-note {:sink sink
                  :tempo 120
                  :dur (* 0.99 2 (min 2 dur-s)
                          (rainseq [1 1.2 1.3 {1 15 #_#_5 1}]))
                  :vel (min 127 (int (rainseq (++ 0 0 (** 12 [3 4 5 3 3])))))
                  :chan (rainseq (lin 1 (lin 1 (ret 1 0) 2) 1 (lin 1 0) 1))
                  :offset (rainseq (++ -7 70
                                       #_(repcat [20 0]
                                                 [5 2]
                                                 [20 0]
                                                 [20 7]
                                                 [5 1])))
                  #_(comment [0 3 5 7 6 3 5 (lin 9 4) (lin 0 1 0)])
                  :note (rainseq #_(lin 0 3 5 7 6 3 5 (lin 9 4) (lin 0 1 0))
                         [(ret 0 3) 5 (ret 7) 3 (ret 5 (lin 9 2)) (lin 0 1 0)])}))))

  (ref-rain
   :id :pulse2b
   :ref :pulse1
   :ratio 1/14
   :durs [3/2]
   :on-event
   (on-event
    (let [offset (rainseq (++ -7 2 (repcat [10 90]
                                           [10 89]
                                           [10 90]
                                           [1 75]
                                           [10 88]
                                           [1 75]
                                           [10 90]
                                           [1 60])))]

      (algo-note {:sink   sink
                  :dur    (* (if (< offset 70) 20 3) (rainseq (mirror (range 1 5 0.1))) dur-s)
                  :vel   (min 127 (inc (int (rainseq (++ 20 [7 -7 0] (** 0.8 (concat (mirror (range 100))
                                                                                     (mirror (range 100 0.5)))))))))
                  :chan   (rainseq {2 20 1 1 0 1})
                  :offset offset
                  :tempo  120
                  :note   (int (rainseq (++ -3
                                            (mirror (range 5 -5 -0.1))
                                            (mirror (range -5 5 0.05))
                                            [0 3 5 7 6 3 5 (lin 9 4) 0])))})))))
