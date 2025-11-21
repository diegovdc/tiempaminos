(ns tieminos.lc.ensayo-rossana-06102025
  (:require
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [algo-note]]
   [tieminos.seq-utils.core :refer [** mirror rainseq]]
   [time-time.dynacan.players.refrain.v2 :refer [on-event ref-rain]]))

(defn init!
  []
  (midi/midi-sinks)
  (def sink (midi/midi-out "VirMIDI"))
  (def sink2 (midi/midi-out "VirMIDI Bus 2"))
  (def sink3 (midi/midi-out "VirMIDI Bus 3")))

(comment
  (init!)

  (ref-rain
   :id :pulse1
   :ratio 1/16
   :durs (concat [3 2 3 2 2]
                 [3 2 3 2 2]
                 [3 2 3 2 2]
                 (map #(* 1/2 %) [3 2 3 2 2])
                 (map #(* 1/4 %) [3 2 3 2 2])
                 (map #(* 1/8 %) [3 2 3 2 2]))
   :on-event
   (on-event
    #_(when #{0 1 3 6 5} (mod i 7)
            (algo-note {:sink sink
                        :dur (* 0.99
                                dur-s
                                (rainseq (**
                                          [0.5 1 0.5 2]
                                          [4 1 2 1])))
                        :vel (min 127 (int (rainseq (** 12 [3 4 5 3]))))
                        :chan 0
                        :offset (rainseq [20 50 20 80 90])
                        :tempo 120
                        :note (rainseq [0 12 0 8 0 0 18 0 0 7 0 -5])}))))
  (ref-rain
   :id :pulse2
   :ratio 6
   :durs [3 2 3 2 2]
   :on-event
   (on-event
    #_(algo-note {:sink sink
                  :dur (* 0.99
                          dur-s
                          (rainseq (** [4 1 2 1])))
                  :vel (min 127 (int (rainseq (** 3 [3 4 5 3]))))
                  :chan 10
                  :offset 22
                  :tempo 120
                  :note (rainseq (mirror [0 5 7 12 14 16 19]))}))))
