(ns tieminos.scales.feigenbaum
  "Jam de feigenbaum-explorations2"
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.utils.ratios :refer [ratios->scale]]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [algo-note]]
   [tieminos.osc.surge :as surge]
   [time-time.dynacan.players.gen-poly :as gp]))

(def feigenbaum9
  (ratios->scale
   [139.05530
    267.81106
    406.86636
    535.62212
    674.67742
    803.43318
    942.48848
    1071.24424
    2/1]))

(defn smap [f x]
  (if (sequential? x)
    (map f x)
    (f x)))

(defn s+ [n xs]
  (smap #(+ n %) xs))

(comment
  (midi/midi-sinks)
  (def sink (midi/midi-out "VirMIDI"))
  (def sink2 (midi/midi-out "VirMIDI Bus 3"))
  (def sink3 (midi/midi-out "VirMIDI Bus 4"))
  (surge/init)
  (gp/stop :sequencer2)
  (let [scale feigenbaum9
        scale-size (count scale)]
    (gp/ref-rain
     :id :bd
     :tempo 120
     :durs [1/4]
     :on-event (gp/on-event
                (when (#{0 2 6} (mod i 8))
                    ;; bd
                  (algo-note {:sink sink
                              :dur 1/20
                              :vel (min 127 (int (* 12 (at-i [8 3 4 5 3]))))
                              :chan 0
                              :offset 40
                              :tempo 120
                              :note (at-i [1])}))))

    (gp/ref-rain
     :id :sd
     :ref :bd
     :tempo 120
     :durs [1/8]
     :on-event (gp/on-event
                (when (#{1 3 5 7} (mod i 8))
                  (algo-note {:sink sink
                              :dur 1/20
                              :vel (min 127 (int (* 12 (weighted {(at-i [8 3 1 5 3]) 8
                                                                  120 1}))))
                              :chan 1
                              :offset 40
                              :tempo 120
                              :note (at-i [1 1 1 1 1 3 1 1 -1])}))))
    (gp/ref-rain
     :id :hh
     :ref :bd
     :tempo 120
     :durs [1/4 1/4 1/4 1/4 1/8 1/8 1/4 1/8 1/4 1/8 1/8]
     :on-event (gp/on-event
                (algo-note {:sink sink
                            :dur 1/3
                            :vel (min 127 (int (* 8 (at-i [8 3 1 3]))))
                            :chan 2
                            :offset 80
                            :tempo 120
                            :note (at-i [7])})))
    (gp/ref-rain
     :id :chords
     :ref :bd
     :tempo 120
     :durs [1/4]
     :on-event (gp/on-event
                (when (#{0} (mod i 5))
                  (algo-note {:sink sink
                              :dur (* 0.1 (at-i [14 3 7 3 2 3]))
                              :vel (min 127 (int (* 12 (at-i [#_1 8 #_10 3 #_1 3]))))
                              :chan 3
                              :offset (+ 60 (at-i [0 0 0 0 -9]))
                              :tempo 120
                              :note (->> (at-i [[0 -6]
                                                (at-i [3 #_#_#_9 3 2])
                                                #_(at-i [7 #_5])
                                                #_(at-i [4 #_-8])
                                                5])
                                         #_(s+ -9)
                                         #_(smap #(+ % (at-i [0 0 -4 0 0 4])))
                                         #_(smap (fn [x] (if (#{4} (mod i 10))
                                                           (flatten [x (smap #(+ % 5) x) (smap #(- % 5) x)])
                                                           x))))})))))
  (gp/stop))
