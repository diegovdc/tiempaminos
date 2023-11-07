(ns tieminos.compositions.sept192023.mandala-canon
  "IDEA: temporal canon of lines opening from the center.

  Independent and coordinated control of some params from the synths
  to make things brighter and give more movement. Perhaps using filters."

  (:require
   [overtone.core :as o]
   [overtone.midi :as midi]
   [tieminos.math.bezier :as bz]
   [tieminos.math.utils :refer [linexp]]
   [tieminos.midi.core :refer [all-notes-off]]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.converge :refer [converge]]
   [time-time.sequencing :refer [sequencer]]))


(def sink (midi/midi-out "VirMIDI"))

(defn algo-note [data] (malgo-note (merge {:sink sink
                                           :scale-size 6
                                           :base-midi-deg 60
                                           :base-midi-chan 2}
                                          data)))

(do
  (defn f
    ([steps] (f steps  0.03 0.1))
    ([steps min-dur max-dur]
     (linexp min-dur max-dur (bz/curve steps [20 0.1 30 6 1]))))

  (bz/plot (f 20 0.5 5)))

(comment
  (o/stop)
  (all-notes-off sink)
  ;; (c) = pa comenzar; (m) = medio; (f) = final
  ;; possibles synths: pad 8, primes (bajar lfo), smooth dist, sparkly(c), sprinky(c), verb pad (bajar rate lfo) (mf),
  ;; well (jugar con wt), bell pad (bajar amp lfo y jugar con envolvente y jugar con wt - un poco dulzón)
  (let [nome (o/metronome 120)
        x (- (rand-int 20) 10)
        degs (->> [0 3 5 6 4 7 10 9 11 14
                   16 15 17 18 12 13 15 14 16 18 17 19 16 14 10 9 8 6 7 4 5]
                  #_(take (int (rrange 10 20)))
                  ((fn [xs]
                     #_(concat xs (reverse xs))
                     #_(interleave xs (reverse xs))
                     (interleave xs (map #(* -1 %) xs))
                     #_(reverse (interleave xs (map #(* -1 %) xs)))))
                  (map #(+ x %)))
        durs (f (count degs) 0.5 5)]
    (->> (converge {:durs durs
                    :tempos [8 9 10 12 15 16]
                    :cps [(int (* (count degs) 2/3))]
                    :bpm 120
                    :period 220})
         (map (fn [voice] (sequencer
                            nome
                            voice
                            (fn [{:keys [tempo-index]} index]
                              (let [dur (rrange 0.5 20)]
                                (when-not (and (zero? index) (> (rand) 0.2))
                                  (case tempo-index
                                    0 (algo-note {:base-midi-chan 0
                                                  :deg (wrap-at index degs)
                                                  :dur dur
                                                  :vel (rand-nth [1 30 50 10])})
                                    1 (algo-note {:base-midi-chan 0
                                                  :deg (* -1 (wrap-at index degs))
                                                  :dur dur
                                                  :vel (rand-nth [1 30 50 10])})
                                    2 (algo-note {:base-midi-chan 1
                                                  :deg (* -1 (- (wrap-at index degs) 3))
                                                  :dur dur
                                                  :vel (rand-nth [1 30 50 10])})
                                    3 (algo-note {:base-midi-chan 1
                                                  :deg (+ 3 (wrap-at index degs))
                                                  :dur dur
                                                  :vel (rand-nth [1 30 50 10])})
                                    4 (doseq [d [(- (wrap-at index degs) 6)
                                                 (- (wrap-at index degs) 3)]]
                                        (algo-note {:base-midi-chan 2
                                                    :deg d
                                                    :dur dur
                                                    :vel (rand-nth [1 30 50 10])}))
                                    5 (algo-note {:base-midi-chan 2
                                                  :deg (* -1 (wrap-at index degs))
                                                  :dur dur
                                                  :vel (rand-nth [1 30 50 10])}))))
                              nil)
                            {:repeat nil}))))))
