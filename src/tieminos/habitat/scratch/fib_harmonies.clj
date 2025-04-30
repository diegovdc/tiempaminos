(ns tieminos.habitat.scratch.fib-harmonies
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [tieminos.habitat.extended-sections.harmonies.chords :refer [fib-chord-seq
                                                                transpose-chord]]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]
   [tieminos.synths :as synths]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(fib-chord-seq (transpose-chord [0 3 7] (range 0 21)))

(declare efficient-voice-leading)

(->> (combo/combinations (range 21) 3)
     (filter #(and (= 21 (apply + %))
                   (not ((set %) 0))))
     (mapv #(combo/permutations %))
     (apply concat)
     (map (fn [chord] {:chord chord
                       :transps (efficient-voice-leading 0 chord)})))

(defn chord->degrees [chord]
  (reduce
   (fn [degrees interval]
     (conj degrees (+ interval (or (last degrees) 0))))
   []
   chord))

(defn degrees->chord [size degrees]
  (->> (partition 2 1 degrees)
       (mapv (fn [[a b]] (- b a)))
       ((fn [intervals] (conj intervals (- size (apply + intervals)))))))

(defn efficient-voice-leading
  [step-limit chord]
  (let [size (apply + chord)
        degrees (chord->degrees chord)
        transps (map (fn [i] (sort (map #(mod (+ i %) size) degrees))) (range size))]
    (remove nil? (for [t1 transps
                       t2 transps]
                   (let [diff (apply + (map - t1 t2))]

                     (when (and (= 1 (count (set/intersection (set t1) (set t2))))
                                (<= (Math/abs diff) step-limit))
                       [diff
                        [t1 (degrees->chord size t1)]
                        [t2 (degrees->chord size t2)]]))))))

(comment
  (def fib-21 [{:ratio 1, :bounded-ratio 1024/987, :bounding-period 2} {:ratio 4181/4096, :bounded-ratio 4181/3948, :bounding-period 2} {:ratio 17/16, :bounded-ratio 1088/987, :bounding-period 2} {:ratio 17711/16384, :bounded-ratio 17711/15792, :bounding-period 2} {:ratio 9/8, :bounded-ratio 384/329, :bounding-period 2} {:ratio 305/256, :bounded-ratio 1220/987, :bounding-period 2} {:ratio 5/4, :bounded-ratio 1280/987, :bounding-period 2} {:ratio 323/256, :bounded-ratio 1292/987, :bounding-period 2} {:ratio 21/16, :bounded-ratio 64/47, :bounding-period 2} {:ratio 5473/4096, :bounded-ratio 5473/3948, :bounding-period 2} {:ratio 89/64, :bounded-ratio 1424/987, :bounding-period 2} {:ratio 1449/1024, :bounded-ratio 69/47, :bounding-period 2} {:ratio 377/256, :bounded-ratio 1508/987, :bounding-period 2} {:ratio 3/2, :bounded-ratio 512/329, :bounding-period 2} {:ratio 1597/1024, :bounded-ratio 1597/987, :bounding-period 2} {:ratio 13/8, :bounded-ratio 1664/987, :bounding-period 2} {:ratio 6765/4096, :bounded-ratio 2255/1316, :bounding-period 2} {:ratio 55/32, :bounded-ratio 1760/987, :bounding-period 2} {:ratio 28657/16384, :bounded-ratio 28657/15792, :bounding-period 2} {:ratio 233/128, :bounded-ratio 1864/987, :bounding-period 2} {:ratio 987/512, :bounded-ratio 2N, :bounding-period 2}])
  (fib-chord-seq [[0 8 16]])
  (gp/stop)
  (o/stop)
  (ref-rain
   :id :fib-harmonies
   :durs [8]
   :tempo 60
   :ratio 1
   :on-event
   (on-event
    ((o/synth
      (o/out 1
             (* 0.3
                (o/env-gen (o/env-perc (/ dur 3/2) (/ dur 2)) :action o/FREE)
                (o/pan2:ar
                 (o/mix
                  (o/lpf
                   (o/saw
                    (* 200 (at-i (fib-chord-seq
                                  [[4 8 12]
                                   #_[2 10 14]]
                                  #_[[0 8 15 18]
                                     [2 9 12 15]
                                     [2 5 13 20]
                                     [5 8 11 19]
                                     [1 9 16 19]
                                     [3 10 13 16]
                                     [2 10 17 20]
                                     [4 11 14 17]
                                     [5 8 11 19]
                                     [2 5 13 20]
                                     [2 9 12 15]
                                     #_[0 9 16]
                                     #_[2 9 14]
                                     #_[2 7 14]]))))
                   3000))
                 0))))))))

(comment
  (midi-in-event
   :midi-input (get-oxygen!)
   :note-on (fn [{:as event}]
              (let [vel (/ (:velocity event) 127)]
                (synths/soft-saw2
                 :freq (scale/deg->freq fib-21 112 (- (:note event) 40))
                 :atk (*  5 vel)
                 :amp vel)))))
