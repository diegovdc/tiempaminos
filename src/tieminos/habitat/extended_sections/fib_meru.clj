(ns tieminos.habitat.extended-sections.fib-meru
  (:require
   [erv.scale.core :as scale]))

(def fib-21 [{:ratio 1, :bounded-ratio 1024/987, :bounding-period 2}
             {:ratio 4181/4096, :bounded-ratio 4181/3948, :bounding-period 2}
             {:ratio 17/16, :bounded-ratio 1088/987, :bounding-period 2}
             {:ratio 17711/16384, :bounded-ratio 17711/15792, :bounding-period 2}
             {:ratio 9/8, :bounded-ratio 384/329, :bounding-period 2}
             {:ratio 305/256, :bounded-ratio 1220/987, :bounding-period 2}
             {:ratio 5/4, :bounded-ratio 1280/987, :bounding-period 2}
             {:ratio 323/256, :bounded-ratio 1292/987, :bounding-period 2}
             {:ratio 21/16, :bounded-ratio 64/47, :bounding-period 2}
             {:ratio 5473/4096, :bounded-ratio 5473/3948, :bounding-period 2}
             {:ratio 89/64, :bounded-ratio 1424/987, :bounding-period 2}
             {:ratio 1449/1024, :bounded-ratio 69/47, :bounding-period 2}
             {:ratio 377/256, :bounded-ratio 1508/987, :bounding-period 2}
             {:ratio 3/2, :bounded-ratio 512/329, :bounding-period 2}
             {:ratio 1597/1024, :bounded-ratio 1597/987, :bounding-period 2}
             {:ratio 13/8, :bounded-ratio 1664/987, :bounding-period 2}
             {:ratio 6765/4096, :bounded-ratio 2255/1316, :bounding-period 2}
             {:ratio 55/32, :bounded-ratio 1760/987, :bounding-period 2}
             {:ratio 28657/16384, :bounded-ratio 28657/15792, :bounding-period 2}
             {:ratio 233/128, :bounded-ratio 1864/987, :bounding-period 2}
             {:ratio 987/512, :bounded-ratio 2N, :bounding-period 2}])

(defn fib-chord [degs]
  (->> degs
       (map (fn [deg]
              (scale/deg->freq fib-21 1 deg)))))

(defn fib-chord-seq [chords]
  (map fib-chord chords))

(defn transpose-chord [chord transpositions]
  (map (fn [t] (map (fn [deg] (+ t deg)) chord)) transpositions))
