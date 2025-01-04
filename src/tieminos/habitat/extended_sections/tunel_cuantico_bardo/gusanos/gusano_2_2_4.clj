(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.gusanos.gusano-2-2-4
  (:require
   [tieminos.habitat.extended-sections.harmonies.chords :refer [fib-chord-seq
                                                                transpose-chord]]
   [tieminos.habitat.recording :as rec]
   [tieminos.utils :refer [rrange]]))

(def chords
  {:fib-0.5.13.21-range0.6-5step-interleaved+reverse
   (interleave (fib-chord-seq (transpose-chord [0 5 13 21] (range 0 (* 21 6) 5)))
               (reverse (fib-chord-seq (transpose-chord [0 5 13 21] (range 0 (* 21 6) 5)))))
   :fib-0.5.13.21-range-3.6-5step-interleaved+reverse
   (interleave (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -3) (* 21 6) 5)))
               (reverse (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -3) (* 21 6) 5)))))
   :fib-multiple-interleaved
   (interleave (fib-chord-seq (transpose-chord [0 9 16] [20 19 2 27 23 34 50 48]))
               (fib-chord-seq (transpose-chord [8] [20 19 2 27 23 34 50 48]))
               (fib-chord-seq (transpose-chord [-2 13 18] [20 19 27 23 3 34 50 48]))
               (fib-chord-seq (transpose-chord [3] [20 19 2 27 23 34 50 48]))
               (fib-chord-seq (transpose-chord [-15 21] [20 19 27 23 34 50 48 0]))
               (fib-chord-seq (transpose-chord [13] [20 19 2 27 23 34 50 48])))})

(def s1
  {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 5) (#(when (seq %) (rand-nth %)))))
   :rates (interleave (fib-chord-seq (transpose-chord [0 5 13 21] (range 0 (* 21 6) 5)))
                      (reverse (fib-chord-seq (transpose-chord [0 5 13 21] (range 0 (* 21 6) 5)))))
   :durs [2 3 5 3 8]
   :d-weights {8 1
               5 1
               3 1}
   :d-level-weights {0.3 5
                     0.1 2
                     0.2 3
                     0.4 2}
   :a-weights {(rrange 0.01 0.2) 1/4
               (rrange 0.2 0.8) 1
               (rrange 1 2) 3
               (rrange 2 5) 1}})
(def s2
  {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 5) (#(when (seq %) (rand-nth %)))))
   :rates (interleave (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -3) (* 21 6) 5)))
                      (reverse (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -3) (* 21 6) 5)))))
   :amp 0.4
   :period 20
   :durs [1 2 3]
   :d-weights {10 1
               15 1
               13 1}
   :d-level-weights {0.3 5
                     0.1 2
                     0.2 3
                     0.4 2}
   :a-weights {(rrange 8 15) 4
               (rrange 2 5) 1}})

(def s3
  {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 5) (#(when (seq %) (rand-nth %)))))
   :rates (interleave (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -3) (* 21 6) 5)))
                      (reverse (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -3) (* 21 6) 5)))))
   :amp 0.6
   :period 30
   :durs [2 3 5 3 8]
   :d-weights {8 1
               5 1
               3 1}
   :d-level-weights {0.3 5
                     0.1 2
                     0.2 3
                     0.4 2}
   :a-weights {(rrange 0.01 0.2) 1/4
               (rrange 0.2 0.8) 1
               (rrange 1 2) 3
               (rrange 2 5) 1}})
(def s4
  {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 5) (#(when (seq %) (rand-nth %)))))
   :rates (interleave (fib-chord-seq (transpose-chord [0 9 16] [20 19 2 27 23 34 50 48]))
                      (fib-chord-seq (transpose-chord [8] [20 19 2 27 23 34 50 48]))
                      (fib-chord-seq (transpose-chord [-2 13 18] [20 19 27 23 3 34 50 48]))
                      (fib-chord-seq (transpose-chord [3] [20 19 2 27 23 34 50 48]))
                      (fib-chord-seq (transpose-chord [-15 21] [20 19 27 23 34 50 48 0]))
                      (fib-chord-seq (transpose-chord [13] [20 19 2 27 23 34 50 48])))
   :amp 0.6
   :period 15
   :d-weights {8 1
               5 1
               3 1}
   :d-level-weights {0.3 5
                     0.1 2
                     0.2 3
                     0.4 2}
   :a-weights {(rrange 0.01 0.2) 1/4
               (rrange 0.2 0.8) 1
               (rrange 1 2) 3
               (rrange 2 5) 1}})
