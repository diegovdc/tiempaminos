(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.versions.toma-6
  "Based on commit `3fb086b3903cc6ae5a5ba1ee5797958fe65d83d8`.
  There is rendered recording of it on the reaper habitat-live_v2.2.xxx project"
  (:require
   [overtone.core :as o]
   [tieminos.habitat.extended-sections.harmonies.fib-1 :refer [fib-chord-seq
                                                               transpose-chord]]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq :refer [subsequencer]]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :refer [inputs preouts]]
   [tieminos.habitat.scratch.sample-rec2 :refer [hacia-un-nuevo-universo-perc-refrain-v1p2 start-rec-loop2!]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.sc-utils.recording.v1 :refer [recording?]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp]))


(defn- sections
  [time-offset context]
  (subsequencer
    :sequencer/hacia-un-nuevo-universo-live
    context
    [[[time-offset 00] (fn [_]
                         (println "S1-=============")
                         (open-inputs-with-rand-pan
                           {:inputs inputs
                            :preouts preouts}))]
     [[(+ #_3 time-offset) 1]
      (fn [_]
        (start-rec-loop2!
          {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals rand-nth :bus))
           :durs (mapv (fn [_] (rrange 5 10)) (range 40))})
        (hacia-un-nuevo-universo-perc-refrain-v1p2
          {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 5) (#(when (seq %) (rand-nth %)))))
           :rates #_(interleave (fib-chord-seq (transpose-chord [0 9 16] [20 19 2 27 23 34 50 48])
                                               #_(transpose-chord [0 5 13 21] (map #(- % 21) [20 28 25 31 39 27])))
                                (fib-chord-seq (transpose-chord [8] [20 19 2 27 23 34 50 48]))
                                (fib-chord-seq (transpose-chord [-2 13 18] [20 19 27 23 3 34 50 48]))
                                (fib-chord-seq (transpose-chord [3] [20 19 2 27 23 34 50 48]))
                                (fib-chord-seq (transpose-chord [-15 21] [20 19 27 23 34 50 48 0]))
                                (fib-chord-seq (transpose-chord [13] [20 19 2 27 23 34 50 48])))
           #_(concat (fib-chord-seq (transpose-chord [0 5 13 21] (map #(- % 21) [20 28 25 31 39 27])))
                     (fib-chord-seq (transpose-chord [0 5 13 21] (map #(* 2 (- % 21)) [20 28 25 31 39 27])))
                     (fib-chord-seq (transpose-chord [0 5 13 21] (map #(* 4 (- % 21)) [20 28 25 31 39 27]))))
           (interleave (fib-chord-seq (transpose-chord [0 5 13 21] (range 0 (* 21 6) 5)))
                       (reverse (fib-chord-seq (transpose-chord [0 5 13 21] (range 0 (* 21 6) 5)))))
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
                       (rrange 2 5) 1}}))]
     [[(+ 5 time-offset) 00]
      (fn [_]
        (hacia-un-nuevo-universo-perc-refrain-v1p2
          {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 5) (#(when (seq %) (rand-nth %)))))
           :rates #_(interleave (fib-chord-seq (transpose-chord [0 9 16] [20 19 2 27 23 34 50 48])
                                               #_(transpose-chord [0 5 13 21] (map #(- % 21) [20 28 25 31 39 27])))
                                (fib-chord-seq (transpose-chord [8] [20 19 2 27 23 34 50 48]))
                                (fib-chord-seq (transpose-chord [-2 13 18] [20 19 27 23 3 34 50 48]))
                                (fib-chord-seq (transpose-chord [3] [20 19 2 27 23 34 50 48]))
                                (fib-chord-seq (transpose-chord [-15 21] [20 19 27 23 34 50 48 0]))
                                (fib-chord-seq (transpose-chord [13] [20 19 2 27 23 34 50 48])))
           #_(concat (fib-chord-seq (transpose-chord [0 5 13 21] (map #(- % 21) [20 28 25 31 39 27])))
                     (fib-chord-seq (transpose-chord [0 5 13 21] (map #(* 2 (- % 21)) [20 28 25 31 39 27])))
                     (fib-chord-seq (transpose-chord [0 5 13 21] (map #(* 4 (- % 21)) [20 28 25 31 39 27]))))
           (interleave (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -3) (* 21 6) 5)))
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
                       (rrange 2 5) 1}}))]
     [[(+ 5 time-offset) 00]
      (fn [_]
        (hacia-un-nuevo-universo-perc-refrain-v1p2
          {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 5) (#(when (seq %) (rand-nth %)))))
           :rates #_(interleave (fib-chord-seq (transpose-chord [0 9 16] [20 19 2 27 23 34 50 48])
                                               #_(transpose-chord [0 5 13 21] (map #(- % 21) [20 28 25 31 39 27])))
                                (fib-chord-seq (transpose-chord [8] [20 19 2 27 23 34 50 48]))
                                (fib-chord-seq (transpose-chord [-2 13 18] [20 19 27 23 3 34 50 48]))
                                (fib-chord-seq (transpose-chord [3] [20 19 2 27 23 34 50 48]))
                                (fib-chord-seq (transpose-chord [-15 21] [20 19 27 23 34 50 48 0]))
                                (fib-chord-seq (transpose-chord [13] [20 19 2 27 23 34 50 48])))
           #_(concat (fib-chord-seq (transpose-chord [0 5 13 21] (map #(- % 21) [20 28 25 31 39 27])))
                     (fib-chord-seq (transpose-chord [0 5 13 21] (map #(* 2 (- % 21)) [20 28 25 31 39 27])))
                     (fib-chord-seq (transpose-chord [0 5 13 21] (map #(* 4 (- % 21)) [20 28 25 31 39 27]))))
           (interleave (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -3) (* 21 6) 5)))
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
                       (rrange 2 5) 1}}))]
     [[(+ 5 time-offset) 00]
      (fn [_]
        (hacia-un-nuevo-universo-perc-refrain-v1p2
          {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 5) (#(when (seq %) (rand-nth %)))))
           :rates (interleave (fib-chord-seq (transpose-chord [0 9 16] [20 19 2 27 23 34 50 48]))
                              (fib-chord-seq (transpose-chord [8] [20 19 2 27 23 34 50 48]))
                              (fib-chord-seq (transpose-chord [-2 13 18] [20 19 27 23 3 34 50 48]))
                              (fib-chord-seq (transpose-chord [3] [20 19 2 27 23 34 50 48]))
                              (fib-chord-seq (transpose-chord [-15 21] [20 19 27 23 34 50 48 0]))
                              (fib-chord-seq (transpose-chord [13] [20 19 2 27 23 34 50 48])))
           #_(concat (fib-chord-seq (transpose-chord [0 5 13 21] (map #(- % 21) [20 28 25 31 39 27])))
                     (fib-chord-seq (transpose-chord [0 5 13 21] (map #(* 2 (- % 21)) [20 28 25 31 39 27])))
                     (fib-chord-seq (transpose-chord [0 5 13 21] (map #(* 4 (- % 21)) [20 28 25 31 39 27]))))

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
                       (rrange 2 5) 1}}))]

     [[(+ 5 time-offset) 0] (fn [_]
                              (gp/stop :rec-loop2))]]))

(comment

  (open-inputs-with-rand-pan {:inputs inputs
                              :preouts preouts})

  (do  (when @habitat-initialized?
         (main/stop-sequencer! hseq/context)
         (reset! recording? {})
         (reset! rec/bufs {}))
       (init!))

  (do ;; Start
    (let [time-offset 167]
      (def sections* (partial sections time-offset))
      (main/start-sequencer!
        {:context (merge main/context {})
         :sections [[[time-offset 00] #'sections*]]
         :initial-section #'sections*
         ;; :rec? true
         })))
  (o/stop))
