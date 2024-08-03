(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.main
  "The code from the recorded versions of `2.2.9"
  (:require
   [overtone.core :as o]
   [tieminos.habitat.extended-sections.fib-meru :refer [fib-chord-seq
                                                        transpose-chord]]
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo.main-4ch :as hunu.4ch]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :as habitat]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :as habitat.route]
   [tieminos.habitat.scratch.sample-rec2 :refer [hacia-un-nuevo-universo-perc-refrain-v1p2 quad-router-2o
                                                 rev-filter start-rec-loop3!]]
   [tieminos.osc.reaper :as reaper]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp]))

(comment
  (fib-chord-seq (transpose-chord [0 6 12 18] (range 21))) ;; acorde bonito, muy liso
  (fib-chord-seq (transpose-chord [0 4 8 12 16 20 24 28] [0 1])) ;; calido con un poco de disonancia
  (fib-chord-seq (transpose-chord [11 15 19] (range 21))) ;; estable claro (segmento de arriba: 4-4)
  (fib-chord-seq (transpose-chord [10 15 20] (range 21))) ;; nocturno (5-5)
  )

(defn algo-2-2-9
  [{:keys [chord
           transpositions
           out-bus]
    :or {chord [0 6 12 18]
         transpositions [0]}}]

  (start-rec-loop3!
    {:input-bus-fn (fn [_] (-> @habitat.route/inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
     :durs (mapv (fn [_] (rrange 10 20)) (range 40))})
  (hacia-un-nuevo-universo-perc-refrain-v1p2
    {:out-bus out-bus
     :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
     :silence-thresh 0.05
     :rates (fib-chord-seq (transpose-chord chord transpositions))
     :amp 0.6
     :period 30
     :durs [2 3 5 3 8 13 5 8 2 3 5]
     :d-weights {8 1
                 5 1
                 3 0.3}
     :d-level-weights {0.3 5
                       0.1 2
                       0.2 3
                       0.4 8}
     :a-weights {(rrange 5 8) 3
                 (rrange 3 5) 2}}))

(comment
  (when @habitat/habitat-initialized?
    (reset! rec/recording? {})
    (main/stop-sequencer! hseq/context)
    (reset! rec/bufs {}))
  (o/stop)
  (o/kill qbr)
  (o/kill rev-filter*)

  (habitat/init! {})

  ;; also part of the initialization of hacia un nuevo universo
  (def in1 (o/audio-bus 4 "algo-2.2.9-out"))

  (def out1 (o/audio-bus 4 "reverb-out"))

  (def qbr (quad-router-2o {:group (groups/mid :tail)
                            :in-bus in1
                            :out-bus1 out1
                            :out-bus2 (habitat.route/main-returns :mixed)}))

  (def rev-filter* (rev-filter
                     {:group (groups/panners)
                      :in-bus out1}))

  #_(open-inputs-with-rand-pan
      {:inputs habitat.route/inputs
       :preouts habitat.route/preouts})

  (hunu.4ch/open-inputs-with-rand-pan*
    {:inputs habitat.route/inputs
     :preouts habitat.route/preouts}
    {}
    #_{:guitar {:amp 1
                :type :clockwise
                :rate 1}})


  (algo-2-2-9 {:out-bus in1
               :chord [10 15 20]
               :transpositions [0 -5 0 5]})
  (gp/stop)
  (reaper/stop))


(comment
  (doseq [i (range 60)]
    (o/demo (o/in i
                  1)))
  (o/demo (o/sin-osc))
  )
