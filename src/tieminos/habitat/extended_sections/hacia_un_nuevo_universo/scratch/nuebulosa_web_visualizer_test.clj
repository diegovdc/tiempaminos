(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.nuebulosa-web-visualizer-test
  (:require
   [hacia-un-nuevo-universo.nebulosa&planeta-hostil :as-alias nebulosa&planeta-hostil]
   [overtone.core :as o]
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo.refrains :refer [hacia-un-nuevo-universo-perc-refrain-v2p2]]
   [tieminos.habitat.extended-sections.harmonies.fib-1 :refer [fib-chord-seq
                                                               transpose-chord]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec :refer [recording?]]
   [tieminos.habitat.routing :refer [inputs main-returns preouts]]
   [tieminos.habitat.scratch.sample-rec2 :refer [amanecer*guitar-clouds-2
                                                 quad-router-2o rev-filter
                                                 start-rec-loop3!]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.utils :refer [rrange]]
   [tieminos.web-visualizer.refrain-utils-v0 :refer [collect-event!
                                                     event->viz-event
                                                     fast-event-rendering? get-visualizer-data send-events!]]
   [time-time.dynacan.players.gen-poly :as gp]))
(def events (atom []))

(defn nebulosa&planeta-hostil
  [context]
  ;; acordes graves
  ;;
  ;; Mucho menos material
  ;; Dejar que la maquina esté generando el camino
  ;; Utilizamos lo que la maquina produce como material para desarrollar
  ;; Imagen: planeta hostil, entorno raro, desértico, venenoso quizá
  ;; Imagen: nebulosa
  ;;

  (hacia-un-nuevo-universo-perc-refrain-v2p2
    {:out-bus (::nebulosa&planeta-hostil/out-bus context)
     :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
     :rates (interleave (fib-chord-seq (transpose-chord [0 1] (range (* 21 -3) 0 4)))
                        (reverse (fib-chord-seq (transpose-chord [2 3] (range (* 21 -3) 0 4)))))
     :amp 0.6
     :period 30
     :durs [2 3 5 3 8]
     :d-weights {8 1
                 5 1
                 13 1}
     :d-level-weights {0.3 5
                       0.1 2
                       0.2 3
                       0.4 2}
     :a-weights {(rrange 0.01 0.2) 1/4
                 (rrange 0.2 0.8) 1
                 (rrange 1 2) 3
                 (rrange 2 5) 1}
     :on-play (fn [{:keys [synth-args] :as event-data}]
                (when-not (fast-event-rendering? context)
                  (amanecer*guitar-clouds-2 synth-args))
                (collect-event! events {:event event-data
                                        :context context
                                        :root-freq (::nebulosa&planeta-hostil/root-freq context)}))}))

(comment
  #_(-> `events)
  (->> @events
       )
  (->> @events
       (map #(get-visualizer-data %))
       (map event->viz-event)
       (send-events!))
  
  (reset! events [])
  (o/stop)
  (when @habitat-initialized?
    (gp/stop)
    (main/stop-sequencer! hseq/context)
    (reset! recording? {})
    (reset! rec/bufs {}))
  
  (do
    (init!)
    (def section-out (o/audio-bus 4 "section-out-bus"))
    (def qbr-out1 (o/audio-bus 4 "qbr-out-bus"))

    (quad-router-2o
      {:group (groups/mid :tail)
       :in-bus section-out
       :out-bus1 qbr-out1
       :out-bus2 (main-returns :mixed)})

    (rev-filter
      {:group (groups/panners)
       :in-bus qbr-out1})

    (open-inputs-with-rand-pan
      {:inputs inputs
       :preouts preouts})

    (start-rec-loop3!
      {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
       :durs (mapv (fn [_] (rrange 10 20)) (range 40))})

    (nebulosa&planeta-hostil
      {::nebulosa&planeta-hostil/out-bus section-out
       ::nebulosa&planeta-hostil/root-freq 112
       :sequencer/refrain-playback-rate 1
       :sequencer/end-after-ms (* 5 60 1000)}))
  :rcf)
