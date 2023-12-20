(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.nebulosa-less-dissonant
  (:require
   [hacia-un-nuevo-universo.nebulosa&planeta-hostil :as-alias nebulosa&planeta-hostil]
   [overtone.core :as o]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec :refer [recording? silence?]]
   [tieminos.habitat.routing :refer [inputs main-returns
                                     percussion-processes-main-out preouts]]
   [tieminos.habitat.scratch.sample-rec2 :refer [amanecer*guitar-clouds-2
                                                 hacia-un-nuevo-universo-perc-refrain-v1p2 quad-router-2o rev-filter start-rec-loop3!]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp]))

(do (def rates (->> (for [h (range 1 14)
                          sub (range 1 6)]
                      (/ h sub))
                    shuffle
                    (take 3)))
    rates)

(do
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

    (hacia-un-nuevo-universo-perc-refrain-v1p2
     {:out-bus (::nebulosa&planeta-hostil/out-bus context)
      :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (remove #(silence? 0.01 %)) (take 2) (#(when (seq %) (rand-nth %)))))
      :rates (map #(* 1/4 %) rates) #_(interleave (fib-chord-seq (transpose-chord [0 1] (range (* 21 -3) 0 4)))
                                                (reverse (fib-chord-seq (transpose-chord [2 3] (range (* 21 -3) 0 4)))))
      :silence-thresh 0.01
      :amp 0.8
      :period 3
      :durs [1 2 3 2]
      :d-weights {
                  ;; 1 1
                  ;; 0.1 1
                   1 1
                  }
      :d-level-weights {
                        ;; 0.3 5
                        ;; 0.5 2
                        ;; 0.1 4
                        0.8 2
                        }
      :a-weights {
                  (rrange 0.01 0.6) 1/4
                  ;; (rrange 0.1 0.8) 1
                    (rrange 1 2) 1/5
                  ;; (rrange 2 5) 5
                  }
      :on-play (fn [{:keys [synth-args] :as event-data}]
                 (amanecer*guitar-clouds-2 (merge synth-args
                                                  {:trig-rate 80
                                                   :grain-dur 1/40
                                                   :rev-room (+ 2 (rand 10))
                                                   ;; :start 0.5
                                                   ;; :end 0.61
                                                   :lpf-min 300
                                                   :lpf-max 10000
                                                   :r 5
                                                   })))}))

  #_(nebulosa&planeta-hostil
   {::nebulosa&planeta-hostil/out-bus percussion-processes-main-out}))

(comment
  (gp/stop)
  (o/stop)

  (when @habitat-initialized?
    (main/stop-sequencer! hseq/context)
    (reset! recording? {})
    (reset! rec/bufs {}))
  (do)
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
    {:input-bus-fn (fn [_] (-> @inputs (select-keys [#_:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
     :durs (mapv (fn [_] (rrange 10 20)) (range 40))})

  (nebulosa&planeta-hostil
    {::nebulosa&planeta-hostil/out-bus section-out})
  :rcf)
