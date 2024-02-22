(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.durationally-faithfull-grain-refrain
  (:require
   [clojure.data.generators :refer [weighted]]
   [hacia-un-nuevo-universo.nebulosa&planeta-hostil :as-alias nebulosa&planeta-hostil]
   [overtone.core :as o]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec :refer [norm-amp recording? silence?]]
   [tieminos.habitat.routing :refer [inputs main-returns
                                     percussion-processes-main-out preouts]]
   [tieminos.habitat.scratch.sample-rec2 :refer [periodize-durs
                                                 rand-latest-buf start-rec-loop3!]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.math.bezier-samples :as bzs]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(do
  (defn scale-adr
    [scale-ratio
     {:keys [buf a d r] :as config}]
    (let [buf-dur (:duration buf)
          adr-dur (+ a d r)
          ratio (* scale-ratio (/ buf-dur adr-dur))]
      (assoc config
             :a (* a ratio)
             :d (* d ratio)
             :r (* r ratio))))
  (scale-adr 1 {:buf {:duration 2} :a 1 :d 1 :r 1}))

(do

  (oe/defsynth true-perc
    ;; TODO pass in manual envelope
    [buf 0
     trig-rate 80
     grain-dur 1/40
     rate 1
     amp 1
     amp-lfo-min 0.5
     amp-lfo 0.1
     start 0.1
     end 0.3
     a 0.1
     d 1
     d-level 0.3
     r 3
     out 0
     lpf-min 100
     lpf-max 2000
     pan 0
     rev-mix 1
     rev-room 0.5
     a-level 1]
    (o/out out
           (-> (o/grain-buf
                :num-channels 1
                :trigger (o/impulse:kr trig-rate)
                :dur grain-dur
                :sndbuf buf
                :rate (o/line:kr 1 rate (+ a d r))
                :pos (+ #_(o/lf-noise0:kr 100 0.00001) (o/line:kr start end (+ a d r)))
                :interp 1
                :pan 0
                :max-grains 1000)
               #_(o/pitch-shift 0.1 rate 0 0)
               #_(o/lpf (lfo 0.1 lpf-min lpf-max))
               #_(#(+ % (* 0.7 (o/b-moog % 500 (lfo 2 0.2 0.6) 2))))
               (#(o/pan-az 4 % :pos (lfo 3 -1 1) :width (lfo 0.1 1 2.5)))
               (o/free-verb rev-mix rev-room)
               (* amp
                  #_(lfo amp-lfo amp-lfo-min 1)
                  (o/env-gen (o/envelope [0 a-level d-level 0] [a d r]
                                         [-1 -5])
                             :action o/FREE)))))

  (defn hacia-un-nuevo-universo-perc-refrain-v3
    "This version can handle rate chords (as a vector of rates)"
    [{:keys [refrain-id web-visualizer-fn-id
             buf-fn period durs rates amp
             amp-fn ;; optional, takes the index and returns an amp value, if present `amp` will be overriden
             d-weights d-level-weights a-weights room-weights
             out-bus
             silence-thresh
             on-play
             refrain-ratio
             dur-scaling-fn]
      :or {refrain-id ::hacia-un-nuevo-universo-perc3
           web-visualizer-fn-id ::hacia-un-nuevo-universo-1
           buf-fn rand-latest-buf
           period 2.5
           durs (bzs/fsf 20 0.1 1)
           rates (range 1 10)
           amp 1
           a-weights {(rrange 0.01 0.1) 10
                      (rrange 2 5) 1/2}
           d-weights {(rrange 0.2 0.3) 5
                      (rrange 0.3 0.5) 3
                      (rrange 0.5 1) 1
                      (rrange 1 5) 1/2}
           d-level-weights {0.3 1}
           room-weights {0.2 2, 2 1/2 4 1/2}
           out-bus (main-returns :non-recordable)
           silence-thresh 0.05
           refrain-ratio 1
           dur-scaling-fn (fn [config] config)}}]
    (let [rates* (map (fn [r] (if (sequential? r) r [r])) rates)]
      (ref-rain
       :id refrain-id
       :durs (periodize-durs period durs)
       :ratio refrain-ratio
       :on-event (on-event
                  (when-let [buf (buf-fn {:index index})]
                    (when-not (silence? silence-thresh buf) ;; allow us to control silences by not playing
                      (let [rate (at-i rates*)
                            amp* (if amp-fn (amp-fn index) amp)]
                        (doseq [r rate]
                          (let [start 0 #_(rrange (rrange 0 0.5) 0.7)
                                end 1 #_(+ start (rrange 0.05 0.3))
                                a (weighted a-weights)
                                trig-rate (+ 90 (rand-int 20))
                                config (dur-scaling-fn {:group (groups/mid)
                                                        :buf buf
                                                        :a a
                                                        :d (/ (+ (/ a 2) (weighted d-weights))
                                                              2)
                                                        :r 1 #_(+ (/ a 2) (weighted d-weights))
                                                        :d-level (weighted d-level-weights)
                                                        :rev-room (weighted room-weights)
                                                        :trig-rate 100
                                                        :grain-dur (/ 1 (/ trig-rate 2))
                                                        :amp-lfo (rrange 0.1 0.4)
                                                        :amp-lfo-min 0.95
                                                        :lpf-max (rrange 2000 10000)
                                                        :start start
                                                        :end end
                                                        :out out-bus
                                                        :pan (rrange -1 1)})]

                            (doseq [synth-args [(assoc config
                                                       :rate (float r)
                                                       :interp (rand-nth [1 2 4])
                                                       :amp (* amp* (rrange 0.2 1) (norm-amp buf)))
                                                #_(assoc config
                                                         :rate (* (rand-nth [2 3/2 5/4 7/4 1/2 1 1 1 1]) r)
                                                         :interp (rand-nth [4])
                                                         :amp (* amp* (rrange 0 0.7) (norm-amp buf)))]]
                              (println "=============BUFF Duration" (:duration buf))
                              (println "=============SYNTH Duration"
                                       (apply + (vals (select-keys config [:a :d :r])))
                                       (select-keys config [:a :d :r]))
                              (on-play {:refrain/data data
                                        :meta {:context/web-visualizer-fn-id web-visualizer-fn-id} ;; TODO
                                        :synth-args synth-args})))))))))))

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

    (hacia-un-nuevo-universo-perc-refrain-v3
     {:out-bus (::nebulosa&planeta-hostil/out-bus context)
      :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis)
                           (remove #(silence? 0.05 %))
                           (take 10)
                           (#(when (seq %) (rand-nth %)))))
      :rates [2 1 3/2]
      :amp 1
      :period 3
      :durs [3]
      :dur-scaling-fn (fn [config] (rand-nth [(scale-adr 1 config)
                                              (scale-adr 1/3 config)]))
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
      :r-weights {1 1}
      :room-weights {0 1}
      :on-play (fn [{:keys [synth-args] :as event-data}]
                 (println "PLAYAING----------")
                 (true-perc synth-args))}))

  #_(nebulosa&planeta-hostil
     {::nebulosa&planeta-hostil/out-bus percussion-processes-main-out}))

(comment
  (o/stop)
  (when @habitat-initialized?
    (main/stop-sequencer! hseq/context)
    (reset! recording? {})
    (reset! rec/bufs {}))
  (do
    (init!)

    (open-inputs-with-rand-pan
     {:inputs inputs
      :preouts preouts})

    (start-rec-loop3!
     {:input-bus-fn (fn [_] (-> @inputs (select-keys [:mic-1]) vals (->> (map :bus))))
      :durs (mapv (fn [_] (rrange 10 20)) (range 40))}))

  (nebulosa&planeta-hostil
   {::nebulosa&planeta-hostil/out-bus percussion-processes-main-out})
  :rcf)
