(ns tieminos.habitat.extended-sections.main
  "For setting up different recompositions of Habitat.
  i.e. like playing only different sections, and extending them."
  (:require
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [tieminos.compositions.garden-earth.synths.recording :as rec]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq :refer [subsequencer]]
   [tieminos.habitat.parts.amanecer :as amanecer]
   [tieminos.habitat.parts.noche :as noche]
   [tieminos.habitat.recording :refer [norm-amp]]
   [tieminos.habitat.scratch.sample-rec2 :refer [rising-upwards
                                                 start-rec-loop!]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp]))

(comment
  main/context
  main/performance-config
  main/start-sequencer!
  main/stop-sequencer!)

(def intercambios-de-energia
  {:context
   (assoc main/context
          :intercambios-de-energia/min-width 2
          :intercambios-de-energia/input-pairs-fn (fn [{:keys [inputs texto-sonoro-rand-mixer-bus]}]
                                                    (let [ts {:bus @texto-sonoro-rand-mixer-bus}]
                                                      [[(:guitar @inputs) ts]
                                                       [(:guitar @inputs) (:mic-1 @inputs)]
                                                       #_[(:guitar @inputs) (:mic-3 @inputs)]
                                                       [ts (:mic-1 @inputs)]
                                                       #_[(:mic-2 @inputs) ts]
                                                       #_[(:mic-3 @inputs) ts]
                                                       #_[(:mic-4 @inputs) ts]]))
          :intercambios-de-energia/convolver-in2-amp 0.3
          :intercambios-de-energia/convolver-max-amp-fn #(rrange 0.2 0.5))
   :sections [[[0 0] (fn [_])]
              [[0 2] #'amanecer/intercambios-de-energia]
              [[10 25] (fn [_] (println "end"))]] #_(main/quick-sections 5 sections)})

(comment
  (main/start-sequencer! intercambios-de-energia))

(defn polinizadores-nocturnos*
  [context]
  (noche/fuego context)
  (Thread/sleep 200)
  (noche/fuego-stop context)
  (noche/polinizadores-nocturnos context)

  (subsequencer
   :sequencer/polinizadores-nocturnos
   context
   (let [scale-1 (->> (cps/make 3 [9 13 15 21 27 31]) :scale)
         make-sample-player-config (fn [scale]
                                     {:buf-fn (fn [_] (-> @rec/bufs vals rand-nth))
                                      :period-dur 20
                                      :total-durs 20
                                      :loop? true
                                      :refrain-id :rising-upwards-loop
                                      :synth-params (fn [{:keys [buf i]}]
                                                      {:amp (* (rrange 0.2 1) (norm-amp buf))
                                                       :rate (scale/deg->freq scale 1 (+ (mod i 43)))})})]
     [[[52 22] (fn [_] (start-rec-loop!))]
      [[53 0] (fn [_] (rising-upwards (make-sample-player-config scale-1)))]
      [[55 0] (fn [_] (rising-upwards (-> (make-sample-player-config scale-1)
                                          (assoc :period-dur 4))))]
      [[55 10] (fn [_]
                 (gp/stop :rising-upwards-loop)
                 (reset! rec/bufs {}))]
      [[57 0] (fn [_] (rising-upwards (make-sample-player-config scale-1)))]
      [[59 0] (fn [_] (reset! rec/bufs {}))]
      [[60 0] (fn [_] (rising-upwards (-> (make-sample-player-config scale-1)
                                          (assoc :period-dur 4))))]
      [[60 10] (fn [_] (rising-upwards (make-sample-player-config scale-1)))]
      [[61 0] (fn [_]
                (reset! rec/bufs {})
                (rising-upwards (-> (make-sample-player-config scale-1)
                                    (assoc :period-dur 4))))]
      [[62 10] (fn [_]
                 (gp/stop :rec-loop)
                 (gp/stop :rising-upwards-loop))]])))

(def polinizadores-nocturnos
  ;; TODO revisar refrains de emision hay cosas raras (aumentos de volumen y saturación del servidor)
  {:context (merge main/context {})
   :sections [#_[[52 15] #'noche/fuego]
              #_[[52 18] #'noche/fuego-stop]
              [[52 22] #'polinizadores-nocturnos*]
              [[62 10] (fn [_] (println "end"))]]
   :initial-sections #'polinizadores-nocturnos*
   :rec? true})

(comment
  (main/start-sequencer! polinizadores-nocturnos)
  (noche/fuego hseq/context)
  (noche/fuego-stop hseq/context))

(comment
  (-> @hseq/context)
  (do  (when @habitat-initialized?
         (main/stop-sequencer! hseq/context))
       (init!)))

