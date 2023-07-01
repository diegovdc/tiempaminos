(ns tieminos.habitat.extended-sections.main
  "For setting up different recompositions of Habitat.
  i.e. like playing only different sections, and extending them."
  (:require
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq :refer [subsequencer]]
   [tieminos.habitat.parts.amanecer :as amanecer]
   [tieminos.habitat.parts.noche :as noche]
   [tieminos.habitat.recording :refer [norm-amp]]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :refer [inputs]]
   [tieminos.habitat.scratch.sample-rec2 :refer [hacia-un-nuevo-universo-perc-refrain rand-latest-buf
                                                 rising-upwards start-rec-loop! start-rec-loop!2]]
   [tieminos.math.bezier-samples :as bzs]
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
   :sections [[[52 22] #'polinizadores-nocturnos*]
              [[62 10] (fn [_] (println "end"))]]
   :initial-sections #'polinizadores-nocturnos*
   ;; :rec? true
   })

(defn hacia-un-nuevo-universo*
  [context]
  (noche/hacia-un-nuevo-universo context)
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
     ;; TODO coso percusivo con samples ganulados, breves y reson-eco-reverb, tal vez convlucieon
     ;; Siempre usar últimos sonidos tocados
     ;; Transponer siempre espectral
     [[[62 10] (fn [_]
                 (start-rec-loop!2
                  {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals rand-nth :bus))
                   :durs (mapv (fn [_] (rrange 1 3)) (range 40))})
                 (hacia-un-nuevo-universo-perc-refrain
                  {:buf-fn rand-latest-buf
                   :period 5
                   :amp 5
                   :durs (bzs/fsf 10 0.1 1)
                   :rates (concat (map #(* % 4) (range 1 14)))
                   :d-weights {(rrange 0.2 0.3) 5
                               (rrange 0.3 0.5) 3
                               (rrange 0.5 1) 1
                               (rrange 1 5) 2}}))]
      [[63 30] (fn [_] (hacia-un-nuevo-universo-perc-refrain
                        {:buf-fn rand-latest-buf
                         :period 5
                         :amp 8
                         :durs (bzs/fsf 18 0.1 1)
                         :rates (concat (range 1 14)
                                        (map #(/ % 2) (range 14 29)))
                         :a-weights {(rrange 0.01 0.1) 3
                                     (rrange 2 5) 1}
                         :d-weights {(rrange 0.2 0.3) 5
                                     (rrange 0.3 0.5) 3
                                     (rrange 0.5 1) 1
                                     (rrange 1 5) 1}}))]
      [[64 40] (fn [_] (hacia-un-nuevo-universo-perc-refrain
                        {:buf-fn rand-latest-buf
                         :period 3.5
                         :amp 8
                         :durs (bzs/fsf 20 0.1 1)
                         :rates (concat (map #(/ % 4) (range 64 128)))
                         :a-weights {(rrange 0.01 0.1) 6
                                     (rrange 2 5) 1}
                         :d-weights {(rrange 0.2 0.3) 5
                                     (rrange 0.3 0.5) 3
                                     (rrange 0.5 1) 1.5
                                     (rrange 1 5) 0.5}}))]
      [[65 0] (fn [_] (hacia-un-nuevo-universo-perc-refrain
                       {:buf-fn rand-latest-buf
                        :period 3
                        :amp 9
                        :durs (bzs/fsf 20 0.1 1)
                        :rates (concat (map #(/ % 4) (range 64 128)))
                        :d-weights {(rrange 0.2 0.3) 5
                                    (rrange 0.3 0.5) 3
                                    (rrange 0.5 1) 1
                                    (rrange 1 5) 1/2}}))]
      [[65 10] (fn [_] (hacia-un-nuevo-universo-perc-refrain
                        {:buf-fn rand-latest-buf
                         :period 2.5
                         :amp 10
                         :durs (bzs/fsf 20 0.1 1)
                         :rates (concat (map #(/ % 4) (range 64 128)))
                         :d-weights {(rrange 0.2 0.3) 5
                                     (rrange 0.3 0.5) 3
                                     (rrange 0.5 1) 1
                                     (rrange 1 5) 1/2}}))]
      [[65 30] (fn [_] (hacia-un-nuevo-universo-perc-refrain
                        {:buf-fn rand-latest-buf
                         :period 2.5
                         :amp 10
                         :durs (bzs/fsf 20 0.1 1)
                         :rates (concat (map #(/ % 4) (range 64 128)))
                         :d-weights {(rrange 0.2 0.3) 5
                                     (rrange 0.3 0.5) 3
                                     (rrange 0.5 1) 1
                                     (rrange 1 5) 1/2}}))]
      [[66 0] (fn [_] (hacia-un-nuevo-universo-perc-refrain
                       {:buf-fn rand-latest-buf
                        :period 2.5
                        :amp 7
                        :durs (bzs/fsf 20 0.1 1)
                        :rates (concat (range 1 38))
                        :d-weights {(rrange 0.2 0.3) 5
                                    (rrange 0.3 0.5) 3
                                    (rrange 0.5 1) 1
                                    (rrange 1 5) 1/2}
                        :a-weights {(rrange 0.01 0.1) 5
                                    (rrange 2 5) 1}}))]
      [[66 30] (fn [_] (hacia-un-nuevo-universo-perc-refrain
                        {:buf-fn rand-latest-buf
                         :period 4
                         :amp 5
                         :durs (bzs/fsf 20 0.1 1)
                         :rates (concat (range 1 19))
                         :d-weights {(rrange 0.2 0.3) 5
                                     (rrange 0.3 0.5) 3
                                     (rrange 0.5 1) 1
                                     (rrange 1 5) 1/2}}))]

      [[67 0] (fn [_]
                (gp/stop :rec-loop2)
                (gp/stop :hacia-un-nuevo-universo-perc2))]])))

(def hacia-un-nuevo-universo
  {:context (merge main/context {})
   :sections [[[62 10] #'hacia-un-nuevo-universo*]
              #_[[67 45] #'noche/hacia-un-nuevo-universo-stop]]
   :initial-sections #'hacia-un-nuevo-universo*
   ;; :rec? true
   })

(comment
  (main/start-sequencer! hacia-un-nuevo-universo))

(comment
  ;; TODO generar función para abrir y cerrar los micros para probar
  (-> @hseq/context)
  (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals rand-nth :bus))
  (timbre/set-level! :info)
  (do  (when @habitat-initialized?
         (main/stop-sequencer! hseq/context))
       (init!)))

