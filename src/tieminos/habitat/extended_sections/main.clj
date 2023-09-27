(ns tieminos.habitat.extended-sections.main
  "For setting up different recompositions of Habitat.
  i.e. like playing only different sections, and extending them."
  (:require
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo :refer [algo-basic-pitch-shifter]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq :refer [subsequencer]]
   [tieminos.habitat.parts.amanecer :as amanecer]
   [tieminos.habitat.parts.noche :as noche]
   [tieminos.habitat.recording  :as rec :refer [norm-amp recording?]]
   [tieminos.habitat.routing :refer [inputs main-returns
                                     percussion-processes-main-out preouts]]
   [tieminos.habitat.scratch.sample-rec2 :refer [hacia-un-nuevo-universo-perc-refrain
                                                 hacia-un-nuevo-universo-perc-refrain-v1p2 hacia-un-nuevo-universo-perc-refrain-v2-scalable-durs
                                                 quad-router-2o rev-filter rising-upwards start-rec-loop! start-rec-loop2!
                                                 start-rec-loop3!]]
   [tieminos.habitat.utils :refer [open-inputs-with-rand-pan]]
   [tieminos.midi.core :refer [midi-in-event oxygen]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

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
   :sequencer/hacia-un-nuevo-universo
   context

    ;; TODO coso percusivo con samples ganulados, breves y reson-eco-reverb, tal vez convlucieon
    ;; Siempre usar últimos sonidos tocados
    ;; Transponer siempre espectral

   [[[62 10]
     (fn [_context]
       (start-rec-loop2!
        {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1]) vals rand-nth :bus))
         :durs (mapv (fn [_] (rrange 5 10)) (range 40))})
       (hacia-un-nuevo-universo-perc-refrain
        {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 5) (#(when (seq %) (rand-nth %)))))
         :rates (map #(* 2 %) [1 6 7 11 9 2 12 8 5 13]) #_(range 1 10)
         :amp 1
         :period 10
         :durs [2 3 5 3]
         :d-weights {8 1
                     5 1
                     3 1}
         :d-level-weights {0.3 5
                           0.1 2
                           0.2 3
                           0.4 2}
         :a-weights {(rrange 0.01 0.3) 2
                     (rrange 1 2) 3
                     (rrange 2 5) 1}}))]

    [[67 0] (fn [_]
              (gp/stop :rec-loop2)
              (gp/stop :hacia-un-nuevo-universo-perc2))]]))

(def hacia-un-nuevo-universo
  {:context (merge main/context {})
   :sections [[[62 10] #'hacia-un-nuevo-universo*]
              #_[[67 45] #'noche/hacia-un-nuevo-universo-stop]]
   :initial-sections #'hacia-un-nuevo-universo*
   :rec? true})


(def fib-21 [{:ratio 1, :bounded-ratio 1024/987, :bounding-period 2} {:ratio 4181/4096, :bounded-ratio 4181/3948, :bounding-period 2} {:ratio 17/16, :bounded-ratio 1088/987, :bounding-period 2} {:ratio 17711/16384, :bounded-ratio 17711/15792, :bounding-period 2} {:ratio 9/8, :bounded-ratio 384/329, :bounding-period 2} {:ratio 305/256, :bounded-ratio 1220/987, :bounding-period 2} {:ratio 5/4, :bounded-ratio 1280/987, :bounding-period 2} {:ratio 323/256, :bounded-ratio 1292/987, :bounding-period 2} {:ratio 21/16, :bounded-ratio 64/47, :bounding-period 2} {:ratio 5473/4096, :bounded-ratio 5473/3948, :bounding-period 2} {:ratio 89/64, :bounded-ratio 1424/987, :bounding-period 2} {:ratio 1449/1024, :bounded-ratio 69/47, :bounding-period 2} {:ratio 377/256, :bounded-ratio 1508/987, :bounding-period 2} {:ratio 3/2, :bounded-ratio 512/329, :bounding-period 2} {:ratio 1597/1024, :bounded-ratio 1597/987, :bounding-period 2} {:ratio 13/8, :bounded-ratio 1664/987, :bounding-period 2} {:ratio 6765/4096, :bounded-ratio 2255/1316, :bounding-period 2} {:ratio 55/32, :bounded-ratio 1760/987, :bounding-period 2} {:ratio 28657/16384, :bounded-ratio 28657/15792, :bounding-period 2} {:ratio 233/128, :bounded-ratio 1864/987, :bounding-period 2} {:ratio 987/512, :bounded-ratio 2N, :bounding-period 2}])

(defn fib-chord [degs] (->> degs
                            (map (fn [deg]
                                   (scale/deg->freq fib-21 1 deg)))))
  (defn fib-chord-seq [chords]
    (map fib-chord chords))

  (defn transpose-chord [chord transpositions]
    (map (fn [t] (map (fn [deg] (+ t deg)) chord)) transpositions))

(comment
  ;; impro sobre hacia un nuevo universo
  (def fib-21 [{:ratio 1, :bounded-ratio 1024/987, :bounding-period 2} {:ratio 4181/4096, :bounded-ratio 4181/3948, :bounding-period 2} {:ratio 17/16, :bounded-ratio 1088/987, :bounding-period 2} {:ratio 17711/16384, :bounded-ratio 17711/15792, :bounding-period 2} {:ratio 9/8, :bounded-ratio 384/329, :bounding-period 2} {:ratio 305/256, :bounded-ratio 1220/987, :bounding-period 2} {:ratio 5/4, :bounded-ratio 1280/987, :bounding-period 2} {:ratio 323/256, :bounded-ratio 1292/987, :bounding-period 2} {:ratio 21/16, :bounded-ratio 64/47, :bounding-period 2} {:ratio 5473/4096, :bounded-ratio 5473/3948, :bounding-period 2} {:ratio 89/64, :bounded-ratio 1424/987, :bounding-period 2} {:ratio 1449/1024, :bounded-ratio 69/47, :bounding-period 2} {:ratio 377/256, :bounded-ratio 1508/987, :bounding-period 2} {:ratio 3/2, :bounded-ratio 512/329, :bounding-period 2} {:ratio 1597/1024, :bounded-ratio 1597/987, :bounding-period 2} {:ratio 13/8, :bounded-ratio 1664/987, :bounding-period 2} {:ratio 6765/4096, :bounded-ratio 2255/1316, :bounding-period 2} {:ratio 55/32, :bounded-ratio 1760/987, :bounding-period 2} {:ratio 28657/16384, :bounded-ratio 28657/15792, :bounding-period 2} {:ratio 233/128, :bounded-ratio 1864/987, :bounding-period 2} {:ratio 987/512, :bounded-ratio 2N, :bounding-period 2}])
  (defn fib-chord [degs] (->> degs
                              (map (fn [deg]
                                     (scale/deg->freq fib-21 1 deg)))))
  (defn fib-chord-seq [chords]
    (map fib-chord chords))

  (defn transpose-chord [chord transpositions]
    (map (fn [t] (map (fn [deg] (+ t deg)) chord)) transpositions))
  (transpose-chord [0 1 2] [0 1 2 3])
  (fib-chord-seq (transpose-chord [0 1 2] [0 1 2 3]))

  (start-rec-loop2!
    {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals rand-nth :bus))
     :durs (mapv (fn [_] (rrange 5 10)) (range 40))})

  (open-inputs-with-rand-pan
    {:inputs inputs
     :preouts preouts})

  (gp/stop :rec-loop2)
  (map #(* 2 %) [1 6 7 11 9 2 12 8 5 13])
  (fib-chord-seq (transpose-chord [0 5 17] [20 24 19 27 33]))

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
                 (rrange 2 5) 1}})


  (declare in1)

  ;; TODO renombrar
  (defn hacia-un-nuevo-universo-live
    [context]
    (subsequencer
      :sequencer/hacia-un-nuevo-universo-live
      context

      [[[87 00] (fn [_]
                  (println "S1-=============")
                  (open-inputs-with-rand-pan
                    {:inputs inputs
                     :preouts preouts}))]
       [[90 00]
        (fn [_]
          (start-rec-loop3!
            {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
             :durs (mapv (fn [_] (rrange 5 10)) (range 40))})
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
             :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
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
             :period 10
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
       [[93 00]
        (fn [_]
          (start-rec-loop3!
            {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
             :durs (mapv (fn [_] (rrange 5 10)) (range 40))})
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
             :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
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
             :period 10
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
       [[96 00]
        (fn [_]
          ;; duraciones y ataques más largos
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
             :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
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
             (interleave (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -5) (* 21 6) 5)))
                         (reverse (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -5) (* 21 6) 5)))))
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
       [[99 00]
        (fn [_]
          ;; acordes graves
          ;;
          ;; Mucho menos material
          ;; Dejar que la maquina esté generando el camino
          ;; Utilizamos lo que la maquina produce como material para desarrollar
          ;; Imagen: planeta hostil, entorno raro, desértico, venenoso quizá
          ;; Imagen: nebulosa
          ;;
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
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
                         (rrange 2 5) 1}}))]
       [[102 00]
        (fn [_]
          ;; grabado en v 2.2.8
          ;; A. manteniéndonos en una sola nota/cuerda/platillo funciona muy bien (2da y 3ra grabaciónes de ese archivo de reaper) - reconocible, humano - milo de pronto golpes secos sobr el platillo que suenan como latidos
          ;; B. moviéndonos entre muchas notas/cuerdas/platillos funciona bien (1ra grabación de ese archivo de reaper) - pero es algo difícil de reconocer, no se siente familiar, sino desconocido, alienígena, no-gaiano - secciones mucho más altas y lejanas del espectro

          ;; Idea: puede haber una transición de esa interioridad espiritual hacia esa cosa desconocida del universo
          (start-rec-loop3!
            {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
             :durs (mapv (fn [_] (rrange 10 20)) (range 40))})
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
             :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
             :silence-thresh 0.05
             :rates (interleave (fib-chord-seq (transpose-chord [1 3 5 8] (range (* 21 -3) (* 21 3) 5)))
                                (fib-chord-seq (transpose-chord [14 15 16 17] (range (* 21 -3) (* 21 3)  5)))
                                (reverse (fib-chord-seq (transpose-chord [1 3 5 8] (range (* 21 -3) (* 21 3) 5))))
                                (fib-chord-seq (transpose-chord [13 15] (range (* 21 -3) (* 21 3) 5)))
                                (reverse (fib-chord-seq (transpose-chord [11 12] (range (* 21 -3) (* 21 3) 5))))
                                (reverse (fib-chord-seq (transpose-chord [14 15 16 17] (range (* 21 -3) (* 21 3) 5)))))
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
                         (rrange 3 5) 2}}))]
       [[102 00]
        ;; v2.2.9
        (fn [_]
          (start-rec-loop3!
            {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
             :durs (mapv (fn [_] (rrange 10 20)) (range 40))})
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
             :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
             :silence-thresh 0.05
             :rates (fib-chord-seq (transpose-chord [0 5] [0]))
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
                         (rrange 3 5) 2}}))]
       [[102 00]
        ;; v2.2.10
        (fn [_]
          (start-rec-loop3!
            {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
             :durs (mapv (fn [_] (rrange 10 20)) (range 40))})
          (ref-rain
            :id :milo-pitch-shifter
            :tempo 60
            :durs [10 10 7]
            :on-event (on-event
                        (println "ev")
                        (algo-basic-pitch-shifter
                          {:group (groups/mid)
                           :in (-> @inputs :mic-1 :bus)
                           :ratio 1/4
                           :amp 1
                           :dur dur-s
                           :out percussion-processes-main-out})
                        (algo-basic-pitch-shifter
                          {:group (groups/mid)
                           :in (-> @inputs :mic-2 :bus)
                           :ratio 1/2
                           :amp 1
                           :dur dur-s
                           :out percussion-processes-main-out})
                        #_(algo-basic-pitch-shifter
                            {:group (groups/mid)
                             :in (-> @inputs :mic-2 :bus)
                             :ratio (+ 3/2 0.2)
                             :amp 1
                             :dur dur-s
                             :out percussion-processes-main-out})))
          #_(hacia-un-nuevo-universo-perc-refrain-v1p2
              {:out-bus in1
               :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
               :silence-thresh 0.05
               :rates (fib-chord-seq (transpose-chord [0 5] [0]))
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
                           (rrange 3 5) 2}}))]
       #_[[110 00]
          (fn [_]
            ;; multiple harmonies in the mid-high register
            (hacia-un-nuevo-universo-perc-refrain-v1p2
              {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
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

       [[105 0] (fn [_]
                  (gp/stop :rec-loop3)
                  (gp/stop :hacia-un-nuevo-universo-perc2))]]))

  (defn hacia-un-nuevo-universo-live-2
    [context]
    (subsequencer
      :sequencer/hacia-un-nuevo-universo-live
      context

      [[[305 00] (fn [_]
                  (println "S1-=============")
                  (open-inputs-with-rand-pan
                    {:inputs inputs
                     :preouts preouts}))]
       [[308 00]
        (fn [_]
          (start-rec-loop3!
            {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
             :durs (mapv (fn [_] (rrange 5 10)) (range 40))})
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
             :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
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
             :period 10
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
       [[313 00]
        (fn [_]
          (start-rec-loop3!
            {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
             :durs (mapv (fn [_] (rrange 5 10)) (range 40))})
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
             :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
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
             :period 10
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
       [[316 00]
        (fn [_]
          ;; duraciones y ataques más largos
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
             :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
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
             (interleave (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -5) (* 21 6) 5)))
                         (reverse (fib-chord-seq (transpose-chord [0 5 13 21] (range (* 21 -5) (* 21 6) 5)))))
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
       #_[[99 00]
        (fn [_]
          ;; acordes graves
          ;;
          ;; Mucho menos material
          ;; Dejar que la maquina esté generando el camino
          ;; Utilizamos lo que la maquina produce como material para desarrollar
          ;; Imagen: planeta hostil, entorno raro, desértico, venenoso quizá
          ;; Imagen: nebulosa
          ;;
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
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
                         (rrange 2 5) 1}}))]
       [[319 00]
        ;; v2.2.9
        (fn [_]
          (start-rec-loop3!
            {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
             :durs (mapv (fn [_] (rrange 10 20)) (range 40))})
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
             :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
             :silence-thresh 0.05
             :rates (fib-chord-seq (transpose-chord [0 5] [0]))
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
                         (rrange 3 5) 2}}))]
       [[324 00]
        (fn [_]
          ;; grabado en v 2.2.8
          ;; A. manteniéndonos en una sola nota/cuerda/platillo funciona muy bien (2da y 3ra grabaciónes de ese archivo de reaper) - reconocible, humano - milo de pronto golpes secos sobr el platillo que suenan como latidos
          ;; B. moviéndonos entre muchas notas/cuerdas/platillos funciona bien (1ra grabación de ese archivo de reaper) - pero es algo difícil de reconocer, no se siente familiar, sino desconocido, alienígena, no-gaiano - secciones mucho más altas y lejanas del espectro

          ;; Idea: puede haber una transición de esa interioridad espiritual hacia esa cosa desconocida del universo
          (start-rec-loop3!
            {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
             :durs (mapv (fn [_] (rrange 10 20)) (range 40))})
          (hacia-un-nuevo-universo-perc-refrain-v1p2
            {:out-bus in1
             :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
             :silence-thresh 0.05
             :rates (interleave (fib-chord-seq (transpose-chord [1 3 5 8] (range (* 21 -3) (* 21 3) 5)))
                                (fib-chord-seq (transpose-chord [14 15 16 17] (range (* 21 -3) (* 21 3)  5)))
                                (reverse (fib-chord-seq (transpose-chord [1 3 5 8] (range (* 21 -3) (* 21 3) 5))))
                                (fib-chord-seq (transpose-chord [13 15] (range (* 21 -3) (* 21 3) 5)))
                                (reverse (fib-chord-seq (transpose-chord [11 12] (range (* 21 -3) (* 21 3) 5))))
                                (reverse (fib-chord-seq (transpose-chord [14 15 16 17] (range (* 21 -3) (* 21 3) 5)))))
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
                         (rrange 3 5) 2}}))]
       [[330 00]
        ;; v2.2.10
        (fn [_]
          (gp/stop :rec-loop3)
          (gp/stop :hacia-un-nuevo-universo-perc2)
          (ref-rain
            :id :milo-pitch-shifter
            :tempo 60
            :durs [10 10 7]
            :on-event (on-event
                        (println "ev")
                        (algo-basic-pitch-shifter
                          {:group (groups/mid)
                           :in (-> @inputs :mic-1 :bus)
                           :ratio 1/4
                           :amp 1
                           :dur dur-s
                           :out percussion-processes-main-out})
                        (algo-basic-pitch-shifter
                          {:group (groups/mid)
                           :in (-> @inputs :mic-2 :bus)
                           :ratio 1/2
                           :amp 1
                           :dur dur-s
                           :out percussion-processes-main-out})
                        #_(algo-basic-pitch-shifter
                            {:group (groups/mid)
                             :in (-> @inputs :mic-2 :bus)
                             :ratio (+ 3/2 0.2)
                             :amp 1
                             :dur dur-s
                             :out percussion-processes-main-out})))
          #_(hacia-un-nuevo-universo-perc-refrain-v1p2
              {:out-bus in1
               :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
               :silence-thresh 0.05
               :rates (fib-chord-seq (transpose-chord [0 5] [0]))
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
                           (rrange 3 5) 2}}))]
       #_[[110 00]
          (fn [_]
            ;; multiple harmonies in the mid-high register
            (hacia-un-nuevo-universo-perc-refrain-v1p2
              {:buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
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

       [[335 0] (fn [_]
                  (gp/stop :rec-loop3)
                  (gp/stop :hacia-un-nuevo-universo-perc2))]]))

  (def hacia-un-nuevo-universo-impro
    {:context (merge main/context {})
     :sections [[[305 00] #'hacia-un-nuevo-universo-live-2]]
     :initial-sections #'hacia-un-nuevo-universo-live-2
     :rec? true})
  :rfc)

(comment
  ;; milo solo, grabado en 2.2.10
  (gp/stop)
  (ref-rain
    :id :milo-pitch-shifter
    :tempo 60
    :durs [10 10 7]
    :on-event (on-event
                (algo-basic-pitch-shifter
                  {:group (groups/mid)
                   :in (-> @inputs :mic-2 :bus)
                   :ratio (at-i [1/2])
                   :amp 1
                   :dur dur-s
                   :out percussion-processes-main-out})
                (algo-basic-pitch-shifter
                  {:group (groups/mid)
                   :in (-> @inputs :mic-2 :bus)
                   :ratio (* 4/3 (at-i [1 3/2 (+ 3/2 1/20) (+ 3/2 1/30)
                                        (+ 7/2 1/30)
                                        (+ 7/2)
                                        (+ 7/2 1/20)]))
                   :amp 1
                   :dur dur-s
                   :out percussion-processes-main-out})))
  (ref-rain
    :id :milo-pitch-shifter2
    :tempo 60
    :durs [1/2 1/5 2 1/3 1/4 1/5]
    :on-event (on-event
                (algo-basic-pitch-shifter
                  {:group (groups/mid)
                   :in (-> @inputs :mic-1 :bus)
                   :ratio (* 2 (at-i [1 2 3 5 7 13 3/2 17/2]))
                   :amp (at-i [0.2 1 1.5])
                   :a (at-i [0.2 ])
                   :dur (* 2 dur-s)
                   :out percussion-processes-main-out}))))

(comment
  ;;
  (gp/stop)
  (def cps (cps/make 2 [1 3 5 7]))
  (midi-in-event
    :midi-input oxygen
    :note-on (fn [{:as event}]
               (let [vel (:velocity event)
                     note (:note event)
                     ratio #_(/ note 24) #_(scale/deg->freq fib-21 1 (- note 40))
                     (scale/deg->freq (:scale cps) 1 (- note 30))
                     amp (/ vel  127)
                     a (/ vel 30)]
                 #_(println note ratio)
                 #_(basic-pitch-shifter
                     {:group (groups/mid)
                      :in (-> @inputs :guitar :bus)
                      :ratio ratio
                      :window 0.1
                      :amp 0.3
                      :a 0.5
                      :out guitar-processes-main-out}))))

  #_(ref-rain
      :id :diego-pitch-shifter
      :tempo 60
      :durs [1/2 1/5 2 1/3 1/4 1/5]
      :on-event (on-event

                  (algo-basic-pitch-shifter
                    {:group (groups/mid)
                     :in (-> @inputs :guitar :bus)
                     :ratio (* 2 (at-i [1 2 3 5 7 13 3/2 17/2]))
                     :amp (at-i [0.2 1 1.5])
                     :a (at-i [0.2 ])
                     :dur (* 2 dur-s)
                     :out percussion-processes-main-out})))

  (start-rec-loop3!
    {:input-bus-fn (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals (->> (map :bus))))
     :durs (mapv (fn [_] (rrange 10 20)) (range 40))})

  (hacia-un-nuevo-universo-perc-refrain-v2-scalable-durs
    {:out-bus in1
     :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
     :silence-thresh 0.05
     :rates (interleave (fib-chord-seq (transpose-chord [1 3 5 8] (range (* 21 -3) (* 21 3) 5)))
                        (fib-chord-seq (transpose-chord [14 15 16 17] (range (* 21 -3) (* 21 3)  5)))
                        (reverse (fib-chord-seq (transpose-chord [1 3 5 8] (range (* 21 -3) (* 21 3) 5))))
                        (fib-chord-seq (transpose-chord [13 15] (range (* 21 -3) (* 21 3) 5)))
                        (reverse (fib-chord-seq (transpose-chord [11 12] (range (* 21 -3) (* 21 3) 5))))
                        (reverse (fib-chord-seq (transpose-chord [14 15 16 17] (range (* 21 -3) (* 21 3) 5)))))
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
  (fib-chord-seq (transpose-chord [0 6 12 18] (range 21))) ;; acorde bonito, muy liso
  (fib-chord-seq (transpose-chord [0 4 8 12 16 20 24 28] [0 1])) ;; calido con un poco de disonancia
  (fib-chord-seq (transpose-chord [ 11 15 19] (range 21))) ;; estable claro (segmento de arriba: 4-4)
  (fib-chord-seq (transpose-chord [ 10 15 20] (range 21))) ;; nocturno (5-5)
  (flatten [(fib-chord-seq (transpose-chord [0 9 16] [20 19 2 27 23 34 50 48]))
            (fib-chord-seq (transpose-chord [8] [20 19 2 27 23 34 50 48]))
            (fib-chord-seq (transpose-chord [-2 13 18] [20 19 27 23 3 34 50 48]))
            (fib-chord-seq (transpose-chord [3] [20 19 2 27 23 34 50 48]))
            (fib-chord-seq (transpose-chord [-15 21] [20 19 27 23 34 50 48 0]))
            (fib-chord-seq (transpose-chord [13] [20 19 2 27 23 34 50 48]))])
  )


(comment
  (reset! recording? {})
  (reset! rec/bufs {})
  (main/start-sequencer! hacia-un-nuevo-universo-impro))

(comment
  ;; TODO generar función para abrir y cerrar los micros para probar
  (-> @hseq/context)
  (fn [_] (-> @inputs (select-keys [:guitar :mic-1 :mic-2]) vals rand-nth :bus))
  (timbre/set-level! :info)
  (do  (when @habitat-initialized?
         (main/stop-sequencer! hseq/context))
       (init!))

  ;; also part of the initialization of hacia un nuevo universo
  (def in1 (o/audio-bus 4 "test-in-bus"))
  (def out1 (o/audio-bus 4 "test-out-bus1"))
  (def out2 (o/audio-bus 4 "test-out-bus1"))

  (def qbr (quad-router-2o {:group (groups/mid :tail)
                            :in-bus in1
                            :out-bus1 out1
                            :out-bus2 (main-returns :mixed)} ))

  (o/stop)
  (rev-filter
    {:group (groups/panners)
     :in-bus out1})

  (open-inputs-with-rand-pan
    {:inputs inputs
     :preouts preouts}))
