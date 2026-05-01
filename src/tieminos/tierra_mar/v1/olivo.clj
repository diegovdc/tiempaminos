(ns tieminos.tierra-mar.v1.olivo
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [tieminos.blackhole :as bh]
   [tieminos.math.utils :refer [normalize]]
   [tieminos.sc-utils.groups.v1 :as sc.groups]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [make-synth-fn]]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.tierra-mar.v1.arp :as tm.arp]
   [tieminos.tierra-mar.v1.configs :as tm.configs]
   [tieminos.tierra-mar.v1.state :as tm.state]
   [tieminos.tierra-mar.v1.synths :refer [+outs1 panaz-line]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]
   [time-time.standard :refer [rrand]]))

;; * Olivo
;; ** fase 1
;; - arpa-flauta: fluido vital en espiral ascendente
;; - gong/sintes - ramas-hojas: estiramientos en el espacio ambisónico (az-widths)
;; - guitarra
;; ** fase 2
;; flauta desparaece: mucho reverb se convierte en lluvia
;; lo demás continúa
;;
;;
(comment
  (init!)
  (stop!))

;;;;;;;;;;;;;;;;;;
;; * Arp
;;;;;;;;;;;;;;;;;;

(defn start-arp!
  []
  (tm.state/set-arp-pattern! tm.state/state {:name "stateful-rise"
                                             :fn (partial tm.arp/stateful-rise
                                                          {:id ::fase-a
                                                           :min-len 4
                                                           :max-len 8
                                                           :min-deg -18
                                                           :max-deg 12
                                                           :intervals [1 2 3]})})
  (tm.arp/start-sample-arp! {:group (sc.groups/early)
                             :out-fn (fn [_i]
                                       (tm.configs/get-audio-bus
                                        (rand-nth [:arp->nubosidad
                                                   :arp->nubosidad2])))}))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Copa del Olivo
;; ** (gong branched paths)
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn  get-node-val
  [weights min* max* prev-val]
  (loop []
    (let [x (+ prev-val (weighted weights))]
      (if (< (dec min*) x (inc max*))
        x
        (recur)))))
#_(get-node-val {-10 3 10 1 1 4 -1 2} 1 40 35)

(defn make-branch-path
  "For traversing ring paths.
  +-7 goes one level up, and +-1 moves sideways"
  [len]
  (let [weigths {-7 (rand-nth [3 2 4])
                 7 (rand-nth [1 2])
                 1 (rand-nth [2 4 1 5])
                 -1 (rand-nth [2 4 1 5])}]
    (->> (range len)
         (reduce (fn [acc _]
                   (conj acc
                         (get-node-val weigths 1 28 (last acc))))
                 [(rrand 1 29)]))))
(make-branch-path 10)

;;;;;;;;;;;;;;;;;;
;; * Synthdefs
;;;;;;;;;;;;;;;;;;

(comment
  ;; for import
  (lfo-kr 2 0 1))

(make-synth-fn
 'rama
 (-> {:in 0
      :filter-freq 2000
      :filter-q 0.3
      :amp 0.5
      :dur 2
      :asr [0.1 0.6 0.3]
      :curve 0
      :rev-mix 0.3
      :filtered-amp 0.8
      :unfiltered-amp 0.4}
     panaz-line
     +outs1)
 '(-> (o/sound-in in)
      (#(+ (* unfiltered-amp  %)
           (* filtered-amp  (o/moog-ladder % filter-freq filter-q))))
      (* amp
         (o/env-gen #_(o/env-perc 0.5 0.5)
          (o/envelope [0 1 1 0] asr curve)
                    :time-scale dur
                    :action o/FREE))
      :ugen/panner
      (o/free-verb rev-mix 0.7 0.7)
      :ugen/outs
      #_(#(o/out 0 %)))
 {:reset? true})

;;;;;;;;;;;;;;;;;;
;; * Ramas-sintes
;;;;;;;;;;;;;;;;;;

(defn stereo-bus
  [bus]
  [bus (inc bus)])

(defn start-ramasintes-loop!
  []
  (let [ins (concat (stereo-bus (tm.configs/get-input :olivo/surge-pad))
                    (stereo-bus (tm.configs/get-input :olivo/plamonic-phantom-resonance))
                    (stereo-bus (tm.configs/get-input :olivo/plamonic-hal-bop)))
        filter-freqs {#(rrand 100 300) 7
                      #(rrand 300 600) 7
                      #(rrand 600 1000) 5
                      #(rrand 1000 4000) 4
                      #(rrand 4000 8000) 4
                      #(rrand 8000 20000) 4}]
    (rain.v2/ref-rain
     :id ::ramasintes
     :durs (fn [_] (rrand 3 4.0))
     :on-event (rain.v2/on-event
                (println i)

                (doseq [in (->> ins
                                shuffle
                                (take 3))]
                  (let [outs (map dec (make-branch-path (rrand 5 15)))]
                    (rama {:in in
                           :amp (rrand 0.7 1.6) #_(rrand 2 12)
                           :dur (weighted {#(rrand 4.0 6) 4
                                           #(rrand 6.0 15) 1})
                           :asr (normalize (repeatedly 3 rand))
                           :rev-mix (weighted {0 3
                                               #(rrand 0.0 1) 2})
                           :filter-freq (weighted filter-freqs)
                           :filter-q (rrand 0.3 0.9)
                           :width-durs (shuffle [0.05 0.15 0.1 0.7])
                           :min-width 4
                           :max-width (rrand 4 (count outs))
                           :out-offset (tm.configs/get-output :olivo-tree-top-30ch)
                           :outs outs})))))))

(comment
  (start-ramasintes-loop!)
  (rain.v2/stop ::ramasintes))

;;;;;;;;;;;;;;;;;;
;; * TODO espirales de la voz
;;;;;;;;;;;;;;;;;;

(defn calculate-spiral-outs-range
  [total-outs prev-spiral-outs-range-data]
  (let [[prev-start prev-end] prev-spiral-outs-range-data
        min-offset 2
        start (if (= 0 prev-start prev-end)
                0
                (+ min-offset
                   (rrand  prev-start prev-end)))
        end (+ start (rrand (inc min-offset) 4))]
    (->> (range start (inc end))
         (map #(min % total-outs)))))

(defn reset-spiral-outs-range-atom!
  [range-atom outs]
  (let [freqs (frequencies outs)]
    #_(println)
    (reset! range-atom
            (if (> (get freqs 28 0) 2)
              [0 0]
              [(first outs) (last outs)]))))

#_(let [prev-spiral-outs-range (atom [0 0])
        total-outs 28]
    (doseq [_ (range 20)]
      (let [outs (calculate-spiral-outs-range total-outs @prev-spiral-outs-range)]
        (println outs)
        (reset-spiral-outs-range-atom!
         prev-spiral-outs-range outs))))

(defn start-vozpiral-loop!
  [durs-fn]
  (let [prev-spiral-outs-range (atom [0 0])]
    (rain.v2/ref-rain
     :id ::vozpiral
     :durs durs-fn
     :on-event (rain.v2/on-event
                (let [total-outs 28
                      outs (calculate-spiral-outs-range
                            total-outs
                            @prev-spiral-outs-range)]
                  (println outs)
                  (reset-spiral-outs-range-atom!
                   prev-spiral-outs-range outs)
                  (println "duration:  " dur-s)
                  (rama {:in (tm.configs/get-input :voz-main)
                         :amp 8
                         :dur (* dur-s
                                 (weighted {#(rrand 1.3 2) 4
                                            #(rrand 2.0 3) 1}))
                         :asr (normalize [1 1 1])
                         :rev-mix (rrand 0.4 0.6)
                         :filtered-amp 0 #_(rrand 0.4 0.7)
                         :unfiltered-amp 0.8
                         :filter-freq 3000
                         :width-durs (shuffle [0.05 0.15 0.1 0.7])
                         :min-width 2
                         :max-width (rrand 4 6)
                         :out-offset (tm.configs/get-output :olivo-spiral-arp-28ch)
                         :outs outs}))))))

(defn stop-vozpiral-loop!
  []
  (rain.v2/stop ::vozpiral))

(comment
  (start-vozpiral-loop! (fn [_] (rrand 3 8.0)))

  (stop-vozpiral-loop!)

  (rain.v2/stop)
  (o/stop))

;;;;;;;;;;;;;;;;;;
;; * Main
;;;;;;;;;;;;;;;;;;

(defn init! []

  (start-arp!))

(defn stop!
  [])
