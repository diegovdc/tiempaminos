(ns tieminos.tierra-mar.v1.olivo
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.utils.core :refer [period-reduce]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.base :refer [base-freq]]
   [tieminos.math.utils :refer [normalize]]
   [tieminos.midi.core :refer [midi-in-event]]
   [tieminos.sc-utils.groups.v1 :as sc.groups]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [make-synth-fn]]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.tierra-mar.v1.arp :as tm.arp]
   [tieminos.tierra-mar.v1.configs :as tm.configs]
   [tieminos.tierra-mar.v1.harmonies.explorations-v2 :as tm.har]
   [tieminos.tierra-mar.v1.state :as tm.state]
   [tieminos.tierra-mar.v1.synths :refer [+outs1 cristal-liquidizado-2
                                          panaz-line]]
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
                (println "rama" i)

                (doseq [in (->> ins
                                shuffle
                                (take 3))]
                  (let [outs (map dec (make-branch-path (rrand 5 15)))]
                    (rama {:in in
                           :amp (rrand 1.3 2.7) #_(rrand 2 12)
                           :dur (weighted {#(rrand 4.0 6) 4
                                           #(rrand 6.0 15) 1})
                           :asr (normalize [1 3 5])
                           :curve -2
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

(def spiral-a
  (range 1 15))

(def spiral-b
  "Shares start and end nodes with `spiral-a` (1 & 14)"
  (concat [1]
          (range 16 27)
          [14]))

(defn calculate-spiral-outs-range
  [total-outs min-offset max-len prev-spiral-outs-range-data]
  (let [[prev-start prev-end] prev-spiral-outs-range-data
        start (if (= 0 prev-start prev-end)
                0
                (rrand
                 (+ min-offset prev-start)
                 prev-end))
        end (+ start (rrand (inc min-offset) max-len))]
    (->> (range start (inc end))
         (map #(min % (dec total-outs))))))

(defn reset-spiral-outs-range-atom!
  ([spirals-state-atom spiral-key total-outs outs]
   (let [freqs (frequencies outs)]
     (swap! spirals-state-atom
            assoc
            spiral-key
            (if (> (get freqs (dec total-outs) 0) 2)
              [0 0]
              [(first outs) (last outs)])))))

(defn map-outs-to-spiral
  "Maps the outs returned by `calculate-spiral-outs-range` to an ordered list of numbers that represent an actual spiral."
  [offset spiral outs]
  (map #(nth spiral (min (dec (count spiral))
                         (+ offset %)))
       outs))

(defn get-outs-data
  "Returns two sequences of outs, a `virtual` which always from 0 to `total-outs` and a `real` which has the outs mapped to the given `spiral`."
  [total-outs offset max-len spiral prev-outs-range]
  (let [virtual-outs (calculate-spiral-outs-range total-outs
                                                  1
                                                  max-len
                                                  prev-outs-range)
        real-outs (map-outs-to-spiral offset spiral virtual-outs)]
    {:virtual-outs virtual-outs
     :real-outs real-outs}))

(def ^:private default-spirals-state
  {:offset 0
   :max-len 4
   :total-outs 14
   :spiral-a-prev-range [0 0]
   :spiral-b-prev-range [0 0]})

(defonce ^:private spirals-state
  (atom default-spirals-state))

(defn gen-spiral-outs-seq!
  [spiral-k]
  (let [{:keys [offset
                max-len
                total-outs]} @spirals-state
        spiral (case spiral-k
                 :spiral-a spiral-a
                 :spiral-b spiral-b)
        spiral-range-k (case spiral-k
                         :spiral-a :spiral-a-prev-range
                         :spiral-b :spiral-b-prev-range)
        spiral-range (@spirals-state spiral-range-k)
        _ (when-not spiral-range (throw (ex-info "Unknown `spiral-prev-range-k`" {:spiral-k spiral-k
                                                                                  :spiral-prev-range-k spiral-range-k})))
        {:keys [virtual-outs real-outs]} (get-outs-data
                                          total-outs
                                          offset
                                          max-len
                                          spiral
                                          spiral-range)]
    (reset-spiral-outs-range-atom! spirals-state
                                   spiral-range-k
                                   total-outs
                                   virtual-outs)
    real-outs))

(comment
  (rain.v2/stop)
  (reset! spirals-state default-spirals-state)
  (-> @spirals-state)
  (gen-spiral-outs-seq! :spiral-a)
  #_(swap! spirals-state assoc :reset-range [0 0])
  (swap! spirals-state assoc :total-outs 14)
  (swap! spirals-state assoc :offset 0)
  (doseq [_ (range 20)]
    (println (gen-spiral-outs-seq! :spiral-a))))

(defn set-spirals-params!
  "`total-outs`: the total number of available outputs, so if `5` is set only the first five outputs will be used (unless transposed by the offset)
  `height-offset`: the offset from the starting point of the spiral (0 bieng the bottom)"
  [total-outs height-offset]
  (swap! spirals-state assoc
         :total-outs total-outs
         :offset height-offset))

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
                  (reset-spiral-outs-range-atom! prev-spiral-outs-range
                                                 outs)
                  (println "vozpiral:  " dur-s "s")
                  (rama {:in (tm.configs/get-input :voz-1)
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
;; * MIDI ctl
;;;;;;;;;;;;;;;;;;

(defn set-section!
  [val]
  (timbre/info "Playing section" val)
  (case val
    0 (do (reset! spirals-state default-spirals-state)
          (set-spirals-params! 5 0))
    1 (set-spirals-params! 7 0)
    2 (set-spirals-params! 9 0)
    3 (set-spirals-params! 14 0)
    4 (set-spirals-params! 14 0)))

(def ^:private cc-responses
  {(tm.configs/get-midi-cc :olivo/voice-spirals-sections) #'set-section!})

(defn call-cc-response
  [cc val]
  (when-let [f (cc-responses cc)]
    (f val)))

(comment

  (midi-in-event
   :midi-input (tm.configs/get-midi-sink)
   :cc (fn [{cc :note
             val :velocity
             :as ev}]
         (when (= (tm.configs/get-midi-chan :olivo) (:channel ev))
           (call-cc-response cc val)))))

;;;;;;;;;;;;;;;;;;
;; * Gongs
;;;;;;;;;;;;;;;;;;

(def sample-data
  {:gong-ch {:path "/Users/diego/Music/samples/Milo/gong-chico.wav"
             :freq 380.3}
   :gong-gd {:path "/Users/diego/Music/samples/Milo/gong-grande.wav"
             :freq 194.8}
   :tam-tam {:path "/Users/diego/Music/samples/Milo/tam-tam.wav"
             :freq 128.6}})

(defn freq->root-ratio
  [freq]
  (period-reduce (/ base-freq freq)))

(defn- load-samples!
  []
  (->> sample-data
       (map (juxt first (fn [[_ m]]
                          (let [freq (:freq m)]
                            (assoc (o/load-sample (:path m))
                                   :freq freq
                                   :root-ratio (freq->root-ratio freq))))))
       (into {})))

(comment
  (def samples (load-samples!))
  (-> samples :tam-tam :root-ratio))

(defonce state (atom {}))
(comment
  (rain.v2/stop ::gongs)
  (rain.v2/ref-rain
   :id ::gongs
   :durs (fn [_] (apply rrand (:gongs/durs @state [1 3])))
   :on-event
   (rain.v2/on-event
    (let [buf (-> [:gong-ch
                   :gong-gd
                   :tam-tam]
                  rand-nth
                  samples)
          pos (rand-int (:n-samples buf))]
      (doseq [_ (range (rrand 2 8))]
        (let [outs (map dec (make-branch-path (rrand 4 8)))]
          (cristal-liquidizado-2
           {:buf buf
            :rate (* (:root-ratio buf)
                     (rand-nth [1/16 1/8 1/4 1/2 1 2])
                     (-> tm.har/olivo
                         (rand-nth)
                         :bounded-ratio))
            :env-levels [0 1 0]
            :env-durs (normalize [0.5 0.5])
            :buf-pos pos
            :dur (rrand 2 5)
            :amp (* (:gongs/amp @state 1)
                    (apply rrand (:gongs/amp-range @state [0.3 0.8])))
            :rev-room (rrand 0.5 1)
            :rev-mix (rrand 0.3 0.6)
            :delay-time (rrand 0.1  0.7)
            :delay-dcy (rrand 0.5  2)
            :delay-amp 0
            :ugen/filter '(#(-> % (overtone.core/moog-ladder 3000 0.5) (* 3)))
            :width-durs (shuffle [0.5 0.3 0.1 0.7])
            :min-width 4
            :max-width (rrand 4 (count outs))
            :out-offset (tm.configs/get-output :olivo-tree-top-30ch)
            :outs outs})))))))

(reset! state
        {:gongs/durs [3 5]
         :gongs/amp 2
         :gongs/amp-range [1 2]})
;;;;;;;;;;;;;;;;;;
;; * Main
;;;;;;;;;;;;;;;;;;

(defn init! []

  (start-arp!))

(defn stop!
  [])
