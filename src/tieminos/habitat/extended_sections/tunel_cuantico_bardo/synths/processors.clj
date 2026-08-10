(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.processors
  (:require
   [clojure.math :refer [round]]
   [clojure.math.combinatorics :as combo]
   [erv.utils.core :refer [round2]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.async-events :as bardo.comms]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.config :as bardo.config]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.guitar-processes :refer [amp-follower
                                                                                            comb
                                                                                            mod-multifx
                                                                                            mono-in
                                                                                            sided-fm]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.utils :refer [outs]]
   [tieminos.habitat.routing :refer [get-input-bus]]
   [tieminos.math.utils :refer [explin* linexp* linlin*]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [defplug make-synth-fn]]
   [tieminos.utils :refer [cb-interpolate]]))

(defplug rand-panaz
  {:pan-rate 0.1
   :pan-width 2
   :ugen/pan (fn [sig]
               (oe/circle-az :num-channels 4
                             :in sig
                             :pos (o/lf-noise1 pan-rate)
                             :width pan-width
                             :orientation 0))})
(defplug hilo-rand-panaz
  {:pan-rate 0.1
   :pan-width 2
   :pan-hilo-cutoff 600
   :ugen/pan (fn [sig]
               (->> [(o/hpf sig pan-hilo-cutoff)
                     (o/lpf sig pan-hilo-cutoff)]
                    (map #(oe/circle-az :num-channels 4
                                        :in %
                                        :pos (o/lf-noise1 pan-rate)
                                        :width pan-width
                                        :orientation 0))
                    (o/mix)))})

(make-synth-fn
 'processor
 (-> {:in   0
      :amp  1
      :a    2
      :r    2
      :gate 1}
     (rand-panaz)
     (outs))
 '(-> (o/in in 1)
      (:ugen/filter)
      (:ugen/pan)
      (:ugen/rev)
      (* amp (o/env-gen (o/env-adsr a 1 1 r :curve -0.5)
                        gate
                        :action o/FREE))
      (:ugen/outs))
 {:reset? true})

(comment
  (require
   '[tieminos.sc-utils.synths.template-synth.v0 :refer [get-variant-data]])

  (do
    (when (o/node-active? p) (o/kill p))

    (def p
      (processor (-> {:in (get-input-bus :guitar)
                      :pan-width 3
                      :pan-rate 1
                      :out-offset (bardo.config/get-bh-bus :guitar-clean)}))))
  (-> p seq)
  (o/ctl p
         :amp 2
         :pan-hilo-cutoff 600
         :pan-rate 0.21
         :pan-width 3))
(comment
  (o/defsynth sound-in-checker
    [in 0
     out 0]
    (o/out out (o/sound-in in)))
  (def s1 (sound-in-checker 20))
  (o/kill s1)

  (o/defsynth in-checker
    [in 0
     out 0]
    (o/out out (o/in in)))
  (def i1 (in-checker (get-input-bus :guitar)))
  (o/kill i1))

;;;;;;;;;;;;;;;;;;
;; * Manager
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** Assumptions
;; 0. Only for guitar
;; 1. Only one processor runs at a time for every input bus
;; 2. They work as presets (filters, etc.) can't be changed
;;    individually
;; 3. There will be a maximum of 8 fader controlls
;; 4. The :default-config will lack `:in` and `outs` configs,
;;    the will be dynamically filled in.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ratio-range
  [factors range-min range-max]
  (->> (combo/combinations factors 2)
       (mapcat (fn [[a b]] [(/ a b) (/ b a)]))
       (filter #(and (>= % range-min) (<= % range-max)))
       set
       sort))
#_(ratio-range (range 1 8) 1/4 4)

(defn quantized-nth
  "Assuming a 0 to 1 range, and a list, quantize any value to the closest index of the list and return the element"
  [n coll]
  (let [n* (-> n (min 1) (max 0))
        max-index (dec (count coll))]
    (when-not (= n n*) (timbre/warn "Out of range value defaulting to: " n* {:input n}))
    (nth coll (round (* n* max-index)))))

#_(quantized-nth 0.54 (ratio-range (range 1 8) 1/4 4))

(defn default-label-mapping
  [n]
  (->> n (round2 2) str))

(defn id-mapping
  "Assuming a value between 0 and 1 returns the value"
  []
  {:mapping identity
   :inv-mapping identity
   :label-mapping default-label-mapping})

(defn quantized-mapping
  "Given a collection returns a mapping and inv-mapping functions.
  The mapping function maps a number between 0 and 1 to an index in the collection.
  The inv-mapping function maps an element of the collection to a number between 0 and 1"
  [coll]
  (let [max-index (dec (count coll))
        inv-mapping (reduce
                     (fn [m [i n]]
                       (assoc m n (/ i max-index)))
                     {}
                     (map-indexed vector coll))]
    {:mapping #(quantized-nth % coll)
     :inv-mapping inv-mapping
     :label-mapping str}))

(defn lin-mapping
  [type in-min in-max out-min out-max]
  (case type
    :linlin {:mapping #(linlin* in-min in-max out-min out-max %)
             :inv-mapping #(linlin* out-min out-max in-min in-max %)
             :label-mapping default-label-mapping}
    :linexp {:mapping #(linexp* in-min in-max out-min out-max %)
             :inv-mapping #(explin* out-min out-max in-min in-max %)
             :label-mapping default-label-mapping}))

(def presets-config
  "Preset configurations.

  The `:controls` vector serves as an interface with the UI and configure IO with it. Specifically each map must have mapping and inv-mapping functions.
    The `mapping` function converts a value from the interface (between 0 and 1) and returns a value used by a synth. This value will also appear in the interface.
    The `inv-mapping` function must convert back from the synth value and into a number used in the interface control's position, that is a number between 0 and 1. This is mostly used for initialization of the interface control.
    A   `label-mapping` function is optional and will ne used to convert the inv-mapping value into a string used for the label in the UI."
  [{:name "RandPanaz"
    :input :guitar-clean
    :synth processor
    :default-config (-> {:a 5
                         :r 5
                         :amp 1}
                        (outs {:out-offset (bardo.config/get-bh-bus :guitar-clean)})
                        (rand-panaz {:pan-width 3
                                     :pan-rate 1}))
    :controls [(merge {:param :amp :name "Amp"} (lin-mapping :linlin 0 1, 0 2))
               (merge {:param :pan-width :name "PanWi"} (lin-mapping :linlin 0 1, 1.2 4))
               (merge {:param :pan-rate :name "PanRt"} (lin-mapping :linexp 0 1, 0.1 5))]}
   {:name "DirtyCb"
    :input :guitar-clean
    :synth mod-multifx
    :default-config (-> {:a 5
                         :r 5
                         :fm-ratio 4
                         :fm-dry-sig-amp 8
                         :pitch-follower-freq 10
                         :ugen/pitch-shifter nil
                         :amp 4}
                        (mono-in {:in 20})
                        (outs {:out-offset (bardo.config/get-bh-bus :guitar-clean)})
                        (rand-panaz {:pan-width 4
                                     :pan-rate 0.5}))
    :controls [(merge {:param :amp :name "Amp"} (lin-mapping :linlin 0 1, 0 4))
               (merge {:param :pan-width :name "PanWi"} (lin-mapping :linlin 0 1, 1.2 4))
               (merge {:param :pan-rate :name "PanRt"} (lin-mapping :linexp 0 1, 0.1 5))]}
   {:name "Sided1/5 Cb1/4"
    :input :guitar-clean
    :synth mod-multifx
    :default-config (-> {:pitch-follower-freq 1
                         :pitch-follower-median 1
                         :a 5
                         :r 5
                         :lpf 2000
                         :amp 16
                         :hpf 300}
                        (amp-follower)
                        (sided-fm {:fm-ratio 1/5
                                   :fm-dry-wet 0.4
                                   :fm-dry-sig-amp 2})
                        (comb {:comb-dry-wet 1
                               :comb-ratio 1/4
                               :comb-dcy 0.1
                               :comb-freq-lag 2})
                        (mono-in {:in 20})
                        (outs {:out-offset (bardo.config/get-bh-bus :guitar-clean)})
                        (rand-panaz {:pan-width 4
                                     :pan-rate 0.5}))

    :controls [(merge {:param :amp :name "Amp"} (lin-mapping :linlin 0 1, 0 32))
               (merge {:param :pan-width :name "PanWi"} (lin-mapping :linlin 0 1, 1.2 4))
               (merge {:param :pan-rate :name "PanRt"} (lin-mapping :linexp 0 1, 0.1 5))
               (merge {:param :fm-dry-wet :name "FMDryWet"} (id-mapping))
               (merge {:param :fm-ratio :name "FMRatio"} (quantized-mapping (ratio-range (range 1 8) 1/8 8)))
               (merge {:param :comb-dry-wet :name "CombDryWet"} (id-mapping))
               (merge {:param :comb-ratio :name "/CombRatio"} (quantized-mapping (ratio-range (range 1 8) 1/4 4)))
               (merge {:param :comb-dcy :name "CombDecay"} (lin-mapping :linlin 0 1, 0.05 0.9))]}
   ;; WIP - see todo below
   {:name "Sided Subtle" ;; derived from: "Sided1/5 Cb1/4"
    :input :guitar-clean
    :synth mod-multifx
    :default-config (-> {:pitch-follower-freq 1
                         :pitch-follower-median 1
                         :a 5
                         :r 5
                         :lpf 2000
                         :amp 16
                         :hpf 300}
                        (amp-follower)
                        (sided-fm {:fm-ratio 1/5
                                   :fm-dry-wet 0.4
                                   :fm-dry-sig-amp 2})
                        (comb {:comb-dry-wet 1
                               :comb-ratio 1/4
                               :comb-dcy 0.1
                               :comb-freq-lag 2})
                        (mono-in {:in 20})
                        (outs {:out-offset (bardo.config/get-bh-bus :guitar-clean)})
                        (rand-panaz {:pan-width 4
                                     :pan-rate 0.5})
                        ;; TODO: incorporate more cleanly
                        (merge {:amp 32.0,
                                :comb-dcy 0.9,
                                :comb-dry-wet 0.47169495,
                                :comb-ratio 2,
                                :fm-dry-wet 0.97358304,
                                :fm-ratio 3,
                                :pan-rate 2.4159750094040486,
                                :pan-width 2.364386713504791}))

    :controls [(merge {:param :amp :name "Amp"} (lin-mapping :linlin 0 1, 0 32))
               (merge {:param :pan-width :name "PanWi"} (lin-mapping :linlin 0 1, 1.2 4))
               (merge {:param :pan-rate :name "PanRt"} (lin-mapping :linexp 0 1, 0.1 5))
               (merge {:param :fm-dry-wet :name "FMDryWet"} (id-mapping))
               (merge {:param :fm-ratio :name "FMRatio"} (quantized-mapping (ratio-range (range 1 8) 1/8 8)))
               (merge {:param :comb-dry-wet :name "CombDryWet"} (id-mapping))
               (merge {:param :comb-ratio :name "/CombRatio"} (quantized-mapping (ratio-range (range 1 8) 1/4 4)))
               (merge {:param :comb-dcy :name "CombDecay"} (lin-mapping :linlin 0 1, 0.05 0.9))
               (merge {:param :comb-freq-lag :name "CombLag"} (lin-mapping :linlin 0 1, 0.01 3))]}])

(defn start-synth!
  "Starts a synth and returns the instance."
  [{:keys [synth] :as _preset} config]
  (synth config))

(defn stop-synth!
  [synth]
  (when (and (o/node? synth) (o/node-active? synth))
    (o/ctl synth :gate 0)))

(defn ctl-synth!
  [{:keys [synth] :as preset} param value]
  (let [id (-> synth :synth (str "." (name param)))
        init-val (or (-> preset :preset :default-config param)
                     (-> synth :args (get (name param))))]
    (when-not init-val
      (throw (ex-info "Unknown init-val" {:preset (-> preset :preset :name)
                                          :param param
                                          :value value})))
    (cb-interpolate
     {:id id
      :dur-ms 5000
      :tick-ms 50
      :init-val init-val
      :target-val value
      :cb (fn [{:keys [val]}]
            #_(timbre/info param val)
            (o/ctl synth param val))})))

(defn- init-preset-manager!
  []
  (bardo.comms/dispatch {:type :bardo.processor/activate-preset
                         :data {:preset-index 0}}))

;;;;;;;;;;;;;;;;;;
;; * Init
;;;;;;;;;;;;;;;;;;

(defn init! []
  (init-preset-manager!)

  (doseq [[in-k out-k] [[:mic-1 :percussion-clean]
                        [:mic-2 :percussion-clean]]]
    (processor (-> {:in (get-input-bus in-k)
                    :pan-width 3
                    :pan-rate 0.3
                    :out-offset (bardo.config/get-bh-bus out-k)}))))

(comment
  (init!))
