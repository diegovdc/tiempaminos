(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.processors
  (:require
   [overtone.core :as o]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.async-events :as bardo.comms]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.config :as bardo.config]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.guitar-processes :refer [amp-follower
                                                                                            comb
                                                                                            mod-multifx
                                                                                            mono-in
                                                                                            sided-fm]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.utils :refer [outs]]
   [tieminos.habitat.routing :refer [get-input-bus]]
   [tieminos.math.utils :refer [linexp* linlin*]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [defplug make-synth-fn]]))

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

(def presets-config
  [{:name "RandPanaz"
    :input :guitar-clean
    :synth processor
    :default-config (-> {:a 5
                         :r 5
                         :amp 1}
                        (outs {:out-offset (bardo.config/get-bh-bus :guitar-clean)})
                        (rand-panaz {:pan-width 3
                                     :pan-rate 1}))
    :controls [{:param :amp :name "Amp" :mapping #(linlin* 0 1 0 2 %)}
               {:param :pan-width :name "PanWi" :mapping #(linlin* 0 1 1.2 4 %)}
               {:param :pan-rate :name "PanRt" :mapping #(linexp* 0 1 0.1 5 %)}]}
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
    :controls [{:param :amp :name "Amp" :mapping #(linlin* 0 1 0 2 %)}
               {:param :pan-width :name "PanWi" :mapping #(linlin* 0 1 1.2 4 %)}
               {:param :pan-rate :name "PanRt" :mapping #(linexp* 0 1 0.1 5 %)}]}
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

    :controls [{:param :amp :name "Amp" :mapping #(linlin* 0 1 0 2 %)}
               {:param :pan-width :name "PanWi" :mapping #(linlin* 0 1 1.2 4 %)}
               {:param :pan-rate :name "PanRt" :mapping #(linexp* 0 1 0.1 5 %)}]}])

(defn start-synth!
  "Starts a synth and returns the instance."
  [{:keys [synth] :as _preset} config]
  (synth config))

(defn stop-synth!
  [synth]
  (when (and (o/node? synth) (o/node-active? synth))
    (o/ctl synth :gate 0)))

(defn- init-preset-manager!
  []
  (bardo.comms/dispatch {:type :processor/activate-preset
                         :data {:preset-index 0}}))

(comment)

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
