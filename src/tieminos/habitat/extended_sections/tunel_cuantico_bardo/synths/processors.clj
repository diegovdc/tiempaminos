(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.processors
  #_{:clj-kondo/ignore [:unused-namespace :unused-referred-var]}
  (:require
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.config :as bardo.config]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.utils :refer [map-outs]]
   [tieminos.habitat.routing :refer [get-input-bus]]
   [tieminos.math.utils :refer [linexp* linlin*]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [defplug make-synth-fn]]))

(defplug rand-panaz
  {:pan-rate 0.1
   :pan-width 2
   :ugen/pan '((fn [sig]
                 (oe/circle-az :num-channels 4
                               :in sig
                               :pos (o/lf-noise1 pan-rate)
                               :width pan-width
                               :orientation 0)))})

(defplug hilo-rand-panaz
  {:pan-rate 0.1
   :pan-width 2
   :pan-hilo-cutoff 600
   :ugen/pan '((fn [sig]
                 (->> [(o/hpf sig pan-hilo-cutoff)
                       (o/lpf sig pan-hilo-cutoff)]
                      (map #(oe/circle-az :num-channels 4
                                          :in %
                                          :pos (o/lf-noise1 pan-rate)
                                          :width pan-width
                                          :orientation 0))
                      (o/mix))))})

(comment oc/+ map-outs)

(defplug outs
  {:out-offset 0
   :outs [0 1 2 3]
   :ugen/outs '((fn [sig] (map-outs out-offset outs sig)))})

(make-synth-fn
 'processor
 (-> {:in   0
      :freq 200
      :amp  1
      :a    2
      :r    2
      :gate 1}
     (rand-panaz)
     (outs))
 '(-> (o/in in 1)
      :ugen/filter
      :ugen/pan
      :ugen/rev
      (* amp (o/env-gen (o/env-adsr a 1 1 r :curve -0.5)
                        gate
                        :action o/FREE))
      :ugen/outs)
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
    :default-config (-> {:amp 1}
                        (rand-panaz {:pan-width 3
                                     :pan-rate 1}))
    :controls [{:param :amp :name "Amp" :mapping #(linlin* 0 1 0 2 %)}
               {:param :pan-width :name "PanWi" :mapping #(linlin* 0 1 1.2 4 %)}
               {:param :pan-rate :name "PanRt" :mapping #(linexp* 0 1 0.1 5 %)}]}])

(def presets-by-input
  (group-by :input presets-config))

;; TODO: maybe move state to bardo.live-state, but do strongly consider using the live-state atom
(defonce ^:private modified-preset-configs (atom {}))
(defonce ^:private active-preset (atom nil))

(defn input->in&outs&group [input]
  (case input
    :guitar-clean (-> {:in (get-input-bus :guitar)}
                      (outs {:out-offset (bardo.config/get-bh-bus :guitar-clean)}))))

(defn get-previous-config! [modified-preset-configs-data preset]
  (if-let [config (get modified-preset-configs-data preset)]
    config
    (let [{:keys [input default-config]} preset
          io-config (input->in&outs&group input)]
      (merge default-config io-config))))

(defn update-ui!
  [preset config]
  (timbre/warn "TODO: Implement `update-ui!`"))

(defn run-preset!
  [preset]
  (let [config (get-previous-config! @modified-preset-configs preset)
        active-preset* @active-preset]
    (when active-preset* (o/ctl active-preset* :gate 0))
    (reset! active-preset (processor config))
    (swap! modified-preset-configs assoc preset config)
    (update-ui! preset config)))
(-> presets-by-input)

(defn- init-preset-manager! []
  (run-preset! (-> presets-by-input :guitar-clean first)))

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
