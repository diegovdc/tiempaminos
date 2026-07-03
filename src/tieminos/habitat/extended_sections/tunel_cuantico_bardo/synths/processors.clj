(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.processors
  #_{:clj-kondo/ignore [:unused-namespace :unused-referred-var]}
  (:require
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.utils :refer [map-outs]]
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
      :amp  1
      :a    2
      :r    2
      :gate 1}
     (outs)
     (rand-panaz))
 '(-> (o/in in 1)
      :ugen/filter
      (* amp (o/env-gen (o/env-adsr a 1 1 r :curve -0.5)
                        gate
                        :action o/FREE))
      :ugen/pan
      :ugen/rev
      :ugen/outs)
 {:reset? true})

(comment
  (require '[tieminos.blackhole :as bh]
           '[tieminos.habitat.routing :refer [get-input-bus]])
  (type (get-input-bus :guitar))

  (when (o/node-active? p) (o/kill p))
  (-> {:in (get-input-bus :guitar)
       :pan-width 3
       :pan-rate 1

       :out-offset (bh/bus 14)}
      (hilo-rand-panaz))
  (def p
    (processor (-> {:in (get-input-bus :guitar)
                    :pan-width 3
                    :pan-rate 1

                    :out-offset (bh/bus 14)}
                   (hilo-rand-panaz))))
  (-> p seq)
  (o/ctl p
         :amp 2
         :pan-hilo-cutoff 600
         :pan-rate 0.21
         :pan-width 3))

;;;;;;;;;;;;;;;;;;
;; * Manager
;;;;;;;;;;;;;;;;;;

