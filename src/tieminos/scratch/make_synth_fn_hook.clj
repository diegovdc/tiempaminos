(ns tieminos.scratch.make-synth-fn-hook
  (:require
   [overtone.core :as o]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [make-synth-fn]]))

(comment
  (make-synth-fn
   'siny
   (-> {:freq [500 900]
        :amp 1})
   '(let [sig (o/sin-osc freq)]
      (-> sig
          (:ugen/freq-mixer freq amp)
          (:ugen/outs))))

  (make-synth-fn
   'siny
   {:freq [500 900]
    :holi 1
    :amp 1}
   '(let [sig (o/sin-osc freq)]
      (-> sig
          (:ugen/outs)
          (:ugen/freq-mixer freq amp))))

  (make-synth-fn
   'siny
   (-> {:freq [500 900]
        :amp 1})
   '(let [sig (o/sin-osc freq)]
      (-> sig
          (:ugen/freq-mixer freq amp)))))
