(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.utils
  (:require
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [taoensso.timbre :as timbre]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [defplug]]))

(defn map-outs
  "Given a sequence of outs, map a signal array to each out."
  [out-offset outs-seq sig]
  (if (and (sequential? outs-seq)
           (sequential? sig))
    (map (fn [out sig]
           (o/out:ar (oc/+ out out-offset) sig))
         outs-seq
         sig)
    (do (timbre/warn "[map-outs] `outs-seq` & `sig` are not both vectors. Resorting to default output method for current synth variation.")
        (o/out outs-seq sig))))

(defplug outs
  {:out-offset 0
   :outs [0 1 2 3]
   :ugen/outs (fn [sig] (map-outs out-offset outs sig))})
