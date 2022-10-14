(ns tieminos.sc-utils.synths.v1
  (:require
   [overtone.core :as o]))

(defn lfo [freq min* max*]
  (o/lin-lin (o/lf-noise1 freq) -1 1 min* max*))

(defn lfo-kr [freq min* max*]
  (o/lin-lin:kr (o/lf-noise1:kr freq) -1 1 min* max*))

(defn lfo0-kr [freq min* max*]
  (o/lin-lin:kr (o/lf-noise0:kr freq) -1 1 min* max*))
