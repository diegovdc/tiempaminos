(ns tieminos.sc-utils.synths.v1
  (:require
   [overtone.core :as o]))

(defn lfo [freq min* max*]
  (o/lin-lin (o/lf-noise1 freq) -1 1 min* max*))

(defn lfo-kr [freq min* max*]
  (o/lin-lin:kr (o/lf-noise1:kr freq) -1 1 min* max*))

(defn lfo0-kr [freq min* max*]
  (o/lin-lin:kr (o/lf-noise0:kr freq) -1 1 min* max*))

(defn ctl-range
  "Ensures an `o/control-bus` will stay within a given range,
  EVEN when not set when calling the synth.
  Expects the ctl-bus to provide values between 0 and 1."
  [ctl-bus min* max*]
  (-> ctl-bus
      (o/in:kr 1)
      (o/lin-lin:kr 0 1 min* max*)
      (o/clip min* max*)))
