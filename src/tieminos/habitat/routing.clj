(ns tieminos.habitat.routing
  (:require [overtone.core :as o]
            [tieminos.overtone-extensions :refer [defsynth]]))

(comment
  ;;  USAGE
  (defsynth input
    [in 0 out 0]
    (o/out out (o/sound-in in)))
  (def guitar (input (:guitar ins) 0))
  (o/ctl guitar :out (reaper-returns 2))
  (o/stop))

(def ins
  "Input name to blackhole channels map"
  {:guitar 20
   :mic-1  21
   :mic-2  22
   :mic-3  23
   :mic-4  24
   :mic-5  25})

(def reaper-returns
  "Returns map, numbered after the returns in reaper i.e. 1 based numbers"
  (let [starting-chan 29
        n-chans 4
        total-returns 4
        return-outs (range starting-chan
                           (+ starting-chan
                              (* n-chans total-returns))
                           n-chans)]
    (into {} (map-indexed (fn [i out] [(inc i) out]) return-outs))))

;;;;;;;;;;;
;; Buses ;;
;;;;;;;;;;;

(def guitar-bus (o/audio-bus 1 "guitar-bus"))
