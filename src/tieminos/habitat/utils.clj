(ns tieminos.habitat.utils
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.panners :refer [stop-panner!]]
   [tieminos.math.bezier-samples :refer [f]]
   [tieminos.utils :refer [ctl-synth]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(defn free-synth-panner-and-bus
  "Works assuming a flow where synth's out-bus goes to a unique panner for that bus.
  Also assumes that the synth has a `gate` and a release key"
  [synth synth-out-bus]
  (let [release-time (get-in synth [:args "release"])
        panner-release 5]
    (cond
      (not release-time) (timbre/warn "Can not free-synth-panner-and-bus")
      (not (o/node-active? synth)) nil ; do nothing
      :else (ref-rain
             :id (keyword (str "free-synth-panner-and-bus" (rand-int 10000)))
             :durs [release-time panner-release 2]
             :loop? false
             :on-event (on-event
                        (case index
                          0 (ctl-synth synth :gate 0)
                          1 (stop-panner! synth-out-bus)
                          2 (o/free-bus synth-out-bus)
                          (timbre/warn "free-synth-panner-and-bus should not be looping...")))))))

(defn before-dur-end
  [dur s-before-end f]
  (ref-rain
   :id (keyword (str "before-dur-end-" (rand-int 10000)))
   :durs [(- dur s-before-end) s-before-end]
   :on-event (on-event (when (= index 1) (f)))))

(defn rand-time-segments [dur dur-weights]
  (loop [durs []]
    (if (>= (apply + durs) dur)
      durs
      (recur (conj durs (weighted dur-weights))))))
