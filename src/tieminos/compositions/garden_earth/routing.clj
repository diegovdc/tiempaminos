(ns tieminos.compositions.garden-earth.routing
  (:require
   [clojure.core.async :as async]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [bh]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]))

;;;;;;;;;;;;;
;; Inputs
;;;;;;;;;;;;;

(oe/defsynth input
  [in 0 out 0 amp 1]
  (o/out out (* amp (o/sound-in in))))

(def ^:private ins {:in-1 {:in 22}})

(defonce inputs
  (atom ins))

(defn init-inputs! [{:keys [inputs config]}]
  (doseq [{:keys [bus synth]} (->> @inputs vals)]
    (when bus (o/free-bus bus))
    (when (and synth (o/node-active? synth))
      (try (o/kill synth)
           (catch Exception _
             (timbre/warn "Synth does not exist anymore")))))
  (->> ins
       ;; For some reason amp via o/sound-in is coming 8db lower than it should be
       ;; so allowing here for compensation.
       ;; FIXME find the cause for the above.
       (map (fn [[input-key {:keys [in]}]]

              (let [bus (o/audio-bus 1 (str (name input-key) "-input"))
                    amp (-> config input-key (:amp 1))]
                (println amp)
                {input-key {:in in
                            :bus bus
                            :synth (input {:group (groups/early)
                                           :in in
                                           :amp amp
                                           :out bus})}})))
       (into {})
       (reset! inputs)))

(defn fl-i1
  "input 1 on blackhole output 1 (i.e. sound-in 20).
  `k` - #{:in :bus :synth}"
  [k]
  (-> @inputs :in-1 k))

(comment
  (->> @inputs)
  (o/stop)
  #_(init-inputs! inputs)
  (o/demo (o/in (-> @inputs :in-1 :bus))))

;;;;;;;;;;;;;
;; Outs
;;;;;;;;;;;;;

(defonce outputs (atom {}))

(oe/defsynth output
  [in 0
   out 0]
  (o/out out (o/in in 2)))

(defn init-outputs!
  "Creates output synths that take their `in` via a bus.
  This way the output bus can be (for example) recorded."
  [{:keys [outputs config]}]
  (doseq [{:keys [bus synth]} (->> @outputs vals)]
    (when bus (o/free-bus bus))
    (when (and synth (o/node-active? synth))
      (try (o/kill synth)
           (catch Exception _
             (timbre/warn "Synth does not exist anymore")))))

  (reset! outputs {})
  (->> config
       (mapv
        (fn [[k {:keys [bh-out]}]]
          (let [out-bus (o/audio-bus 2 (str (name k) "-out"))
                bh-out* (bh bh-out)
                out-synth (output {#_#_:group (groups/post-fx)
                                   :in  out-bus
                                   :out bh-out*})]
            [k {:out bh-out*
                :bus out-bus
                :synth out-synth}])))
       (into {})
       (reset! outputs)))

(defn out [k]
  (let [{:keys [bus synth]} (k @outputs)]
    (when-not (o/node-active? synth)
      (timbre/warn (format "Output synth not active: `%s`. Call `ge.route/init-outputs!` first." k)))
    bus))

(comment
  (groups/init-groups!)
  (init-outputs!
   {:outputs outputs
    :config {:sc-1 {:bh-out 2}
             :sc-2 {:bh-out 4}}})

  (oe/defsynth testy
    [out 0]
    (o/out out (o/pan2 (* 0.5 (o/sin-osc 200)))))

  (oe/defsynth testy-ps*
    [in 0
     out 0]
    (o/out out (o/pitch-shift:ar (o/in in 1) 0.2 3)))

  (testy {:group (groups/early)
          :out (out :sc-1)})

  (testy-ps* {:group (groups/output-rec)
              :in (out :sc-1)})
  (o/demo (o/sin-osc))
  (o/stop))

;;;;;;;;;;;;;;;;;;
;; Control Buses
;;;;;;;;;;;;;;;;;;

(defonce controls (atom {}))

(defn init-control-buses!
  "Control buses may be used like this in a synth:
    (o/clip (o/in:kr amp-ctl 1) 0 1)
   Where `amp-ctl` is the control bus.
   The use of clip is recommended so that if no control bus
   is passed in, then the bus will not get some random high o low value."
  [{:keys [controls controls-config]}]
  (doseq [[_k {:keys [bus]}] @controls]
    (o/free-bus bus))
  (->> controls-config
       (mapv (fn [[k {:keys [chans]}]]
               (let [bus (o/control-bus chans (name k))]
                 [k {:bus bus}])))
       (into {})
       (reset! controls)))

(defn ctl-bus
  [control-bus-key]
  (-> @controls control-bus-key :bus))

(defn set-ctl
  "Set the value of a control bus"
  [control-bus-key value]
  (when-let [bus (ctl-bus control-bus-key)]
    (o/control-bus-set! bus (/ value 127))))

(defn ctl-range
  [ctl-bus min* max*]
  (-> ctl-bus
      (o/in:kr 1)
      (o/lin-lin:kr 0 1 min* max*)
      (o/clip min* max*)))

(comment
  (def default-ctl-1 (let [bus (o/control-bus 1 "default-ctl-bus")]
                       (o/control-bus-set! bus 0.5)
                       bus))
  (-> default-ctl-1 (into {}))
  (init-control-buses!
    {:exp/pedal-1 {:chans 1}})

  (set-ctl :exp/pedal-1 127)

  (oe/defsynth sini
    [freq 200
     amp 0.5
     amp-ctl 0
     amp-ctl-max 6
     out 0]
    ;; NOTE the use of clip to prevent high amps if no
    ;; ctl is passed in.
    (o/out out (* amp (ctl-range amp-ctl 0 amp-ctl-max)
                  (o/pan2 (o/sin-osc 200)))))

  (def test-sini (sini
                   {:amp-ctl (ctl-bus :exp/pedal-1)}))
  (o/kill test-sini)
  (o/stop)

  (oe/defsynth sini2
    [freq 200
     amp 0.5
     out 0]
    (o/out out (* amp (o/pan2 (o/sin-osc (o/clip (o/lin-lin:kr 4000 100 400 100 200)
                                                 100 200))))))

  (def test-sini2 (sini2))
  (o/kill test-sini2))
