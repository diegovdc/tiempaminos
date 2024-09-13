(ns tieminos.compositions.garden-earth.routing
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]))

(oe/defsynth input
  [in 0 out 0]
  (o/out out (o/sound-in in)))

(def ^:private ins {:in-1 {:in 22}})

(defonce inputs
  (atom ins))

(defn init-inputs! [inputs]
  (doseq [{:keys [synth]} (->> @inputs vals)]
    (when (and synth (o/node-active? synth))
      (try (o/kill synth)
           (catch Exception _
             (timbre/warn "Synth does not exist anymore")))))
  (->> ins
       (map (fn [[input-key {:keys [in]}]]
              (let [bus  (o/audio-bus 1 (str (name input-key) "-input") )]
                {input-key {:in in
                            :bus bus
                            :synth (input {:group (groups/early)
                                           :in in
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
  (init-inputs! inputs)
  (o/demo (o/in (-> @inputs :in-1 :bus))))
