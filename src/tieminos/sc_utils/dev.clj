(ns tieminos.sc-utils.dev
  "Utils for developing synths"
  (:require
   [overtone.core :as o]))

(defonce test-synths (atom {}))

(defn get-synth
  [id]
  (if-let [sy (@test-synths id)]
    sy
    (throw (ex-info "No synth for id" {:id id}))))

(defn stop
  "Stop a synth, if it has a gate then use that, else kill it."
  [id]
  (let [synth (get @test-synths id)
        gate? (get-in synth [:args "gate"])]
    (when (o/node-active? synth)
      (if gate?
        (o/ctl synth :gate 0)
        (o/kill synth)))
    (swap! test-synths dissoc id)))

(defn track
  "Keeps track of a running synth using an id, if a new synth is called with that id, then stop the previous synth."
  [id synth]
  (stop id)
  (swap! test-synths assoc id synth))

(defn stop-all
  []
  (doseq [[id _synth] @test-synths]
    (stop id)))
