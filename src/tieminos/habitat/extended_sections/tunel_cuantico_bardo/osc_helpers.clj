(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-helpers
  (:require
   [overtone.osc :as osc]
   [tieminos.habitat.osc :as habitat-osc]))

(def ^:private excluded-paths #{"/presets/load"})

(defn update-clients
  [clients path args]
  (doseq [client (map second clients)]
    (when-not (excluded-paths path)
      (apply osc/osc-send client path args))))

(defn get-label-path
  [player label-key]
  (case [player label-key]
    ;; other cases to come
    (format "/%s/%s"
            (case player :milo "Milo" :diego "Diego" :gusano "gusano")
            (str (name label-key) "-label"))))
(comment
  (update-label :milo "harmonic-lowest-note" 100))

(defn update-label
  "Expects `label-key` to be provided as `:my-label` when in touchosc it is defined as `/player/my-label-label` (note the `-label` suffix)."
  [player label-key value]
  (let [path (get-label-path player label-key)]
    (update-clients @habitat-osc/receiver-clients path [(str value)])
    {:path path :value [(str value)]}))

(defn send-osc-msg
  [path & values]
  (update-clients @habitat-osc/receiver-clients path values)
  {:path path :value values})

(defn send-osc-msg-to-self
  [path & values]
  (update-clients {:internal @habitat-osc/internal-client} path values)
  {:path path :value values})
