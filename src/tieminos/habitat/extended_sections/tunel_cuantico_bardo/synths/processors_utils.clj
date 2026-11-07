(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.processors-utils
  (:require
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.config :as bardo.config]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.utils :refer [outs]]
   [tieminos.habitat.routing :refer [get-input-bus]]))

(defn input->in&outs&group [input]
  (case input
    :guitar-clean (-> {:in (get-input-bus :guitar)}
                      (outs {:out-offset (bardo.config/get-bh-bus :guitar-clean)}))))
