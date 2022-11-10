(ns tieminos.habitat.init
  (:require
   [overtone.core :as o]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.habitat.routing :refer [guitar-bus ins]]
   [tieminos.overtone-extensions :refer [defsynth]]
   [tieminos.sc-utils.groups.v1 :as groups]))

(defsynth input
  [in 0 out 0]
  (o/out out (o/sound-in in)))

(def inputs
  [{:in (:guitar ins)
    :bus guitar-bus}])

(defn init! []
  (habitat-osc/init)
  (groups/init-groups!)
  (doseq [{:keys [in bus]} inputs]
    (input {:group (groups/early)
            :in in
            :out bus})))

