(ns tieminos.habitat.main
  (:require
   [overtone.core :as o]
   [tieminos.habitat.init :refer [init!]]
   [tieminos.habitat.osc :as habitat-osc :refer [args->map map-val]]
   [tieminos.habitat.panners :refer [panner panner-rate rand-pan4]]
   [tieminos.habitat.resonance-panner :as reso-pan]
   [tieminos.habitat.routing :refer [guitar-bus reaper-returns]]
   [tieminos.overtone-extensions :refer [defsynth]]
   [tieminos.sc-utils.groups.v1 :as groups]))

(defsynth input
  [in 0 out 0]
  (o/out out (o/sound-in in)))

(comment
  (o/stop)
  (init!)
  (habitat-osc/responder
   (fn [{:keys [path args] :as msg}]
     (println path args)
     (let [args-map (args->map args)]
       (case path
         "/panner" (panner args-map)
         "/panner-rate" (panner-rate args-map)
         "/reso-pan-voices" (reso-pan/update-state :voices (map-val args-map 1 10))
         "/reso-pan-dur" (reso-pan/update-state :dur (map-val args-map 5 60))
          ;; TODO add reso-pan-amp
         "/reso-pan" (reso-pan/trigger (:in args-map) 0 5 10)
         (println "Unknown path for message: " msg))))))

(comment
  (rand-pan4 {:group (groups/late)
              :in guitar-bus
              :out (reaper-returns 1)}))
