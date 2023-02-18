(ns tieminos.habitat.synths.main-fx
  (:require
   [overtone.core :as o]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.routing :refer [reaper-returns]]
   [tieminos.overtone-extensions :refer [defsynth]]))

(declare heavy-reverb)

(def main-fx (atom nil))

(defn init-main-fx!
  []
  (reset! main-fx
          {:heavy-reverb (let [bus (o/audio-bus 4 "heavy-reverb-bus")]
                           {:group (groups/fx)
                            :bus bus
                            :synth (heavy-reverb {:in bus})})}))

(defsynth heavy-reverb
  [in 0
   out (reaper-returns 2)
   mix 0.7
   room 2
   damp 0.6]
  (o/out out
         (o/free-verb (o/in in 4) mix room damp)))
