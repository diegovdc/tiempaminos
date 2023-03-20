(ns tieminos.habitat.synths.main-fx
  (:require
   [overtone.core :as o]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.routing :refer [reaper-returns]]
   [tieminos.overtone-extensions :refer [defsynth]]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]))

(declare heavy-reverb mid-reverb osc-reverb light-reverb)

(def main-fx (atom nil))

(defn init-main-fx!
  []
  (reset! main-fx
          {:heavy-reverb (let [bus (o/audio-bus 4 "heavy-reverb-bus")]
                           {:group (groups/fx)
                            :bus bus
                            :synth (heavy-reverb {:in bus})})
           :light-reverb (let [bus (o/audio-bus 4 "light-reverb-bus")]
                           {:group (groups/fx)
                            :bus bus
                            :synth (light-reverb {:in bus})})
           :mid-reverb (let [bus (o/audio-bus 4 "mid-reverb-bus")]
                         {:group (groups/fx)
                          :bus bus
                          :synth (mid-reverb {:in bus})})
           :osc-reverb (let [bus (o/audio-bus 4 "osc-reverb-bus")]
                         {:group (groups/fx)
                          :bus bus
                          :synth (osc-reverb {:in bus})})}))

(defsynth heavy-reverb
  [in 0
   out (reaper-returns 2)
   mix 0.7
   room 2
   damp 0.6]
  (o/out out
         (o/free-verb (o/in in 4) mix room damp)))

(defsynth mid-reverb
  [in 0
   out (reaper-returns 3)
   mix 0.7
   room 1.2
   damp 0.6
   amp 1]
  (o/out out
         (* amp (o/free-verb (o/in in 4) mix room damp))))

(defsynth light-reverb
  [in 0
   out (reaper-returns 3)
   mix 0.5
   room 0.7
   damp 0.3
   amp 1]
  (o/out out
         (* amp (o/free-verb (o/in in 4) mix room damp))))

(defsynth osc-reverb
  [in 0
   out (reaper-returns 3)
   min-mix 0.1
   max-mix 1
   min-room 0.3
   max-room 1
   min-damp 0
   max-damp 1
   amp 1]
  (o/out out
         (* amp (o/free-verb (o/in in 4)
                             (lfo-kr (lfo-kr 0.3 1 4) min-mix max-mix)
                             (lfo-kr (lfo-kr 0.3 1 4) min-room max-room)
                             (lfo-kr (lfo-kr 0.3 1 4) min-damp max-damp)))))

(comment
  (-> @main-fx :heavy-reverb
      :synth
      (o/ctl :mix 1)))
