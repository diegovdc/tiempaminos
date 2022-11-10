(ns tieminos.scratch.buses
  (:require
   [clojure.walk :as walk]
   [overtone.core :as o]
   [tieminos.sc-utils.groups.v1 :as groups]))

(o/defsynth sini [out 0]
  (o/out out  (*
               0.5
               (o/env-gen (o/env-perc 10))
               (o/pan-az 4 (o/sin-osc 200) (o/lf-noise1 2)))))

(o/defsynth rev4ch [in-bus 0 out 0]
  (o/out out (o/free-verb (o/in:ar in-bus 4) 1 5)))

(comment
  (groups/init-groups!)
  (def main-rev-in-bus (o/audio-bus 4 "main-rev-in-bus"))
  (-> main-rev-in-bus)
  (rev4ch (groups/late) main-rev-in-bus)
  (sini (groups/early) main-rev-in-bus)
  (o/stop))
