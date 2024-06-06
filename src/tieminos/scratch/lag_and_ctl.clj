(ns tieminos.scratch.lag-and-ctl
  (:require
   [overtone.core :as o]
   [tieminos.sc-utils.groups.v1 :as groups]))

(comment

  (o/defsynth laggy-sine
    [freq 200]
    (o/out 0 (* 0.2 (o/sin-osc (o/lag:kr freq 10)))))

  (def ls (laggy-sine (groups/early)))
  (def ls2 (laggy-sine (groups/early) :freq 800))
  (o/ctl (:early @groups/groups)  :freq 100)

  (groups/init-groups!)
  (o/stop))
