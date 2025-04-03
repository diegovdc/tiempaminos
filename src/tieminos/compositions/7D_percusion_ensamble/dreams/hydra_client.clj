(ns tieminos.compositions.7D-percusion-ensamble.dreams.hydra-client
  (:require
   [overtone.osc :as osc]))

(defonce osc-client (atom nil))

(defn init [lan-ip]
  (when-not @osc-client
    (reset! osc-client (osc/osc-client lan-ip 7777))
    @osc-client))

(comment
  (init "192.168.0.104")

  (osc/osc-send @osc-client "/test" 1 2 3)
  (osc/osc-send @osc-client "/fade-from-black")
  (osc/osc-send @osc-client "/fade-to-black"))
