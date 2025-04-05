(ns tieminos.compositions.7D-percusion-ensamble.dreams.hydra-client
  (:require
   [overtone.osc :as osc]))

(defonce osc-client (atom nil))

(defn- init [lan-ip]
  (when-not @osc-client
    (reset! osc-client (osc/osc-client lan-ip 7777))
    @osc-client))

(defn moment-1 []
  (osc/osc-send @osc-client "/moment-1"))

(defn play-main []
  (osc/osc-send @osc-client "/play-main"))

(defn end []
  (osc/osc-send @osc-client "/end"))

(defn to-color []
  (osc/osc-send @osc-client "/to-color"))

(defn to-gray []
  (osc/osc-send @osc-client "/to-gray"))

(defn fade-to-black []
  (osc/osc-send @osc-client "/fade-to-black"))

(defn fade-from-black []
  (osc/osc-send @osc-client "/fade-from-black"))

(defn to-init []
  (osc/osc-send @osc-client "/to-init"))

(comment
  (moment-1)
  (play-main)
  (end)
  (fade-from-black)
  (fade-to-black)
  (to-gray)
  (to-color)
  (to-init))

(comment
  (init "192.168.0.104")

  (osc/osc-send @osc-client "/test" 1 2 3)
  (osc/osc-send @osc-client "/fade-from-black")
  (osc/osc-send @osc-client "/fade-to-black"))
