(ns tieminos.sc-utils.inputs
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]))

(oe/defsynth input
  [in 0 out 0]
  (o/out out (o/sound-in in)))

(oe/defsynth direct-out ;; use for testing
  [bus 0
   out 0]
  (o/out out (o/in bus)))

(defonce inputs (atom {}))

(defn- get-early-group!
  []
  (timbre/info "Initializing default `early` group for input. Pass a group to init input to avoid this call.")
  (let [groups @groups/groups]
    (when-not groups (groups/init-groups!))
    (groups/early)))

(defn init-input
  [{:keys [id in n-chans group] :as args
    :or {n-chans 1}}]

  (when-not (keyword? id) (throw (ex-info "id should be a keyword" args)))

  (let [{:keys [synth bus]} (@inputs id)
        group* (or group (get-early-group!))
        bus (or bus (o/audio-bus n-chans (name id))) ;; NOTE: prevent the reinitialization of buses
        synth* (input {:group group*
                       :in in
                       :out bus})
        input-data {:synth synth* :bus bus :group group*}]

    (when (and synth (o/node-active? synth))
      (o/kill synth))

    (swap! inputs assoc id input-data)

    input-data))

(defn get-input [id]
  (@inputs id))

(defn get-bus [id]
  (-> id get-input :bus))

(comment
  (o/stop)
  (input {:group (groups/early)
          :in 0})

  (init-input {:id :guitar
               :group (groups/early)
               :in 0})

  (direct-out {:group (groups/late)
               :bus (get-bus :guitar)}))

