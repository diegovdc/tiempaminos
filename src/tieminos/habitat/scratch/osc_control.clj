(ns tieminos.habitat.scratch.osc-control
  (:require
   [overtone.core :as o]))

(comment
  ;; pseudocode

  ;; :nc/xyz means don't add a bus for this
  (def osc-mappings (atom {:guitar/rev {:room 1 :lag/room 2 :mix 0.5 :lag/mix 1}}))
  (defn init-mappings [osc-mappings]
    (swap! osc-mappings
           #(->> %
                 (map (fn  [[k config]]
                        ;; make busses with config
                        (let [room-bus (o/control-bus)
                              mix-bus (o/control-bus)
                              lag-room 2
                              lag-mix 1]
                          [k (assoc config
                                    :bus/room room-bus
                                    :bus/mix mix-bus
                                    :synth ((o/synth [;; mappings keys e.g.:
                                                      room 1
                                                      mix 0.5]
                                                     (o/out room-bus (o/lag:kr room lag-room))
                                                     (o/out mix-bus (o/lag:kr mix lag-mix)))))])))
                 (into {}))))
  (def initialized-mappings (init-mappings osc-mappings))
  ;; =>
  #_{:guitar/rev {:room 1 :lag/room 2 :bus/room room-bus
                  :mix 0.5 :lag/mix 1 :bus/mix mix-bus
                  :synth guitar-rev-ctl-synth}}

  (defn get-mapping
    [x])
  (defn ctl [synth m])
  (defn on-change [k v]
    (ctl (:synth (get-mapping k)) v))

  (defn get-bus [k bus])

;; on osc-responder
  (on-change :guitar/rev {:room 2})

  ;; usage
  (o/defsynth my-guitar-synth
    [in 0
     mix (get-bus :guitar/rev :bus/mix)
     room (get-bus :guitar/rev :bus/room)
     out 0]
    (o/out out (-> (o/in in 1)
                   (o/free-verb mix room)))))
