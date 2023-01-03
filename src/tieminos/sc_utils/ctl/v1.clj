(ns tieminos.sc-utils.ctl.v1)

(comment
  ;; IDEA
  (with-ctls
    synth
    dur                                 ; for the control-opts
    uncontrolled-opts                   ; {}
    {:amp {:type :continuous ; other types :lorentz :discontinuous
           :vals [0 1 0.6 0.2 0]
           ;; optional, else equal divisions of dur
           :durs [1 2 3 2]}})
  ;; =>
  {:synth running-synth
   :controller-id #{:ctl-refrain/amp+some-id}})
