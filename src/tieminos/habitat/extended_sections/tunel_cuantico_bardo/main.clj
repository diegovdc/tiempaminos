(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.main
  "The code from the recorded versions of `2.3.x`, `2.2.9.x`"
  (:require
   [clojure.string :as str]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.init :as bardo.init]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc :as bardo.osc]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.scratch.main]))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;* main initialization section
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; init OSC communication
  (bardo.osc/init!
    ;; NOTE if a client is missing there will be a "Host is Down" error.
   [["127.0.0.1" 16181]      ;; local
    #_["192.168.0.101" 16180] ;; diego
    #_["192.168.0.104" 16180] ;; milo
    ])
  (reset-default-state!)
  ;; init everything (habitat and input synths, bardo.comms) except SC, REAPER and OSC communications
  (bardo.init/all!)
  (bardo.osc/post-live-state-to-ui!)
  (bardo.osc/post-live-state-to-ui! :print-instead? true))

(defn reset-default-state!
  []
  (bardo.live-state/init-state!)
  ;; TODO: perhaps here the default-touch-osc state is duplicated and the state of the :selected-synth should be used instead?
  (doseq [[path args] bardo.live-state/default-touch-osc-state]
    (when-not (or (some #(str/ends-with? path %) ["-visible" "-label" "-group"])
                  (str/includes? path "toggle-bank"))
      (bardo.osc/osc-responder {:path path :args args})
      ;; if a some buttons get activated, deactivate them
      (when (#{"/gusano/harmonic-seq-up-btn"
               "/gusano/harmony-up-btn"}
             path)
        (Thread/sleep 100)
        (bardo.osc/osc-responder {:path path :args [(float 0)]}))))
  (timbre/info "State has been reset"))


