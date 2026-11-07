(ns tieminos.osc-controllers.server
  "Taken from tunel-cuantico's code"
  (:require
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.network-utils :refer [get-local-host]]
   [tieminos.osc.core :refer [init-server]]))

(defonce osc-server (atom nil))

(defn init
  [& {:keys [port] :or {port 16180}}]
  (if-not @osc-server
    (reset! osc-server (:server (init-server port)))
    (timbre/warn "OSC Server is already running on:" (str (get-local-host) "@" port))))

(defn responder
  "The `::default` keyword is an identifier, so calling responder with different functions will overwrite the previous function.
  See usage example at the top of the file."
  [f]
  (if-not @osc-server
    (throw (ex-info "You must `init` the `osc-server` first" {}))
    (osc/osc-listen @osc-server f ::defaults)))
