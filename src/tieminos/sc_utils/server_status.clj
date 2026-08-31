(ns tieminos.sc-utils.server-status
  "Query the server data (e.g. cpu usage)"
  (:require
   [clojure.core.async :as a]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]))

(defonce running? (atom false))

(defn start-server-status-query-loop!
  [freq-ms f]
  (if @running?
    (timbre/warn "SC server status query loop already running.")
    (do
      (timbre/info "Starting SC server status query loop")
      (reset! running? true)
      (a/go-loop
       []
        (a/<! (a/timeout freq-ms))
        (try
          (f (o/server-status))
          (catch Exception e (timbre/error "Error in SC server status query loop" e)))
        (when @running?
          (recur))))))

(defn stop-server-status-query-loop!
  []
  (reset! running? false)
  (timbre/info "Stopping SC server status query loop"))

(comment
  (o/server-status)
  (start-server-status-query-loop! 1000 println)
  (stop-server-status-query-loop!))
