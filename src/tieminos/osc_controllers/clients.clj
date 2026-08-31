(ns tieminos.osc-controllers.clients
  "Taken from tunel-cuantico's code"
  (:require
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.network-utils :refer [get-local-host]]
   [tieminos.osc-controllers.server :refer [osc-server]]))

(defonce reaper-client (atom nil))

(comment
  (reset! reaper-client nil))

(defn make-reaper-osc-client
  []
  (if @reaper-client
    @reaper-client
    (reset! reaper-client (osc/osc-client
                           #_(get-local-host) ;; for some reason this is not working
                           #_"0.0.0.0" ;; and this doesn't work anymore (after OS update?)
                           "127.0.0.1" ;; this one seems to work for now
                           65432))))

(defonce internal-client (atom nil))

(defn make-internal-osc-client
  [port]
  (if @internal-client
    @internal-client
    (reset! internal-client (osc/osc-client
                             (get-local-host)
                             port))))

(defonce receiver-clients (atom {}))

(defn make-receiver-clients
  "`clients` is a vector of [host port]"
  [clients]
  ;; Close existing clients
  (doseq [client (vals @receiver-clients)]
    (osc/osc-close client))

  (reset! receiver-clients {})

  (doseq [client clients]
    (when-not (@receiver-clients client)
      (let [[host port] client]
        (if (and (#{(get-local-host) "127.0.0.1"} host)
                 (= port (-> @osc-server :port deref)))
          (timbre/warn "Skipping: OSC receiver client cannot be the osc-server:"
                       (format "%s@%s" host port))
          (swap! receiver-clients assoc client (osc/osc-client host port)))))))

(defn update-clients
  [clients path args]
  (doseq [client (map second clients)]
    #_(when-not (excluded-paths path))
    (apply osc/osc-send client path args)))

(defn send-osc-msg
  [path & values]
  (update-clients @receiver-clients path values)
  {:path path :value values})
