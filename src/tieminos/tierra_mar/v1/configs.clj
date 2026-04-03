(ns tieminos.tierra-mar.v1.configs
  (:require
   [clojure.pprint :as pprint]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.blackhole :as bh]))

;;;;;;;;;;;;;;;;;;
;; IO
;;;;;;;;;;;;;;;;;;

(def ins
  {:fl-main (bh/bh 3)})

;; First 16 bh buses are reserved as inputs
(def outs
  {:nubosidades-fl-2ch (bh/bh 16)
   :nubosidades-fl2-2ch (bh/bh 18)})

(defn get-input [k]
  (if-let [bus (ins k)]
    bus
    (throw (ex-info "In bus not found" {:key k}))))

(defn get-output [k]
  (if-let [bus (outs k)]
    bus
    (throw (ex-info "Out bus not found" {:key k}))))

;;;;;;;;;;;;;;;;;;
;; IEM
;;;;;;;;;;;;;;;;;;

(def iem-osc-ports
  {:nubosidad-lorenztiana-fl 1234
   :nubosidad-lorenztiana-fl2 1235})

(defonce iem-osc-clients
  (atom nil))

(defn init-osc-clients!
  []
  (doseq [[k {:keys [client]}] @iem-osc-clients]
    (timbre/info "Closing to reinit:" k)
    (osc/osc-close client))
  (reset! iem-osc-clients
          (->> iem-osc-ports
               (map (fn [[k port]]
                      [k {:client (osc/osc-client "localhost" port)
                          :port port}]))
               (into {}))))

(defn get-iem-osc-client
  [k]
  (if-let [c (-> @iem-osc-clients k :client)]
    c
    (throw (ex-info "IEM OSC client not found. Maybe call `tieminos.tierra-mar.v1.configs/init-osc-clients!` first"
                    {:key k}))))

(comment
  (get-iem-osc-client :nubosidad-lorenztiana-fl)
  (init-osc-clients!))
