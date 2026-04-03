(ns tieminos.tierra-mar.v1.configs
  (:require
   [overtone.core :as o]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.blackhole :as bh]))

;;;;;;;;;;;;;;;;;;
;; IO
;;;;;;;;;;;;;;;;;;

(def ins
  {:fl-main (bh/bus 3)})

;; First 16 bh buses are reserved as inputs
(def outs
  {:nubosidades-fl-2ch (bh/bus 16)
   :nubosidades-fl2-2ch (bh/bus 18)
   :nubosidades-arp-2ch (bh/bus 20)
   :nubosidades-arp2-2ch (bh/bus 22)})

(defn get-input [k]
  (if-let [bus (ins k)]
    bus
    (throw (ex-info "In bus not found" {:key k}))))

(defn
  get-output [k]
  (if-let [bus (outs k)]
    bus
    (throw (ex-info "Out bus not found" {:key k}))))

;; IO Buses
;;;;;;;;;

(defonce audio-buses (atom {}))

(defn init-buses!
  []
  (reset! audio-buses
          (reduce-kv
           (fn [m k v] (assoc m k (o/audio-bus v (name k))))
           {}
           {:arp->nubosidad 2
            :arp->nubosidad2 2})))

(defn get-audio-bus
  [k]
  (if-let [bus (get @audio-buses k)]
    bus
    (timbre/error "Audio Bus not found" {:key k})))

(comment
  (init-buses!))

;;;;;;;;;;;;;;;;;;
;; IEM
;;;;;;;;;;;;;;;;;;

(def iem-osc-ports
  {:nubosidad-lorenztiana-fl 1234
   :nubosidad-lorenztiana-fl2 1235
   :nubosidad-lorenztiana-arp 1236
   :nubosidad-lorenztiana-arp2 1237})

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
