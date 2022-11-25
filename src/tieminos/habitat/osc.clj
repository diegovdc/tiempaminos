(ns tieminos.habitat.osc
  (:require
   [clojure.edn :as edn]
   [clojure.math :refer [round]]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.network-utils :refer [get-local-host]]
   [tieminos.osc.core :refer [init-server]])
  (:import
   (java.net InetAddress)))

(declare init responder)
(comment
  ;; USAGE
  (init)
  (responder
   (fn [{:keys [path args] :as msg}]
     (println "msg")
     (case path
       "/hola" (apply println "dicen hola" args)
       "/adios" (println "dicen adios" args)
       (println "Unknown path for message: " msg))))

  ;; TEST
  (osc/osc-debug true)
  (osc/osc-debug false)
  (def client (osc/osc-client (get-local-host) 16180))
  (def client2 (osc/osc-client (get-local-host) 16180))
  (osc/osc-send client "/hola" 1)
  (osc/osc-send client2 "/adios" 3 5 6))

(defonce osc-server (atom nil))

(defn init
  [& {:keys [port] :or {port 16180}}]
  (if-not @osc-server
    (reset! osc-server (:server (init-server port)))
    (timbre/warn "OSC Server is already running.")))

(defn responder
  "The `::default` keyword is an identifier, so calling responder with different functions will overwrite the previous function.
  See usage example at the top of the file."
  [f]
  (osc/osc-listen @osc-server f ::defaults))

(defn args->map [args]
  (try
    (->> args (partition 2 2)
         (map (fn [[k v]]
                {(keyword k) (cond (and (= "in" k) (string? v)) (edn/read-string v)
                                   (string? v) (keyword v)
                                   :else v)}))
         (apply merge))
    (catch Exception _
      (throw (ex-info "Could not convert args to map"
                      {:args args})))))

(defn map-val
  "Maps a `value-key` from an `args-map` between `min*` and `max*`,
  assuming the input `value-key` refers to a number between `0` and `1`
  The `value-key` defaults to `:value`"
  ([args-map min* max*] (map-val :value args-map min* max*))
  ([value-key args-map min* max*]
   (update args-map
           value-key #(->> %
                           (* max*)
                           round
                           (max min*)
                           ;; ensure it doesn't go beyond stated max, if value is > 1
                           (min max*)))))

(comment
  (init :port 9129)
  (reset! osc-server nil)
  (-> @osc-server)
  (def client (osc/osc-client (get-local-host) 2666))
  (osc/osc-send client "/holas" 1,2,3,4))
