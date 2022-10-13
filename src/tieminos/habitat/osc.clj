(ns tieminos.habitat.osc
  (:require
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.network-utils :refer [get-local-host]])
  (:import
   (java.net InetAddress)))
(.getHostAddress (java.net.InetAddress/getLocalHost))

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
  (if-no    t @osc-server
    (reset! osc-server (osc/osc-server port))
    (timbre/warn "OSC Server is already running.")))

(defn responder
  "The `::default` keyword is an identifier, so calling responder with different functions will overwrite the previous function.
  See usage example at the top of the file."
  [f]
  (osc/osc-listen @osc-server f ::defaults))

(comment
  (init :port 9129)
  (reset! osc-server nil)
  (-> @osc-server)
  (def client (osc/osc-client (get-local-host) 2666))
  (osc/osc-send client "/holas" 1,2,3,4)
  )
