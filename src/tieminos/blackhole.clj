(ns tieminos.blackhole
  (:require
   [taoensso.timbre :as timbre]))

;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;

(defonce ^:private interface (atom :scarlett))

(defn set-interface!
  [k]
  (if (#{:scarlett :minifuse} k)
    (do (reset! interface k)
        (timbre/info "Using interface:" k))
    (throw (ex-info "Unknown interface" {:key k}))))

;;;;;;;;;;;;;;;;;;
;; IO
;;;;;;;;;;;;;;;;;;

(defn bus
  [i]
  (when (zero? i)
    (timbre/warn "Blackhole `bus` starts at `1` so that it makes sense when getting the bus in REAPER"))
  (let [offset (case @interface
                 :minifuse 4
                 :scarlett 20)]
    (+ offset (dec i))))


