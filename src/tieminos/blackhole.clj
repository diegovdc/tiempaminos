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

(defn make-aggregate
  "When using an aggregate device with multiple blackhole instances, return a `bus`-like function that will take as first argument the device and as second it's index."
  [& bh-chan-sizes]
  (let [bus-fns (->> bh-chan-sizes
                     (reduce (fn [{:keys [offset bus-fns-map] :as acc}
                                  ch-size]
                               (assoc acc
                                      :offset (+ offset ch-size)
                                      :bus-fns-map (assoc bus-fns-map (keyword (str ch-size)) (comp bus #(+ offset %)))))
                             {:offset 0
                              :bus-fns-map {}})
                     :bus-fns-map)]
    (fn [bh-key i]
      (if-let [bus* (bus-fns bh-key)]
        (bus* i)
        (throw (ex-info "Invalid blackhole instance key" {:key bh-key
                                                          :available-keys (keys bus-fns)}))))))

(comment
  (keyword (str 16))
  (def bus* (make-aggregate 16 128))
  (bus* :16 1)
  (bus* :128 1))
