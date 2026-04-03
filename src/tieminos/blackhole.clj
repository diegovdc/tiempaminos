(ns tieminos.blackhole)

;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;

(defonce interface (atom :scarlett))

;;;;;;;;;;;;;;;;;;
;; IO
;;;;;;;;;;;;;;;;;;

(defn bus
  [i]
  (let [offset (case @interface
                 :minifuse 4
                 :scarlett 20)]
    (+ offset (dec i))))
