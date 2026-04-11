(ns tieminos.tierra-mar.v1.olivo
  (:require
   [clojure.data.generators :refer [weighted]]
   [tieminos.sc-utils.groups.v1 :as sc.groups]
   [tieminos.tierra-mar.v1.arp :as tm.arp]
   [tieminos.tierra-mar.v1.configs :as tm.configs]
   [tieminos.tierra-mar.v1.state :as tm.state]
   [time-time.standard :refer [rrand]]))

;; * Olivo
;; ** fase 1
;; - arpa-flauta: fluido vital en espiral ascendente
;; - gong - ramas-hojas: estiramientos en el espacio ambisónico (az-widths)
;; - guitarra
;; ** fase 2
;; flauta desparaece: mucho reverb se convierte en lluvia
;; lo demás continúa
;;
;;
(comment
  (init!)
  (stop!))

;;;;;;;;;;;;;;;;;;
;; Arp
;;;;;;;;;;;;;;;;;;

(defn start-arp!
  []
  (tm.state/set-arp-pattern! tm.state/state {:name "stateful-rise"
                                             :fn (partial tm.arp/stateful-rise
                                                          {:id ::fase-a
                                                           :min-len 4
                                                           :max-len 8
                                                           :min-deg -18
                                                           :max-deg 12
                                                           :intervals [1 2 3]})})
  (tm.arp/start-sample-arp! {:group (sc.groups/early)
                             :out-fn (fn [_i]
                                       (tm.configs/get-audio-bus
                                        (rand-nth [:arp->nubosidad
                                                   :arp->nubosidad2])))}))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copa del Olivo
;; (gong branched paths)
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn  get-node-val
  [weights min* max* prev-val]
  (loop []
    (let [x (+ prev-val (weighted weights))]
      (if (< (dec min*) x (inc max*))
        x
        (recur)))))
#_(get-node-val {-10 3 10 1 1 4 -1 2} 1 40 35)

(defn make-branch-path
  "For traversing ring paths.
  +-7 goes one level up, and +-1 moves sideways"
  [len]
  (let [weigths {-7 (rand-nth [3 2 4])
                 7 (rand-nth [1 2])
                 1 (rand-nth [2 4 1 5])
                 -1 (rand-nth [2 4 1 5])}]
    (->> (range len)
         (reduce (fn [acc _]
                   (conj acc
                         (get-node-val weigths 1 28 (last acc))))
                 [(rrand 1 29)]))))
(make-branch-path 10)

;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;

(defn init! []

  (start-arp!))

(defn stop!
  [])
