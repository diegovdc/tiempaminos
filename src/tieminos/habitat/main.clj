(ns tieminos.habitat.main
  (:require
   [overtone.core :as o]
   [tieminos.habitat.init :refer [init!]]
   [tieminos.habitat.osc :as habitat-osc :refer [args->map map-val]]
   [tieminos.habitat.panners :refer [panner panner-rate]]
   [tieminos.habitat.parts.amanecer :refer [init-section-1]]
   [tieminos.habitat.resonance-panner :as reso-pan]
   [tieminos.habitat.routing :refer [inputs preouts]]))

(defn TEMPORARY-multichan-wrapper
  "use to control multiple inputs with a single TouchOSC slider or button"
  [f {:keys [in] :as args-map}]
  (let [arg-maps (case in
                   0 [args-map]
                   1 (mapv #(assoc args-map :in %)
                           [1 3])
                   2 (mapv #(assoc args-map :in %)
                           [2 4])
                   nil)]
    (doseq [am arg-maps]
      (f am))))

(comment
  (o/stop)
  (init!)
  (habitat-osc/responder
   (fn [{:keys [path args] :as msg}]
     (println path args)
     (let [args-map (args->map args)]
       (case path
         "/panner" (TEMPORARY-multichan-wrapper panner args-map)
         "/panner-rate" (TEMPORARY-multichan-wrapper panner-rate args-map)
         "/reso-pan-voices" (reso-pan/update-state :voices (map-val args-map 1 10))
         "/reso-pan-dur" (reso-pan/update-state :dur (map-val args-map 5 60))
         ;; TODO add reso-pan-amp
         "/reso-pan" (reso-pan/trigger (:in args-map) 0 5 10)
         "/rec" (rec/rec-controller args-map)
         (println "Unknown path for message: " msg)))))

  ;; amanecer
  (init-section-1 inputs @preouts))
