(ns tieminos.habitat.main
  (:require
   [overtone.core :as o]
   [tieminos.habitat.init :refer [init!]]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.osc :as habitat-osc :refer [args->map map-val]]
   [tieminos.habitat.panners :refer [panner panner-rate]]
   [tieminos.habitat.parts.amanecer :as amanecer]
   [tieminos.habitat.parts.dia :as dia]
   [tieminos.habitat.parts.noche :as noche]
   [tieminos.habitat.resonance-panner :as reso-pan]
   [tieminos.habitat.routing :refer [inputs preouts]]
   [time-time.dynacan.players.gen-poly :as gp]))

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
  (do (o/stop) (gp/stop))
  (init!)
  ;; for testing
  (def test-context (atom {:dur-s 300}))
  #_(amanecer/init-section-1 300 inputs @preouts)
  #_(amanecer/inicio-descomposicion inputs @preouts test-context)
  #_(amanecer/coro-de-la-manana-cantos-iniciales inputs @preouts @main-fx test-context)

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

  #_(o/stop)
  (hseq/sequencer
    ;; TODO move inputs, preouts, main'fx etc to context and pass them here as a context initializer
   (hseq/timestamps->dur-intervals
    [;; amanecer
       ;; TODO revisar timestamps vs texto sonoro
     [[0 0] (partial amanecer/init-section-1 inputs @preouts)]
     [#_[5 20] [0 5] (partial amanecer/sol-y-luminosidad inputs @preouts)]
     [#_[7 30] [0 10] (partial amanecer/intercambios-de-energia inputs @preouts)]
     [#_[9 0] [0 15] (partial amanecer/inicio-descomposicion inputs @preouts)]
     [#_[10 20] [0 20] (partial amanecer/descomposicion-hacia-la-tierra inputs @preouts)
      [[12 24] (partial amanecer/coro-de-la-manana-cantos-iniciales inputs @preouts)]]
     [[14 30] (partial amanecer/coro-de-la-manana-interacciones-cuanticas inputs @preouts)]
     [[17 0] (partial amanecer/coro-de-la-manana-distancia-de-la-escucha inputs @preouts)]
     [[20 25] (partial amanecer/solo-de-milo inputs @preouts)]
       ;; día
     [[22 48] (partial dia/dueto-con-polinizadores inputs @preouts)]
     [[28 19] (partial dia/escucha-de-aves inputs @preouts)]
       ;; noche
     [[39 56] (partial noche/de-la-montana-al-fuego inputs @preouts)]
     [[45 21] (partial noche/fuego inputs @preouts)]
     [[50 0] (partial noche/alejamiento-del-fuego inputs @preouts)]
     [[52 22] (partial noche/polinizadores-nocturnos inputs @preouts)]
     [[59 0] (partial noche/hacia-un-nuevo-universo inputs @preouts)]])))
