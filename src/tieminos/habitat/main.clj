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
   [tieminos.habitat.synths.main-fx :refer [main-fx]]
   [tieminos.habitat.utils :refer [free-synth-panner-and-bus]]
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
  (def test-context (atom {:dur-s 300
                           :inputs inputs
                           :preouts preouts
                           :main-fx main-fx}))
  (-> @test-context)
  #_(amanecer/intercambios-de-energia test-context)
  #_(amanecer/init-section-1 test-context)
  #_(def idc (amanecer/inicio-descomposicion test-context))

  #_(do
      (swap! test-context assoc :dur-s 10)
      (def dht (amanecer/descomposicion-hacia-la-tierra test-context)))
  #_(->> idc
         :amanecer/inicio-descomposicion
         :convolver-synths

         (#(doseq [{:keys [synth out-bus]} %]

             (free-synth-panner-and-bus synth out-bus))))
  #_(amanecer/coro-de-la-manana-cantos-iniciales inputs @preouts @main-fx test-context)

  (hseq/sequencer
   {:inputs inputs
    :preouts preouts
    :main-fx main-fx}
   (hseq/timestamps->dur-intervals
    [;; amanecer
       ;; TODO revisar timestamps vs texto sonoro
     [[0 0] amanecer/init-section-1]
     [#_[5 20] [0 5] amanecer/sol-y-luminosidad]
     [#_[7 30] [0 10] amanecer/intercambios-de-energia]
     [#_[9 0] [1 15] amanecer/inicio-descomposicion]
     [#_[10 20] [0 20] amanecer/descomposicion-hacia-la-tierra]
     [[12 24] amanecer/coro-de-la-manana-cantos-iniciales]
     [[14 30] amanecer/coro-de-la-manana-interacciones-cuanticas]
     [[17 0] amanecer/coro-de-la-manana-distancia-de-la-escucha]
     [[20 25] amanecer/solo-de-milo]
       ;; día
     [[22 48] (partial dia/dueto-con-polinizadores inputs @preouts)]
     [[28 19] (partial dia/escucha-de-aves inputs @preouts)]
       ;; noche
     [[39 56] (partial noche/de-la-montana-al-fuego inputs @preouts)]
     [[45 21] (partial noche/fuego inputs @preouts)]
     [[50 0] (partial noche/alejamiento-del-fuego inputs @preouts)]
     [[52 22] (partial noche/polinizadores-nocturnos inputs @preouts)]
     [[59 0] (partial noche/hacia-un-nuevo-universo inputs @preouts)]]))

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
         (println "Unknown path for message: " msg))))))

