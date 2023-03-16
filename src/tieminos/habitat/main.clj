(ns tieminos.habitat.main
  (:require
   [clojure.string :as str]
   [overtone.core :as o]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.osc :as habitat-osc :refer [args->map map-val]]
   [tieminos.habitat.panners :refer [current-panners panner panner-rate]]
   [tieminos.habitat.parts.amanecer :as amanecer]
   [tieminos.habitat.parts.dia-back :as dia]
   [tieminos.habitat.parts.noche :as noche]
   [tieminos.habitat.resonance-panner :as reso-pan]
   [tieminos.habitat.routing
    :refer [inputs preouts reaper-returns special-inputs]]
   [tieminos.habitat.synths.main-fx :refer [main-fx]]
   [tieminos.osc.reaper :as reaper]
   [time-time.dynacan.players.gen-poly :as gp]
   [taoensso.timbre :as timbre]))

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

(def sections
  [;; amanecer
     ;; TODO revisar timestamps vs texto sonoro
   [[0 0] amanecer/humedad]
   [[5 20] #_[0 5] amanecer/sol-y-luminosidad]
     ;;  bajarle a la conv
   [[7 30] #_[0 10] amanecer/intercambios-de-energia]
   [[9 0] #_[0 15] amanecer/inicio-descomposicion]
   [[10 20] #_[0 20] amanecer/descomposicion-hacia-la-tierra]
     ;; TODO cascabeles giratorios
   [[12 24] #_[0 25]  amanecer/coro-de-la-manana-cantos-iniciales]
   [[14 30] #_[0 30] amanecer/coro-de-la-manana-interacciones-cuanticas]
   [[17 0] #_[0 35] amanecer/coro-de-la-manana-distancia-de-la-escucha]
     ;; TODO revisar continuidad de los paneos
   [[20 25] #_[0 45] amanecer/solo-de-milo]
     ;; día
     ;; mover a 23:14
   [[22 48] #_[0 50] dia/dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía]
   [[24 30] #_[0 55] dia/dueto-con-polinizadores=pt2-percepción-de-señal-danza-desarrollo-de-energía]
   [[25 47] #_[1 0] dia/dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación]
   [[27 51] #_[1 5] dia/dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales]
     ;; NOTE cambio en marca de tiempo respecto de la partitura
   [[28 30] dia/dueto-con-polinizadores=pt5-movimiento-energía-alejamiento->viento]
   #_[[29 55] tacet]
   [[31 11] dia/escucha-de-aves]
     ;; noche
   #_[[39 56] (partial noche/de-la-montana-al-fuego inputs @preouts)]
   #_[[45 21] (partial noche/fuego inputs @preouts)]
   #_[[50 0] (partial noche/alejamiento-del-fuego inputs @preouts)]
   [[52 22] noche/polinizadores-nocturnos]
   #_[[59 0] (partial noche/hacia-un-nuevo-universo inputs @preouts)
      [68 0] end]])

(def context
  {:inputs inputs
   :preouts preouts
   :current-panners current-panners
   :main-fx main-fx
   :special-inputs special-inputs
   :reaper-returns reaper-returns})

(def performance-config
  {:context context
   :sections sections
   :rec? true})

(defn- fn-name [f]
  (first (str/split (str f) #"@")))

(defn start-sequencer!
  [{:keys [context sections initial-section rec?]}]
  (let [sections** (drop-while (fn [sect]
                                 (println initial-section (second sect))
                                 (not= (fn-name initial-section) (fn-name (second sect)))) sections)
        sections* (if (seq sections**) sections** sections)
        starting-time (->> sections* first first
                           ((fn [[m s]] (+ (* 60 m) s))))]
    (hseq/sequencer context sections*)
    (timbre/debug "starting-time" starting-time (count sections*) "/" (count sections))
    (reaper/time starting-time) ;; in case it's already playing
    ;; TODO add reaper/rec
    (reaper/play)))

(comment
  (do
    (amanecer/free-intercambios-de-energia hseq/context)
    (amanecer/coro-de-la-manana-distancia-de-la-escucha-stop hseq/context)
    (dia/dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación-stop hseq/context)
    (dia/dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales-stop hseq/context)
    (o/stop)
    (gp/stop)
    (reaper/stop)
    (reset! habitat-initialized? false))
  (init!)
  ;; for testing
  (start-sequencer!
   {:context context
    :sections sections
    :initial-section dia/dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía})
  (def test-context (atom (merge {:dur-s (* 5 60)
                                  :stop-rate 1/5}
                                 context)))

  (dia/dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía test-context)
  (dia/dueto-con-polinizadores=pt2-percepción-de-señal-danza-desarrollo-de-energía test-context)
  (dia/dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación test-context)
  (dia/dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación-stop test-context)
  (dia/dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales test-context)
  (dia/dueto-con-polinizadores=pt5-movimiento-energía-alejamiento->viento test-context)
  (dia/dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales-stop test-context)
  (dia/escucha-de-aves test-context)

  #_(amanecer/coro-de-la-manana-distancia-de-la-escucha test-context)
  #_(amanecer/coro-de-la-manana-distancia-de-la-escucha-stop test-context)
  #_(amanecer/intercambios-de-energia test-context)
  #_(amanecer/humedad test-context)
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
  (map :bus (vals inputs))
  ;; TODO automatizar rec de reaper y desaactivación de loop
  (start-sequencer! sections)

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
