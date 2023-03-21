(ns tieminos.habitat.main
  (:require
   [clojure.string :as str]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.init :refer [habitat-initialized? init!]]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.osc :as habitat-osc :refer [args->map map-val]]
   [tieminos.habitat.panners :refer [current-panners panner panner-rate]]
   [tieminos.habitat.parts.amanecer :as amanecer]
   [tieminos.habitat.parts.dia-back :as dia]
   [tieminos.habitat.parts.noche :as noche]
   [tieminos.habitat.resonance-panner :as reso-pan]
   [tieminos.habitat.routing
    :refer [inputs
            preouts
            reaper-returns
            special-inputs
            texto-sonoro-rand-mixer-bus]]
   [tieminos.habitat.synths.main-fx :refer [main-fx]]
   [tieminos.osc.reaper :as reaper]
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

(def sections
  [;; amanecer
   ;; TODO revisar timestamps vs texto sonoro
   [[0 0] #'amanecer/humedad]
   [[5 20] #'amanecer/sol-y-luminosidad]
   ;;  bajarle a la conv
   [[7 30] #'amanecer/intercambios-de-energia]
   [[9 0] #'amanecer/inicio-descomposicion]
   [[10 20] #'amanecer/descomposicion-hacia-la-tierra]
   ;; TODO cascabeles giratorios
   [[12 24] #'amanecer/coro-de-la-manana-cantos-iniciales]
   [[14 30] #'amanecer/coro-de-la-manana-interacciones-cuanticas]
   [[17 0] #'amanecer/coro-de-la-manana-distancia-de-la-escucha]
   [[20 10] #'amanecer/solo-de-milo]
   ;; día
   ;; mover a 23:14
   [[22 48] #'dia/dueto-con-polinizadores=pt1-emisión-de-señal-intercambio-de-energía]
   [[24 30] #'dia/dueto-con-polinizadores=pt2-percepción-de-señal-danza-desarrollo-de-energía]
   [[25 47] #'dia/dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación]
   [[27 51] #'dia/dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales]
   ;; NOTE cambio en marca de tiempo respecto de la partitura
   [[28 30] #'dia/dueto-con-polinizadores=pt5-movimiento-energía-alejamiento->viento]
   #_[[29 55] tacet]
   [[31 11] #'dia/escucha-de-aves]
   ;; noche
   [[39 56] #'noche/de-la-montana-al-fuego]
   [[42 30] #'noche/fuego]
   [[52 22] #'noche/polinizadores-nocturnos]
   ;; FIXME parece que hay un error en la transición de estas dos secciones
   [[62 0] #'noche/hacia-un-nuevo-universo]
   [[67 45] #'noche/hacia-un-nuevo-universo-stop]])

(def context
  {:inputs inputs
   :preouts preouts
   :current-panners current-panners
   :main-fx main-fx
   :special-inputs special-inputs
   :texto-sonoro-rand-mixer-bus texto-sonoro-rand-mixer-bus
   :reaper-returns reaper-returns})

(def performance-config
  {:context context
   :sections sections
   :rec? true})

(defn start-sequencer!
  [{:keys [context sections initial-section rec?]}]
  (let [sections** (drop-while (fn [sect]
                                 (not= initial-section (second sect))) sections)
        sections* (if (seq sections**) sections** sections)
        starting-time (->> sections* first first
                           ((fn [[m s]] (+ (* 60 m) s))))]
    (hseq/sequencer context sections*)
    (timbre/debug "starting-time" starting-time (count sections*) "/" (count sections))
    (reaper/time starting-time) ;; in case it's already playing
    (if rec?
      (reaper/rec)
      (reaper/play))))

(defn stop-sequencer! [context]
  (amanecer/free-intercambios-de-energia context)
  (amanecer/coro-de-la-manana-distancia-de-la-escucha-stop context)
  (dia/dueto-con-polinizadores=pt3-polen-electromagnetismo-agitación-stop context)
  (dia/dueto-con-polinizadores=pt4-multiplicación-atracción-orbitales-stop context)
  (noche/fuego-stop context)
  (noche/hacia-un-nuevo-universo-stop context)
  (o/stop)
  (gp/stop)
  (reaper/stop)
  (reset! habitat-initialized? false))

(defn start-habitat-performance! []
  (timbre/set-level! :info)
  (init!)
  (start-sequencer! performance-config))

(comment
  (defn- quick-sections [dur sections]
    (map-indexed (fn [i [_ f]] [[0 (* i dur)] f])
                 sections))
  (start-habitat-performance!)
  (stop-sequencer! hseq/context)
  (init!)

  ;; for testing
  (start-sequencer!
   {:context context
    :sections sections #_(quick-sections 5 sections)
    :initial-section #'noche/polinizadores-nocturnos})

  (timbre/set-level! :info)
  #_(amanecer/humedad test-context)
  #_(noche/fuego test-context)
  #_(noche/fuego-stop test-context)
  (def test-context (atom (merge {:dur-s (* 5 60)
                                  :stop-rate 1/5}
                                 context)))
  (noche/hacia-un-nuevo-universo-stop test-context)
  (-> context :main-fx deref :light-reverb :synth (o/ctl :amp 16))
  (-> context :preouts deref)

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
