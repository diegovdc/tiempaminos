(ns tieminos.compositions.garden-earth.moments.two.interface
  "The MIDI interface"
  (:require
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.live-state :as two.ls]
   [tieminos.compositions.garden-earth.routing :as ge.route]
   [tieminos.midi.core :refer [get-pacer! midi-in-event]]))

(def features
  [{:section [:section/up :section/down]}
   {:rec [:rec/section :rec/special-tagged]}
   {:activate-ndefs&refrains [:refrain/fondo-oceánico
                              :refrain/quimio-síntesis
                              :ndef/movimientos-subterraneos
                              :ndef/movimientos-submarinos
                              :refrain/simple-pitch-shifter
                              :ndef/simple-pitch-shifter
                              :refrain/magma
                              :ndef/magama
                              :refain/sucesiones
                              :ndef/estratos
                              :synth/amp-reactivity
                              ;; TODO finish list
                              ]}
   {:expression [:expression/pedal
                 :expression/buttons
                 :expression/foot-switch-1
                 :expression/foot-switch-2]}])

(def secciones-principales
  [[:fondo-oceánico 8]
   [:formación-de-tierra 11.0]
   [:erupción 14]
   [:totalidad/vida-regalo-de-las-profundidades 12.0]])

;; Posiblemente lo mejor sea:
;; 1. Tener un solo botón para cambiar secciones.
;; 2. La sección corre en su tiempo hasta llegar al final
;; 3. Volver a apretar ese botón para continuar
;; 4. Quizá subdividir (en dos) algunas de las
;;    `secciones-principales` (sobre todo erupción).
;; 5. Por cada `seccion-principal`:
;; 5.1 Asignar sintes interactivos a interface.
;;     Los sintes interactivos ocupan toda la sección.
;; 5.2. Los otros síntes se activan automáticamente.
;; 6. Si las subsecciones aún no terminan de ejecutarse,
;;    el botón avanzar cambia a la siguiente subsección.
;;

(defn set-ctl [clt-bus-k value]
  (swap! two.ls/live-state assoc clt-bus-k value)
  (ge.route/set-ctl clt-bus-k value))

(do
  (defn init!
    []
    (try
      (midi-in-event
       :midi-input (get-pacer!)
       :auto-ctl? false
       :note-on (fn [{:keys [note velocity]}]
                  (println note)
                  (cond
                    (= 2 note) (println "note 2")
                    (= 3 note) (println "note 3")
                    (= 5 note) (set-ctl :exp/btn-2 velocity)))
       :note-off (fn [{:keys [note velocity]}]
                   (println note)
                   (cond
                     (= 2 note) (println "note 2")
                     (= 3 note) (println "note 3")
                     (= 5 note) (set-ctl :exp/btn-2 velocity)))
       :cc (fn [{:keys [note velocity]}]
             (cond
                ;; expression pedal
               (= 7 note) (set-ctl :exp/pedal-1 velocity))))

      (catch Exception e (timbre/error e))))
  (init!))

(comment
  (two.ls/init-watch!)

  (init!)
  (midi-in-event
   :midi-input (get-pacer!)
   :note-on (fn [{:keys [note]}]
              (println note)
              (cond

                (= 2 note) (println "note 2")))
   :cc (fn [{:keys [note velocity]}]
         (cond
            ;; expression pedal
           (= 7 note) (do (swap! two.ls/live-state assoc :exp/pedal-1 velocity)
                          (ge.route/set-ctl :exp/pedal-1 velocity))))))
