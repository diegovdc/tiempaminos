(ns tieminos.compositions.garden-earth.moments.two.sections.erupcion
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.harmonies :as two.harmonies]
   [tieminos.compositions.garden-earth.moments.two.rains :refer [rain-simple-playbuf]]
   [tieminos.compositions.garden-earth.moments.two.rec
    :refer [start-rec-loop!]]
   [tieminos.compositions.garden-earth.moments.two.sections.formacion-terrestre :refer [delayed-ps-estratos-base-config]]
   [tieminos.compositions.garden-earth.moments.two.synths :refer [bi-out
                                                                  delayed-ps
                                                                  fade-rev magma ndef-clean-out ndef-erupcion-rev-bank ndef-interior-del-la-tierra
                                                                  ndef-magma-lava]]
   [tieminos.compositions.garden-earth.routing :as ge.route :refer [fl-i1]]
   [tieminos.habitat.recording :as habitat.rec]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.utils
    :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [wrap-at]]))

(defn- rec-loop!
  [{:keys [id dur section subsection input-bus]
    :or {id :erupcion/rec-loop
         section "erupcion"
         dur 10}}]
  (when-not subsection (timbre/warn "========== WARN: Subsection should not be nil============"))
  (start-rec-loop!
   {:id id
    :section section
    :subsection subsection
    :input-bus input-bus
    :rec-dur-fn (fn [_] dur)
    :rec-pulse [dur]}))

(comment
  (do
    (def section (nth sections 1))
    ((:on-start section)))
  ((:on-end section)))

(def erupcion-ndef-query-1
  {:section "erupcion"
   :subsection "erupcion-ndef-query-1"})

(comment
  (ndef/stop)
  (when-let [buf (habitat.rec/rand-queried-buf al-interior-de-la-tierra-query)]
    (println buf)
    (magma {:buf buf
            :out (ge.route/out  :rain-1)}))

  (ndef/ndef
   :test
   (* 0.2 (o/sin-osc 200))
   {:group (groups/early)
    :out (ge.route/out :rain-1)}))

(defn magma-lava-on-event
  [{:keys [rec-query rates-fn a-level-fn amp]} index]
  (when-let [buf (habitat.rec/rand-queried-buf rec-query)]
    (println buf)
    (magma {:group (groups/early)
            :buf buf
            :rate (rates-fn index)
            :dur  (weighted {1 5
                             2 1
                             3 4
                             9 10})
            :start (rand)
            :end (rand)

            :a-level (a-level-fn index)
            :d-level 0.1
            :amp amp
            :pan (rrange -1.0 1)
            :out (ge.route/out  :magma-rain)})))

(defn rain-magma-lava
  [{:as params
    :keys [id
           rec-query
           amp
           durs-fn
           a-level-fn
           rates-fn
           loop?]
    :or {id :erupcion/magma
         amp 1/2
         durs-fn (fn [_] 1/5) ;; TODO might be too fast? but that was in the original
         a-level-fn (fn [i]
                      ;;  Incluso llegando a 10 suena muy chingon y la secuencia funciona bien
                      (wrap-at i  [3 ;; 2 3 4 5 ;; 6 7 8 9 10
                                   ]))
         rates-fn (fn [i]
                    (wrap-at i
                             [1 3/2 1/2
                              #_7/4
                              #_11/4 ;; buena distor
                              ]))
         loop? true}}]
  (ref-rain
   :id id
   :durs durs-fn
   :loop? loop?
   :on-event (on-event (magma-lava-on-event params index))))

(def al-interior-de-la-tierra-query
  {:section "erupcion"
   :subsection "al-interior-de-la-tierra-query"})

(def mvts-magmaticos-query
  {:section "erupcion"
   :subsection "movimientos-magmaticos"})

(defonce bi-out-bus (atom nil))

(defn lava-handlers
  [& {:keys [ndef-magma-lava-params]}]
  {:exp/btn-a {:description "delayed-ps amp boost 1/2 of the sounds"}
   :exp/btn-b {:description "toggle: clean-out"
               :fn/on (fn [_]
                        (ndef-clean-out
                          {:id :erupcion/btn-b.mantel-plume.clean
                           :group (groups/mid)
                           :pan (rrange -0.5 0.5)
                           :in (fl-i1 :bus)
                           :fade-time (rrange 0.7 2)
                           :out @bi-out-bus}))
               :fn/off (fn [_] (ndef/stop :erupcion/btn-b.mantel-plume.clean))}
   :exp/btn-1 {:description "toggle: fade-rev"}
   :exp/btn-2 {:description "delayed-ps amp boost 1/2 of the sounds"}
   :exp/btn-3 {:description "toggle: ndef-magma-lava"
               :fn/on (fn [_]
                        (ndef-magma-lava
                          (merge {:id :erupcion/btn-2.mantel-plume.magma-lava
                                  :group (groups/mid)
                                  :in (fl-i1 :bus)
                                  :lpf 20000
                                  :a-level-ctl (ge.route/ctl-bus :exp/pedal-1)
                                  :a-level-ctl-min 0.4
                                  :a-level-ctl-max 1
                                  :a-level-ctl-lag 0.1
                                  :amp (rrange 0.5 1)
                                  :fade-in (rrange 0.2 2)
                                  :fade-out (rrange 1.5 3)
                                  :out @bi-out-bus}
                                 (if (fn? ndef-magma-lava-params)
                                   (ndef-magma-lava-params)
                                   ndef-magma-lava-params))))
               :fn/off (fn [_] (ndef/stop :erupcion/btn-2.mantel-plume.magma-lava))}})

(comment
  (gp/stop)
  (ndef/stop)
  )
(def sections
  [;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; al interior de la tierra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :al-interior-de-la-tierra
         dur 1.5
         subsection (name name*)
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description ["seco y contenido, slide de metal sobre pastilla"
                    "hacia el final: preparar amp para atractores"
                    "amp 0.5w, pastilla 3"]
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                    (merge al-interior-de-la-tierra-query
                           {:dur 10
                            :input-bus (ge.route/out :erupcion-ndef)}))

                  (let [rates [1 2/3 1/2]]
                    (rain-simple-playbuf
                      {:id :erupcion/al-interior-de-la-tierra
                       :rec-query al-interior-de-la-tierra-query
                       :durs-fn (fn [_] (rrange 2 4))
                       :amp-fn (fn [_] (rrange 2 4))
                       :rates-fn (fn [_] (rand-nth rates))
                       :synth-config {:a 0.2
                                      :s 0.3
                                      :r 0.5
                                      :rev-mix 0.5}
                       :out (ge.route/out :rain-1)}))

                  (ndef-interior-del-la-tierra
                    {:id :erupcion/al-interior-de-la-tierra
                     :group (groups/mid)
                     :in (fl-i1 :bus)
                     :out (ge.route/out :erupcion-ndef)
                     :fade-time 15})

                  (fade-rev
                    {:id :erupcion/al-interior-de-la-tierra.fade-rev
                     :ins [(ge.route/fl-i1 :bus) (ge.route/out :erupcion-ndef)]
                     :out (ge.route/out :ndef-1)
                     :room 2
                     :amp-boost-bus (ge.route/ctl-bus :exp/btn-1)
                     :amp-boost-min 0
                     :amp-boost-max 2
                     :amp-boost-lag 4}))

      :handlers {:exp/btn-1 {:description "toggle: fade-rev (4s)"}}
      :on-end (fn []
                (gp/stop :erupcion/rec-loop)
                (gp/stop :erupcion/al-interior-de-la-tierra)
                (ndef/stop :erupcion/al-interior-de-la-tierra)
                (ndef/stop :erupcion/al-interior-de-la-tierra.fade-rev))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :movimientos-magmáticos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :movimientos-magmáticos
         dur 1
         subsection (name name*)
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description "resonar de atractores, libre, brillante"
      :dur/minutes dur
      :on-start (fn []
                  ;; grabar para posteriormente usar en refrain de lava
                  (rec-loop!
                   (merge mvts-magmaticos-query
                          {:dur 2
                           :input-bus (fl-i1 :bus)})))
      :handlers {:exp/btn-2
                 {:description "toggle: ndef-magma-lava"
                  :fn/on (fn [_]
                           (ndef-magma-lava
                            {:id :formacion-terrestre/btn-2
                             :group (groups/mid)
                             :in (fl-i1 :bus)
                             :lpf 20000
                             :a-level-ctl (ge.route/ctl-bus :exp/pedal-1)
                             :a-level-ctl-min 0.6
                             :a-level-ctl-max 1
                             :a-level-ctl-lag 0.1
                             :amp (rrange 0.7 2)
                             :out (ge.route/out :magma-ndef)}))
                  :fn/off (fn [_] (ndef/stop :formacion-terrestre/btn-2))}}
      :on-end (fn []
                (gp/stop :erupcion/rec-loop))})

;;;;;;;;;;;;;;;;;;
;;; :mantel-plume
;;;;;;;;;;;;;;;;;;
   (let [name* :mantel-plume
         dur 1
         subsection (name name*)
         rev-bank-bus (o/audio-bus 2 "rev-bank-bus")
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}
         transp-weights {1 1, 1/2 3, 2 2}
         ps-degrees [0 2 4 6 7 9 11 12 14 18 17
                     0 1 3 5 7 8 9 10 11 13 16]
         bi-out-synth-1 (atom nil)]
     {:name name*
      :description ["menos seco, slide y raspador-huacharaca sobre pastilla, ascenciones opaco-brillante"
                    "cerrar volumen del ampli (quizá?)"]
      :dur/minutes dur
      :on-start (fn []
                  ;;  TODO quizá grabar y usar para erupción
                  #_(rec-loop!
                      {:subsection subsection
                       :dur 10
                       :input-bus (fl-i1 :bus)})

                  ;; TODO IMPORTANT free all these things

                  ;;  the two outs
                  (reset! bi-out-bus (o/audio-bus 2 "bi-out-bus"))
                  (reset! bi-out-synth-1 (bi-out {:group (groups/late)
                                                  :in @bi-out-bus
                                                  :r 40
                                                  :out-1 (ge.route/out :mantle-plume-main)
                                                  :out-2 rev-bank-bus}))
                  #_(ndef-erupcion-rev-bank
                    {:id :erupcion/mantel-plume.rev-bank
                     :group (groups/fx)
                     :in rev-bank-bus
                     :out (ge.route/out :mantle-plume-rev)
                     :fade-time 15})

                  ;; main fx
                  #_(ndef-interior-del-la-tierra
                    {:id :erupcion/mantel-plume.interior-del-la-tierra
                     :group (groups/mid)
                     :in (fl-i1 :bus)
                     :out @bi-out-bus
                     :fade-time 15})

                  #_(ref-rain
                    :id :erupcion/mantel-plume.ps
                    :durs (fn [_] (rrange 0.5 2))
                    :on-event (on-event
                                (delayed-ps
                                  (delayed-ps-estratos-base-config
                                    {:ratio
                                     (if (> 0.5 (rand))
                                       (* (-> two.harmonies/fib rand-nth :bounded-ratio)
                                          (weighted {1 1
                                                     1/2 3
                                                     11/4
                                                     8/7}))
                                       (scale/deg->freq two.harmonies/fib 1
                                                        (at-i ps-degrees)))

                                     :out @bi-out-bus}))))
                  (fade-rev
                    {:id :erupcion/mentel-plume.fade-rev
                     :ins [(ge.route/fl-i1 :bus)]
                     :out (ge.route/out :ndef-1)
                     :amp-boost-bus (ge.route/ctl-bus :exp/btn-1)
                     :amp-boost-min 0
                     :amp-boost-max 0.7
                     :amp-boost-lag 4}))

      :handlers (merge
                  {:exp/btn-1 "toggle: fade-rev"}
                  (lava-handlers {:ndef-magma-lava-params (fn [] {:amp (rrange 0.3 1)})}))
      :on-end (fn []
                (gp/stop :erupcion/rec-loop)
                #_(ndef/stop :erupcion/mentel-plume.fade-rev)
                #_(ndef/stop :erupcion/mantel-plume.rev-bank)
                #_(ndef/stop :erupcion/mantel-plume.interior-del-la-tierra)
                (gp/stop :erupcion/mantel-plume.ps)
                #_(ndef/stop :erupcion/btn-b.mantel-plume.clean)
                #_(ndef/stop :erupcion/btn-2.mantel-plume.magma-lava)
                (o/ctl @bi-out-synth-1 :gate 0))})

;;;;;;;;;;;;;;;;
;;; erupción
;;;;;;;;;;;;;;;;
   (let [name* :erupcion
         dur 1
         subsection (name name*)
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}
         ps-degrees [0 2 -4 6 7 9 11 -12 -14 18 17
                     0 -1 3 -5 7 8 9 10 11 -13 -16]]
     {:name name*
      :description ["gaseoso, humo-rocas"
                    "golpes dedos y slide"
                    "abrir volumen del ampli (quizá?)"
                    "rev-con-magma = humo"]
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})

                  ;; Comienza rain-magma-lava
                  (let [rates [1 3/2 1/2]]
                    (rain-magma-lava
                      {:rec-query  mvts-magmaticos-query
                       :amp 1/4
                       :a-level-fn (fn [_i] (+ 0.1 (rand 2.5)))
                       :rates-fn (fn [i] (wrap-at i rates))}))

                  (ref-rain
                    :id :erupcion/erupcion.ps
                    :durs (fn [_] (rrange 1 2))
                    :on-event (on-event
                                (delayed-ps
                                  (delayed-ps-estratos-base-config
                                    {:ratio
                                     (if (> 0.5 (rand))
                                       (* (-> two.harmonies/fib rand-nth :bounded-ratio)
                                          (weighted {1 1
                                                     1/2 3
                                                     11/4
                                                     8/7}))
                                       (scale/deg->freq two.harmonies/fib 1
                                                        (at-i ps-degrees)))

                                     :out @bi-out-bus}))))

                  (fade-rev
                    {:id :erupcion/erupcion.fade-rev
                     :ins [(ge.route/fl-i1 :bus)]
                     :out (ge.route/out :ndef-1)
                     :amp-boost-bus (ge.route/ctl-bus :exp/btn-1)
                     :amp-boost-min 0
                     :amp-boost-max 0.7
                     :amp-boost-lag 4}))

      :handlers (merge {
                        ;; TODO implement
                        ;; :exp/pedal-1 {:description "[TODO] Vol de todo excepto rev-con-magma y exp/btn-3"}
                        ;; :exp/btn-3 {:description "[TODO] humo-delay-ps ~1/1 (low-prob gliss ascedente), envs lfo-kr amp+pan"}
                        }
                       (lava-handlers {:ndef-magma-lava-params (fn [] {:amp (rrange 0.5 1)})}))

      
      :on-end (fn []
                (gp/stop :erupcion/rec-loop))})

;;;;;;;;;;;;;;;;;;;;
;;; Erupción+Lava
;;;;;;;;;;;;;;;;;;;;
   (let [name* :erupcion+lava
         dur 1 ;; TODO shorten when rest of the sections are added
         subsection (name name*)
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description ["continua erupción, comienza lava"
                    "lava, creciendo"
                    "dejar"
                    "hacia el final preparar silencio"]
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})

                  (let [a-levels [1 2 3 4 5]
                        rates [1 3/2 1/2 11/4]]
                    (rain-magma-lava
                      {:rec-query  mvts-magmaticos-query
                       :amp 1/2
                       :a-level-fn (fn [i] (wrap-at i a-levels))
                       :rates-fn (fn [i] (wrap-at i rates))})))
      :handlers (lava-handlers {:ndef-magma-lava-params (fn [] {:amp (rrange 0.7 2)})})
      :on-end (fn []
                (ndef/stop :erupcion/mantel-plume.rev-bank)
                (gp/stop :erupcion/rec-loop))})

;;;;;;;;;;;;;;;;
;;; Lava
;;;;;;;;;;;;;;;;
   (let [name* :lava
         dur 2.5 ;; TODO shorten when rest of the sections are added
         subsection (name name*)
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description ["lava, creciendo"
                    "dejar"]
      :dur/minutes dur
      :on-start (fn []
                  #_(rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})

                  (let [a-levels [3 2 3 4 5 6 7 8 9 10]
                        rates [1 3/2 1/2 7/4 2/3 11/4]]
                    (rain-magma-lava
                      {:rec-query  mvts-magmaticos-query
                       :amp 1/2
                       :a-level-fn (fn [i] (wrap-at i a-levels))
                       :rates-fn (fn [i] (wrap-at i rates))})))
      ;; TODO handlers, controlar parámetros de la lava
      :handlers (lava-handlers
                  {:ndef-magma-lava-params (fn [] {:amp (rrange 0.7 2)})})
      :on-end (fn []
                (gp/stop :erupcion/rec-loop)
                (ndef/stop :erupcion/mentel-plume.fade-rev)
                (ndef/stop :erupcion/mantel-plume.rev-bank)
                (ndef/stop :erupcion/mantel-plume.interior-del-la-tierra)
                (gp/stop :erupcion/erupcion.ps)
                (ndef/stop :erupcion/btn-b.mantel-plume.clean)
                (ndef/stop :erupcion/btn-2.mantel-plume.magma-lava)
                ;; TODO cómo decrecer al final?
                (let [a-levels [3 2 1 (rand 0.5) (rand 0.3) (rand 0.2) (rand 0.1) (rand 0.05) (rand 0.01)]
                      rates [1  2/3 1/2 8/7 1/3 32/11 1/2 1/4]]
                  (rain-magma-lava
                    {:rec-query  mvts-magmaticos-query
                     :loop? false
                     ;; 8 durs
                     :durs (map #(* 8 %) [1/5 1/5 1/4 1/4 1/3 1/2 1 3/2])
                     :amp 1/4
                     :a-level-fn (fn [i] (wrap-at i a-levels))
                     :rates-fn (fn [i] (wrap-at i rates))})))})

;;;;;;;;;;;;;;;;;;;;
;;; cuasi-silencio
;;;;;;;;;;;;;;;;;;;;

   (let [name* :cuasi-silencio
         dur 3
         subsection (name name*)
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description ["decresciendo->silencio, ruido, y pequeñas reverberaciones"
                    "agregar perillas 1-2, 3-4, 5-6"]
      :dur/minutes dur
      :on-start (fn []
                  #_(rec-loop!
                     {:subsection subsection
                      :dur 10
                      :input-bus (fl-i1 :bus)}))
      :on-end (fn []
                (ndef/stop :erupcion/erupcion.fade-rev)
                (gp/stop :erupcion/rec-loop))})]    )

(comment
  (gp/stop)
  (-> @gp/refrains
      keys)
  (def section (nth sections 0))
  ((:on-start section))
  ((:on-end section)))
