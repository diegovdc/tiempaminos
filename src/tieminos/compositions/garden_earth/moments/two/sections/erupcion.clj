(ns tieminos.compositions.garden-earth.moments.two.sections.erupcion
  (:require
   [clojure.data.generators :refer [weighted]]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.rec
    :refer [start-rec-loop!]]
   [tieminos.compositions.garden-earth.moments.two.synths :refer [magma]]
   [tieminos.compositions.garden-earth.routing :as ge.route :refer [fl-i1]]
   [tieminos.habitat.recording :as habitat.rec]
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


(defn ndef-magma-lava
  [{:keys [in
           amp
           a-level-ctl
           a-level-ctl-min
           a-level-ctl-max
           a-level-ctl-lag]}])

(defn rain-magma-lava
  [{:keys [rec-query
           amp
           durs-fn
           a-level-fn
           rates-fn]
    :or {amp 1/2
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
                              ]))}}]
  (ref-rain
    :id ::magma

    :durs durs-fn
    :on-event (on-event
                (when-let [buf (habitat.rec/rand-queried-buf rec-query)]
                  (magma {:buf buf
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
                          :out (ge.route/out :magma-rain)})))))


(def sections
  [                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; al interior de la tierra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :al-interior-de-la-tierra
         dur 2
         subsection (name name*)
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description "seco y contenido, raspador-huacharaca sobre pastilla"
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})
                  )
      :on-end (fn []
                (gp/stop :erupcion/rec-loop))})

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
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})

                  ;; ndef magma
                  )
      :on-end (fn []
                (gp/stop :erupcion/rec-loop))})

;;;;;;;;;;;;;;;;;;
;;; :mantel-plume
;;;;;;;;;;;;;;;;;;

   (let [name* :mantel-plume
         dur 1
         subsection (name name*)
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description "menos seco, raspador-huacharaca sobre pastilla, ascenciones opaco-brillante"
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})
                  )
      :on-end (fn []
                (gp/stop :erupcion/rec-loop))})
;;;;;;;;;;;;;;;;
;;; erupción
;;;;;;;;;;;;;;;;
   (let [name* :erupcion
         dur 1
         subsection (name name*)
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description "gaseoso, humo-rocas"
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})
                  )
      :on-end (fn []
                (gp/stop :erupcion/rec-loop))})

;;;;;;;;;;;;;;;;
;;; Lava
;;;;;;;;;;;;;;;;
   (let [name* :lava
         dur 4 ;; TODO shorten when rest of the sections are added
         subsection (name name*)
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description "lava, creciendo"
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})
                  )
      :on-end (fn []
                (gp/stop :erupcion/rec-loop))})

;;;;;;;;;;;;;;;;;;;;
;;; cuasi-silencio
;;;;;;;;;;;;;;;;;;;;

   (let [name* :cuasi-silencio
         dur 2
         subsection (name name*)
         rain-config {:id (keyword "erupcion" subsection)
                      :rec-query {:section "erupcion"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description "decresciendo->silencio, ruido, y pequeñas reverberaciones"
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})
                  )
      :on-end (fn []
                (gp/stop :erupcion/rec-loop))})])

(comment
  (gp/stop)
  (-> @gp/refrains
      keys)
  (def section (nth sections 0))
  ((:on-start section))
  ((:on-end section)))
