(ns tieminos.compositions.garden-earth.moments.two.sections.fondo-oceanico
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.rains :refer [rain-simple-playbuf]]
   [tieminos.compositions.garden-earth.moments.two.rec
    :refer [start-rec-loop!]]
   [tieminos.compositions.garden-earth.moments.two.synths
    :refer [buf-mvts-subterraneos ndef-mvts-subterraneos]]
   [tieminos.compositions.garden-earth.routing :as ge.route :refer [fl-i1]]
   [tieminos.habitat.recording :as habitat.rec]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.utils
    :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))


(comment
  (map (fn [_]
         (->> (habitat.rec/weigthed-rand-queried-buf
               {:rec-query {:section "fondo-oceanico"
                            :subsection "fondo-oceanico"}
                :recent-amount 3
                :recent-weight 4
                :old-weight 1})
              #_(map second)
              (into {})
              (#(-> % (select-keys [:id #_:rec/meta])))))
       (range 10)))

(defn- rec-loop!
  [{:keys [id dur section subsection input-bus]
    :or {id :fondo-oceanico/rec-loop
         section "fondo-oceanico"
         dur 10}}]
  (when-not subsection
    (timbre/warn "========== WARN: Subsection should not be nil============"))
  (start-rec-loop!
   {:id id
    :section section
    :subsection subsection
    :input-bus input-bus
    :rec-dur-fn (fn [_] dur)
    :rec-pulse [dur]}))

(comment
  (require '[overtone.core :as o])

  (habitat.rec/rand-queried-buf  {:section "fondo-oceanico"
                      :subsection "fondo-oceanico"})
  (o/demo (o/play-buf 1 (habitat.rec/rand-queried-buf  {:section "fondo-oceanico"
                                            :subsection "fondo-oceanico"})))

  (oe/defsynth
    sini
    [out 0]
    (o/out out (* 0.5 (o/sin-osc 200))))
  (def t (sini {:out 0}))
  (def t (sini {:group (groups/early)
                :out (ge.route/out :ndef-1)}))
  (o/kill t))

(defn rain-mvts-submarinos
  [{:keys [id
           rates-fn
           durs-fn
           amp-fn
           rec-query]
    :or {amp-fn (fn [_i] 2)
         durs-fn (fn [_] (+ 0.1 (rand 5)))
         rec-query {:section "fondo-oceanico"
                    :subsection "fondo-oceanico"}}}]
  (ref-rain
   :id id
   :durs durs-fn
   :on-event (on-event
               ;; TODO replace by weighted-queried-buf
               (when-let [buf (habitat.rec/rand-queried-buf rec-query)]
                 (buf-mvts-subterraneos
                   {:group (groups/early)
                    :buf buf
                    :rate (rates-fn index)
                    :max-rev-mix 1
                    :rev-room 2
                    :amp (amp-fn index)
                    :amp-ctl (ge.route/ctl-bus :exp/pedal-1)
                    :amp-ctl-min 0.25
                    :amp-ctl-max (o/db->amp 4)
                    :pan (rrange -1.0 1)
                    :out (ge.route/out :rain-1)})))))

(comment
  (->> @habitat.rec/bufs
       vals
       (map (partial into {})))
  (do
    (def section (nth sections 1))
    ((:on-start section)))
  ((:on-end section)))

(comment
  (-> @gp/refrains
      keys))

(def fondo-oceanico-ndef-query-1
  {:section "fondo-oceanico"
   :subsection "fondo-oceanico-ndef-query-1"})

(def erupciones-submarinas-query
  {:section "fondo-oceanico"
   :subsection "erupciones-submarinas"})

(def quimio-sintesis-query
  {:section "fondo-oceanico"
   :subsection "quimio-sintesis"})

(def ecosistema-submarino-query
  {:section "fondo-oceanico"
   :subsection "ecosistema-submarino"})

;; TODO remove
#_(defn rain-simple-playbuf
    [{:keys [id rates-fn durs-fn amp-fn rec-query out]
      :or {amp-fn (fn [_i] 4)
           durs-fn (fn [_] (+ 0.1 (rand 5)))
           out (ge.route/out :ndef-1)}}]
    (ref-rain
      :id id
      :durs durs-fn
      :on-event (on-event
                  ;; TODO replace by weighted-queried-buf
                  (when-let [buf (habitat.rec/rand-queried-buf rec-query)]
                    (simple-playbuf
                      {:group (groups/early)
                       :buf buf
                       :rate (rates-fn index)
                       :amp (amp-fn index)
                       :amp-ctl (ge.route/ctl-bus :exp/pedal-1)
                       :amp-ctl-min 0.25
                       :amp-ctl-max (o/db->amp 4)
                       :pan (rrange -1.0 1)
                       :out out})))))

(def sections
  [;;;;;;;;;;;;;;;;;;;;
;;; Fondo Oceánico
;;;;;;;;;;;;;;;;;;;;
   (let [name* :fondo-oceanico
         dur 2
         subsection (name name*)
         rain-config {:id (keyword "fondo-oceanico" subsection)
                      :rec-query {:section "fondo-oceanico"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))
                      :amp-fn (fn [_i] (rrange 2 20)) ;; TODO improve amps variation
                      ;; TODO changes rates?
                      :rates-fn (fn [_] (rand-nth [1 11/9 1/2 2/3 4/11 9/22]))}]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                   {:subsection subsection
                    :dur 10
                    :input-bus (fl-i1 :bus)})

                  ;; TODO add controls for `amp` and `dur`?
                  ;; Or somehow start later
                  (rain-mvts-submarinos rain-config)

                  ;; ndef
                  (ndef-mvts-subterraneos {:id :fondo-oceanico/fondo-oceanico
                                           :out (ge.route/out :ndef-1)})
                  ;; rec but don't play yet
                  (rec-loop!
                   (merge {:id :fondo-oceanico/rec-loop.fondo-oceanico-ndef
                           :dur 10
                           :input-bus (ge.route/out :ndef-1)}
                          fondo-oceanico-ndef-query-1)))
      :on-end (fn []
                (gp/stop :fondo-oceanico/rec-loop)
                (gp/stop :fondo-oceanico/rec-loop.fondo-oceanico-ndef)
                (ndef/stop :fondo-oceanico/fondo-oceanico)
                ;; Lower freq and amp
                (rain-mvts-submarinos
                 (merge rain-config
                        {:durs-fn (fn [_] (+ 1 (rand 7)))
                         :amp-fn (fn [_i] (rrange 0 2))})))})
;;;;;;;;;;;;;;;;;;;;;;;
;;; Fuente Hidrotermal
;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :fuente-hidrotermal
         dur 2
         subsection (name name*)
         rain-config {:id (keyword "fondo-oceanico" subsection)
                      :rec-query {:section "fondo-oceanico"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))
                      :amp-fn (fn [_i] (rrange 1 2)) ;; TODO improve amps variation
                      ;; TODO changes rates?
                      :rates-fn (fn [_] (rand-nth [1 11/9 1/2 2/3 4/11 9/22]))}]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  ;; rec
                  (rec-loop!
                   {:subsection (name name*)
                    :dur 10
                    :input-bus (fl-i1 :bus)})

                  ;; rains
                  (rain-mvts-submarinos rain-config)

                  (rain-simple-playbuf
                   {:id :fondo-oceanico/simple-playbuf.fondo-oceanico-ndef
                    :rec-query fondo-oceanico-ndef-query-1
                    :rates-fn (fn [_] (rand-nth [1 2/3 8/7 3/2]))
                    :out (ge.route/out :ndef-1)})

                  ;; ndef
                  (ndef-mvts-subterraneos {:id :fondo-oceanico/fuente-hidrotermal
                                           :out (ge.route/out :ndef-1)
                                           :rates [1/2 7/8 9/8 10/4]
                                           :amp-boost-ctl-bus (ge.route/ctl-bus :exp/btn-2)
                                           :amp-boost-min 0
                                           :amp-boost-max 1
                                           :amp-boost-lag 4}))
      :on-end (fn []
                (gp/stop :fondo-oceanico/rec-loop)
                (gp/stop :fondo-oceanico/simple-playbuf.fondo-oceanico-ndef)
                (ndef/stop :fondo-oceanico/fuente-hidrotermal)
                ;; Lower freq and amp
                (rain-mvts-submarinos
                 (merge rain-config
                        {:durs-fn (fn [_] (+ 1 (rand 7)))
                         :amp-fn (fn [_i] (rand-nth [2 1]))})))})

   ;; TODO left here
   (let [name* :erupciones-submarinas
         dur 2]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  ;; for use in totalidad
                  (rec-loop!
                    (merge erupciones-submarinas-query
                           {:dur 10 :input-bus (fl-i1 :bus)})))
      :on-end (fn [])})

   (let [name* :quimio-sintesis
         dur 1]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  ;; for use in totalidad
                  (rec-loop!
                    (merge quimio-sintesis-query
                           {:dur 10 :input-bus (fl-i1 :bus)})))
      :on-end (fn [])})

   (let [name* :ecosistema-submarino
         dur 1]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  ;; for use in totalidad
                  (rec-loop!
                    (merge ecosistema-submarino-query
                           {:dur 10 :input-bus (fl-i1 :bus)})))
      :on-end (fn [])})])

(comment
  (gp/stop)
  (-> @gp/refrains
      keys)
  (def section (nth sections 0))
  ((:on-start section))
  ((:on-end section)))
