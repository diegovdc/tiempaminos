(ns tieminos.compositions.garden-earth.moments.two.sections.fondo-oceanico
  (:require
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.rec :refer [start-rec-loop!]]
   [tieminos.compositions.garden-earth.moments.two.synths :refer [buf-mvts-subterraneos
                                                                  ndef-mvts-subterraneos simple-playbuf]]
   [tieminos.compositions.garden-earth.routing :as ge.route :refer [fl-i1]]
   [tieminos.habitat.recording :refer [filter-by-rec-meta]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(defn rand-queried-buf [rec-query]
  (try (-> @rec/bufs (filter-by-rec-meta rec-query)
           rand-nth
           second)
       (catch Exception _e nil)))

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

  (rand-queried-buf  {:section "fondo-oceanico"
                      :subsection "fondo-oceanico"})
  (o/demo (o/play-buf 1 (rand-queried-buf  {:section "fondo-oceanico"
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
  [{:keys [id rates-fn durs-fn amp-fn rec-query]
    :or {amp-fn (fn [_i] 4)
         durs-fn (fn [_] (+ 0.1 (rand 5)))
         rec-query {:section "fondo-oceanico"
                    :subsection "fondo-oceanico"}}}]
  (ref-rain
    :id id
    :durs durs-fn
    :on-event (on-event
                (when-let [buf (rand-queried-buf rec-query)]
                  (buf-mvts-subterraneos
                    {:group (groups/early)
                     :buf buf
                     :rate (rates-fn index)
                     :amp (amp-fn index)
                     :pan (rrange -1.0 1)
                     :out (ge.route/out :rain-1)})))))

(comment
  (do
    (def section (nth sections 0))
    ((:on-start section)))
  ((:on-end section)))

(def fondo-oceanico-ndef-query-1
  {:section "fondo-oceanico"
   :subsection "fondo-oceanico-ndef-query-1"})

(defn rain-simple-playbuf
  [{:keys [id rates-fn durs-fn amp-fn rec-query out]
    :or {amp-fn (fn [_i] 4)
         durs-fn (fn [_] (+ 0.1 (rand 5)))
         out (ge.route/out :ndef-1)}}]
  (ref-rain
   :id id
   :durs durs-fn
   :on-event (on-event
              (when-let [buf (rand-queried-buf rec-query)]
                (simple-playbuf
                 {:group (groups/early)
                  :buf buf
                  :rate (rates-fn index)
                  :amp (amp-fn index)
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
                      :amp-fn (fn [_i] 20) ;; TODO improve amps variation
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
                ;; Lower freq and amp
                (rain-mvts-submarinos
                 (merge rain-config
                        {:durs-fn (fn [_] (+ 1 (rand 7)))
                         :amp-fn (fn [_i] (rand-nth [4 1]))})))})
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
                      :amp-fn (fn [_i] 4) ;; TODO improve amps variation
                      ;; TODO changes rates?
                      :rates-fn (fn [_] (rand-nth [1 11/9 1/2 2/3 4/11 9/22]))}]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                   {:subsection (name name*)
                    :dur 10
                    :input-bus (fl-i1 :bus)})
                  (rain-mvts-submarinos rain-config)

                  (rain-simple-playbuf
                   {:id :fondo-oceanico/simple-playbuf.fondo-oceanico-ndef
                    :rec-query fondo-oceanico-ndef-query-1
                    :out (ge.route/out :ndef-1)}))
      :on-end (fn []
                ;; Lower freq and amp
                (rain-mvts-submarinos
                 (merge rain-config
                        {:durs-fn (fn [_] (+ 1 (rand 7)))
                         :amp-fn (fn [_i] (rand-nth [4 1]))})))})

   ;; TODO left here
   (let [name* :erupciones-submarinas
         dur 2]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                   {:subsection (name name*)
                    :dur 10}))
      :on-end (fn [])})

   (let [name* :quimio-sintesis
         dur 1]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                   {:subsection (name name*)
                    :dur 10}))
      :on-end (fn [])})

   (let [name* :ecosistema-submarino
         dur 1]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                   {:subsection (name name*)
                    :dur 10}))
      :on-end (fn [])})])

(comment
  (gp/stop)
  (-> @gp/refrains
      keys)
  (def section (nth sections 0))
  ((:on-start section))
  ((:on-end section)))
