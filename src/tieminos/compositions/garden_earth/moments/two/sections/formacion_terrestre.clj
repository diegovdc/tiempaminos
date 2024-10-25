(ns tieminos.compositions.garden-earth.moments.two.sections.formacion-terrestre
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :as scale]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.harmonies
    :as two.harmonies]
   [tieminos.compositions.garden-earth.moments.two.rains
    :refer [rain-simple-playbuf]]
   [tieminos.compositions.garden-earth.moments.two.rec
    :refer [start-rec-loop!]]
   [tieminos.compositions.garden-earth.moments.two.synths
    :refer [delayed-ps ndef-magma-lava]]
   [tieminos.compositions.garden-earth.routing :as ge.route :refer [fl-i1]]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(defn- rec-loop!
  [{:keys [id dur section subsection input-bus]
    :or {id :formacion-terrestre/rec-loop
         section "formacion-terrestre"
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

(defn tectonic-rates
  [interval n & {:keys [direction harmony]
                 :or {direction :down
                      harmony two.harmonies/fib}}]
  (let [dir-fn (if (= direction :down) - +)]
    (map (fn [i]
           (scale/deg->freq harmony 1 (dir-fn (+ i 1))))
         (range 0 (* interval n) interval))))

(def ^:private delay-weights {0 5
                              #(rrange 0.01 0.2) 2
                              #(rrange 0.5 3) 1
                              #(rrange 3 10) 4})

(defn delayed-ps-estratos-base-config
  [{:keys [ratio out delay-weights]
    :or {delay-weights delay-weights}
    ;;  may include things like in, pan-min, pan-max
    :as config}]
  (merge {:group (groups/mid)
          :in (fl-i1 :bus)
          :delay (weighted delay-weights)
          :dur (rrange 5 30)
          :a (rrange 0.2 0.5)
          :ratio ratio
          :amp 1
          :amp-boost-ctl-bus (ge.route/ctl-bus (rand-nth [:exp/btn-2 :exp/btn-a]))
          :amp-boost-min 1
          :amp-boost-max 4
          :amp-boost-lag (rrange 5 18)
          :out out}
         ;; NOTE functions and unwelcome data structures like maps should be disassociated
         (dissoc config :delay-weights)))

(comment
  (do
    (def section (nth sections 1))
    ((:on-start section)))
  ((:on-end section)))

(def mvts-tectonicos-query
  {:section "formacion-terrestre"
   :subsection "movimientos-tectónicos"})

(def emergiendo-del-océano-query
  {:section "formacion-terrestre"
   :subsection "emergiendo-del-océano"})

(def estratos-query
  {:section "formacion-terrestre"
   :subsection "estratos"})

(def estratos-ecosistema-input-query
  {:section "formacion-terrestre"
   :subsection "ecosistema.input"})

(def estratos-ecosistema-rain-query
  {:section "formacion-terrestre"
   :subsection "ecosistema.delay-ps-rain"})

(def estratos-ecosistema-magma-nutrients-query
  {:section "formacion-terrestre"
   :subsection "ecosistema.magma-nutriets"})

;;;;;;;;;;;;;;;;
;;; Helpers
;;;;;;;;;;;;;;;;

(def delay-atom (atom {}))

(defn delay-fn
  [{:keys [buf index]}]
  (let [delay? (get @delay-atom index)
        delay?* (or delay? (rand-nth [true false]))]
    (when-not delay? (swap! delay-atom assoc index delay?*))
    (if delay?*
      (rand (/ (:duration buf) 3))
      0)))

(defn rates-fn
  [_]
  (tectonic-rates (rrand 2 7)
                  (rrand 2 9)
                  {:direction (rand-nth [:up :down])
                   :harmony two.harmonies/meta-pelog}))

(comment
  (require '[tieminos.compositions.garden-earth.moments.two.async-sequencer :as aseq]
           '[tieminos.compositions.garden-earth.moments.two.habitat-in-volcanic-temporality :as ivt.main])

  (ivt.main/stop! {})
  (ivt.main/init!*)

  (let [sections sections]
    (aseq/run-sections sections 0))

  (aseq/pause)
  (aseq/resume)
  (aseq/skip)
  (aseq/prev)
  (aseq/stop)

  (->> @gp/refrains
       vals
       (filter (comp :playing? deref)))

  (gp/stop)

  ;; TODO ecosistema es muy corto cambiar duraciones
  )

;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;; Sections
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;

(def sections
  [;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lava-solidificada (submarina)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :lava-solidificada
         dur 1
         subsection (name name*)]
     {:name name*
      :description ["Submarina"
                    "preparar: raspador en pastilla, boost; gain: 3.5; bass: 2+; watts: max;master: 11;"]
      :dur/minutes dur
      :on-start (fn []
                  #_(rec-loop!
                     {:subsection subsection
                      :dur 10
                      :input-bus (fl-i1 :bus)})

                  ;; continua pero atenuado y más esporádico
                  #_(rain-simple-playbuf
                     {:id :formacion-terrestre/mvts-tectónicos
                      :rec-query mvts-tectonicos-query
                      :durs-fn (fn [_] (rrange 6 9))
                      :amp-fn (fn [_] (rrange 0.2 0.8))
                      :rates-fn (fn [_] (tectonic-rates (rrand 1 5) (rrand 1 9)))
                      :out (ge.route/out :rain-1)}))
      :handlers {:exp/btn-2 {:description "toggle: ndef-magma-lava"
                             :fn/on (fn [_]
                                      (ndef-magma-lava
                                       {:id :formacion-terrestre/btn-2
                                        :group (groups/mid)
                                        :in (fl-i1 :bus)
                                        :lpf 20000
                                        :a-level-ctl (ge.route/ctl-bus :exp/pedal-1)
                                        :a-level-ctl-min 0.4
                                        :a-level-ctl-max 1
                                        :a-level-ctl-lag 0.1
                                        :pre-amp 22
                                        :amp (rrange 0.5 2)
                                        :out (ge.route/out :magma-ndef)}))
                             :fn/off (fn [_] (ndef/stop :formacion-terrestre/btn-2))}}
      :on-end (fn []
                (ndef/stop :formacion-terrestre/btn-2)
                (gp/stop :formacion-terrestre/rec-loop))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; movimientos-tectonicos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :movimientos-tectónicos
         dur 3
         subsection (name name*)
         rain-config {:id (keyword "formacion-terrestre" subsection)
                      :rec-query mvts-tectonicos-query
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description "raspador en pastilla, boost; gain: 3.5; bass: 2+; watts: max;master: 11;"
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                   (merge {:dur 10 :input-bus (fl-i1 :bus)}
                          mvts-tectonicos-query))

                  (rain-simple-playbuf
                   {:id :formacion-terrestre/mvts-tectónicos
                    :rec-query mvts-tectonicos-query
                    :durs-fn (fn [_] (rrange 4 9))
                    :amp-fn (fn [_] (rrange 0.1 0.3))
                    :rates-fn (fn [_] (tectonic-rates (rrand 1 5) (rrand 1 6)))
                    :old-weight 3
                    :synth-config (fn [_]
                                    {:a 0.3
                                     :s 0.2
                                     :r 0.3
                                     :rev-mix (rrange 0.4 1)
                                     :amp-ctl-min 0.5
                                     :amp-ctl-max 4
                                     :delay (rand 3)})
                    :out (ge.route/out :rain-1)}))
      :on-end (fn []
                (gp/stop :formacion-terrestre/rec-loop))})

;;;;;;;;;;;;;;;;
;;; :emergiendo-del-océano
;;;;;;;;;;;;;;;;
   (let [name* :emergiendo-del-océano
         dur 2
         subsection (name name*)]
     {:name name*
      :description "liso, capas, fb... delayed-ps (meta-pelog), usado en siguiente sección "
      :dur/minutes dur
      :on-start (fn []
                  (ref-rain
                   :id :formacion-terrestre/emergiendo-del-océano
                   :durs (fn [_] (rrange 0.5 2))
                   :on-event (on-event
                              (delayed-ps
                               (delayed-ps-estratos-base-config
                                {:ratio (* (-> two.harmonies/meta-pelog rand-nth :bounded-ratio)
                                           (weighted {1 1
                                                      1/2 3
                                                      1/4 2
                                                      2 2}))
                                 :out (ge.route/out :estratos-rain)}))))

                  (let [durs (map (fn [_] (weighted {7 2, 5 4, 3 3, 2 2})) (range 20))]
                    (start-rec-loop!
                     (merge emergiendo-del-océano-query
                            {:id :formacion-terrestre/rec-loop
                             :input-bus (ge.route/out :estratos-rain)
                             :rec-dur-fn (fn [{:keys [index]}] (wrap-at index durs))
                             :rec-pulse durs})))

                  (rain-simple-playbuf
                   {:id :formacion-terrestre/emergiendo-del-océano.simple-playbuf
                    :rec-query emergiendo-del-océano-query
                    :durs-fn (fn [_] (rrange 9 18))
                    :amp-fn (fn [_] (rrange 0.2 0.8))
                    :rates-fn (fn [_] (rand-nth [1 1/2 2]))
                    :old-weight 8
                    :out (ge.route/out :rain-1)})

                  ;; continua pero atenuado y aún más esporádico
                  (rain-simple-playbuf
                   {:id :formacion-terrestre/mvts-tectónicos
                    :rec-query mvts-tectonicos-query
                    :durs-fn (fn [_] (rrange 9 18))
                    :amp-fn (fn [_] (rrange 0.1 0.5))
                    :rates-fn (fn [_] (tectonic-rates (rrand 1 3) (rrand 1 4)))
                    :old-weight 8
                    :synth-config (fn [_]
                                    {:a 0.3
                                     :s 0.2
                                     :r 0.3
                                     :rev-mix 0.5
                                     :amp-ctl-min 0.5
                                     :amp-ctl-max 4
                                     :delay (rand 3)})
                    :out (ge.route/out :rain-1)}))
      :handlers {:exp/btn-2 {:description "delayed-ps amp boost 1/2 of the sounds"}
                 :exp/btn-a {:description "delayed-ps amp boost 1/2 of the sounds"}}
      :on-end (fn []
                (gp/stop :formacion-terrestre/rec-loop)
                (gp/stop :formacion-terrestre/emergiendo-del-océano)
                (gp/stop :formacion-terrestre/emergiendo-del-océano.simple-playbuf))})

;;;;;;;;;;;;;;;;
;;; :terremotos
;;;;;;;;;;;;;;;;
   #_(let [name* :terremotos
           dur 1
           subsection (name name*)]
       {:name name*
        :dur/minutes dur
        :on-start (fn []
                    (rec-loop!
                     {:subsection subsection
                      :dur 10
                      :input-bus (fl-i1 :bus)})

                    (rain-simple-playbuf
                     {:id :formacion-terrestre/mvts-tectónicos
                      :rec-query mvts-tectonicos-query
                      :durs-fn (fn [_] (rrange 3 8))
                      :amp-fn (fn [_] (rrange 0.5 1.2))
                      :delay-fn delay-fn
                      :rates-fn rates-fn
                      :out (ge.route/out :rain-1)})

                    (rain-simple-playbuf
                     {:id :formacion-terrestre/emergiendo-del-océano
                      :rec-query emergiendo-del-océano-query
                      :durs-fn (fn [_] (rrange 3 18))
                      :amp-fn (fn [_] (rrange 0.1 1.2))
                      :rates-fn rates-fn
                      :delay-fn delay-fn
                      :out (ge.route/out :rain-1)}))

        :handlers {:exp/btn-2
                   {:description "toggle: ndef-magma-lava"
                    :fn/on (fn [_]
                             (ndef-magma-lava
                              {:id :formacion-terrestre/btn-2
                               :group (groups/mid)
                               :in (fl-i1 :bus)
                               :lpf 20000
                               :a-level-ctl (ge.route/ctl-bus :exp/pedal-1)
                               :a-level-ctl-min 0.4
                               :a-level-ctl-max 1
                               :a-level-ctl-lag 0.1
                               :amp (rrange 0.5 2)
                               :out (ge.route/out :magma-ndef)}))
                    :fn/off (fn [_] (ndef/stop :formacion-terrestre/btn-2))}}
        :on-end (fn []
                  (gp/stop :formacion-terrestre/rec-loop)
                  (gp/stop :formacion-terrestre/emergiendo-del-océano)
                  (gp/stop :formacion-terrestre/mvts-tectónicos)
                  (ndef/stop :formacion-terrestre/btn-2))})

;;;;;;;;;;;;;;;;;;;;
;;; :ecosistema
;;;;;;;;;;;;;;;;;;;;

   (let [name* :ecosistema
         dur 1
         subsection (name name*)
         rain-config {:id (keyword "formacion-terrestre" subsection)
                      :rec-query {:section "formacion-terrestre"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}
         delay-weights {0 5
                        #(rrange 0.01 0.2) 5
                        #(rrange 0.5 3) 1
                        #(rrange 9 11) 3}
         rain-durs-weights {(fn [] (rrange 0.1 0.8)) 3
                            (fn [] (rrange 0.5 1.3)) 2
                            (fn [] (rrange 1.3 3))   1}
         transp-weights {1 1, 1/2 3, 2 2}
         ps-degrees [0 2 4 6 7 9 11 12 14 18 17
                     0 1 3 5 7 8 9 10 11 13 16]]
     {:name name*
      :description "\"magma\" ndef en :exp/pedal-1, hacer tierra, pizz, delayed-ps en meta-slendro"
      :dur/minutes dur
      :on-start (fn []
                  (ref-rain
                   :id :formacion-terrestre/ecosistema
                   :durs (fn [_] (weighted rain-durs-weights))
                   :on-event (on-event
                              (delayed-ps
                               (delayed-ps-estratos-base-config
                                {:ratio (if (> 0.5 (rand))
                                          (* (-> two.harmonies/meta-slendro1 rand-nth :bounded-ratio)
                                             (weighted transp-weights))
                                          (scale/deg->freq two.harmonies/meta-slendro1 1
                                                           (at-i ps-degrees)))
                                 :delay-weights delay-weights
                                 :out (ge.route/out :estratos-rain)}))))

                  (ndef-magma-lava
                   {:id :formacion-terrestre/ecosistema
                    :group (groups/mid)
                    :in (fl-i1 :bus)
                    :lpf 20000
                    :a-level-ctl (ge.route/ctl-bus :exp/pedal-1)
                    :a-level-ctl-min 0.4
                    :a-level-ctl-max 1
                    :a-level-ctl-lag 0.1
                    :amp (rrange 0.5 2)
                    :out (ge.route/out :magma-ndef)})

                  ;; rec for use in last totalidad
                  (rec-loop!
                   (merge estratos-ecosistema-input-query
                          {:id :formacion-terrestre/estratos-ecosistema-rain.rec-loop
                           :dur 11
                           :input-bus (fl-i1 :bus)}))
                  (rec-loop!
                   (merge estratos-ecosistema-rain-query
                          {:id :formacion-terrestre/estratos-ecosistema-rain.rec-loop
                           :dur 11
                           :input-bus (ge.route/out :estratos-rain)}))
                  (rec-loop!
                   (merge estratos-ecosistema-magma-nutrients-query
                          {:id :formacion-terrestre/estratos-ecosistema-magma-nutrients.rec-loop
                           :dur 8
                           :input-bus (ge.route/out :magma-ndef)})))
      :handlers {:exp/btn-2 {:description "delayed-ps amp boost 1/2 of the sounds"}
                 :exp/btn-a {:description "delayed-ps amp boost 1/2 of the sounds"}}
      :on-end (fn []
                (gp/stop :formacion-terrestre/rec-loop)
                (gp/stop :formacion-terrestre/ecosistema)
                (gp/stop :formacion-terrestre/estratos-ecosistema-rain.rec-loop)
                (gp/stop :formacion-terrestre/estratos-ecosistema-magma-nutrients.rec-loop)
                (ndef/stop :formacion-terrestre/ecosistema))})

;;;;;;;;;;;;;;;;
;;; :estrátos
;;;;;;;;;;;;;;;;
   (let [name* :estrátos
         dur 2
         subsection (name name*)
         rain-config {:id (keyword "formacion-terrestre" subsection)
                      :rec-query {:section "formacion-terrestre"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}
         delay-weights {0 5
                        #(rrange 0.01 0.2) 5
                        #(rrange 0.5 3) 1
                        #(rrange 9 11) 3}
         rain-durs-weights {(fn [] (rrange 0.1 0.8)) 3
                            (fn [] (rrange 0.5 1.3)) 2
                            (fn [] (rrange 1.3 3))   1}
         transp-weights {1 1, 1/2 3, 2 2}
         rate-weights {1 1, 1/2 3, 2 3 3 1}
         ps-degrees [0 2 4 6 7 9 11 12 14 18 17
                     0 1 3 5 7 8 9 10 11 13 16]]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  (ref-rain
                   :id :formacion-terrestre/estratos
                   :durs (fn [_] (weighted rain-durs-weights))
                   :on-event (on-event
                              (delayed-ps
                               (delayed-ps-estratos-base-config
                                {:ratio (if (> 0.5 (rand))
                                          (* (-> two.harmonies/fib rand-nth :bounded-ratio)
                                             (weighted transp-weights))
                                          (scale/deg->freq two.harmonies/fib 1
                                                           (at-i ps-degrees)))
                                 :delay-weights delay-weights
                                 :out (ge.route/out :estratos-rain)}))))

                  (rec-loop!
                   (merge estratos-query
                          {:dur 15
                           :input-bus (ge.route/out :estratos-rain)}))

                  (rain-simple-playbuf
                   {:id :formacion-terrestre/estratos-2
                    :rec-query estratos-query
                    :durs-fn (fn [_] (rrange 3 18))
                    :amp-fn (fn [_] (rrange 0.1 1.2))
                    :rates-fn (fn [_] (weighted rate-weights))
                    :old-weight 8
                    :delay-fn delay-fn
                    :out (ge.route/out :rain-1)}))
      :handlers {:exp/btn-2 {:description "delayed-ps amp boost 1/2 of the sounds"}
                 :exp/btn-a {:description "delayed-ps amp boost 1/2 of the sounds"}}
      :on-end (fn []
                (gp/stop :formacion-terrestre/rec-loop)
                (gp/stop :formacion-terrestre/estratos)
                (gp/stop :formacion-terrestre/estratos-2))})])

(comment
  (gp/stop)
  (-> @gp/refrains
      keys)
  (def section (nth sections 0))
  ((:on-start section))
  ((:on-end section)))
