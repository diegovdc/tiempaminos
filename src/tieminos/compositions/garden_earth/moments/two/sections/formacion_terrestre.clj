(ns tieminos.compositions.garden-earth.moments.two.sections.formacion-terrestre
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.harmonies :as two.harmonies]
   [tieminos.compositions.garden-earth.moments.two.rec
    :refer [start-rec-loop!]]
   [tieminos.compositions.garden-earth.moments.two.synths
    :refer [delayed-ps ndef-magma-lava simple-playbuf]]
   [tieminos.compositions.garden-earth.routing :as ge.route :refer [fl-i1]]
   [tieminos.habitat.recording :as habitat.rec]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.utils
    :refer [rrange wrap-at]]
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

(defn normalize-rates
  "Ensure `rates` is a vector"
  [rates]
  (if (number? rates) [rates] rates))

(defn rain-simple-playbuf
  [{:keys [id
           rates-fn
           durs-fn
           amp-fn
           rec-query
           delay-fn
           out]
    :or {amp-fn (fn [_i] 4)
         durs-fn (fn [_] (+ 0.1 (rand 5)))
         delay-fn (fn [{:keys [_index _buf _rate]}] 0)
         out (ge.route/out :ndef-1)}}]
  (ref-rain
   :id id
   :durs durs-fn
   :on-event (on-event
              (when-let [buf (habitat.rec/weigthed-rand-queried-buf
                              {:rec-query rec-query
                               :recent-amount 2
                               :recent-weight 2
                               :old-weight 1})]
                (doseq [rate (normalize-rates (rates-fn index))]
                  (simple-playbuf
                   {:group (groups/early)
                    :buf buf
                    :rate rate
                    :amp (amp-fn index)
                    :amp-ctl (ge.route/ctl-bus :exp/pedal-1)
                    :amp-ctl-min 0.25
                    :amp-ctl-max (o/db->amp 4)
                    :delay (delay-fn {:buf buf :index index :rate rate})
                    :pan (rrange -1.0 1)
                    :out out}))))))

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
    :or {delay-weights delay-weights}}]
  {:group (groups/mid)
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
   :out out})

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
  [;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; movimientos-tectonicos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :movimientos-tectónicos
         dur 2
         subsection (name name*)
         rain-config {:id (keyword "formacion-terrestre" subsection)
                      :rec-query mvts-tectonicos-query
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description "raspador en pastilla, <>"
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                   (merge {:dur 10 :input-bus (fl-i1 :bus)}
                          mvts-tectonicos-query))

                  (rain-simple-playbuf
                   {:id :formacion-terrestre/mvts-tectónicos
                    :rec-query mvts-tectonicos-query
                    :durs-fn (fn [_] (rrange 4 9))
                    :amp-fn (fn [_] (rrange 0.5 1.2))
                    :rates-fn (fn [_] (tectonic-rates (rrand 1 5) (rrand 1 6)))
                    :out (ge.route/out :rain-1)}))
      :on-end (fn []
                (gp/stop :formacion-terrestre/rec-loop))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lava-solidificada (submarina)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :lava-solidificada
         dur 1
         subsection (name name*)]
     {:name name*
      :description "Submarina"
      :dur/minutes dur
      :on-start (fn []
                  #_(rec-loop!
                     {:subsection subsection
                      :dur 10
                      :input-bus (fl-i1 :bus)})

                  ;; continua pero atenuado y más esporádico
                  (rain-simple-playbuf
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
                                        :amp (rrange 0.5 2)
                                        :out (ge.route/out :magma-ndef)}))
                             :fn/off (fn [_] (ndef/stop :formacion-terrestre/btn-2))}}
      :on-end (fn []
                (ndef/stop :formacion-terrestre/btn-2)
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
                     :out (ge.route/out :rain-1)})

                  ;; continua pero atenuado y aún más esporádico
                  (rain-simple-playbuf
                    {:id :formacion-terrestre/mvts-tectónicos
                     :rec-query mvts-tectonicos-query
                     :durs-fn (fn [_] (rrange 9 18))
                     :amp-fn (fn [_] (rrange 0.2 0.8))
                     :rates-fn (fn [_] (tectonic-rates (rrand 2 7) (rrand 2 9)))
                     :out (ge.route/out :rain-1)}))
      :on-end (fn []
                (gp/stop :formacion-terrestre/rec-loop)
                (gp/stop :formacion-terrestre/emergiendo-del-océano)
                (gp/stop :formacion-terrestre/emergiendo-del-océano.simple-playbuf))})

;;;;;;;;;;;;;;;;
;;; :terremotos
;;;;;;;;;;;;;;;;
   (let [name* :terremotos
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
                    :out (ge.route/out :magma-ndef)}))
      :on-end (fn []
                (gp/stop :formacion-terrestre/rec-loop)
                (gp/stop :formacion-terrestre/ecosistema)
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
                    :delay-fn delay-fn
                    :out (ge.route/out :rain-1)}))
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
