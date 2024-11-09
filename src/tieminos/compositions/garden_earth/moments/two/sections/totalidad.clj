(ns tieminos.compositions.garden-earth.moments.two.sections.totalidad
  "Vida: regalo de las profundidades"
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.base :refer [dur->env]]
   [tieminos.compositions.garden-earth.fl-grain-1.sample-arp :refer [arp-reponse-2 bz
                                                                     fsf play-sample rand-val]]
   [tieminos.compositions.garden-earth.moments.two.async-sequencer :as aseq]
   [tieminos.compositions.garden-earth.moments.two.harmonies :as two.harmonies]
   [tieminos.compositions.garden-earth.moments.two.rains :refer [rain-simple-playbuf]]
   [tieminos.compositions.garden-earth.moments.two.rec
    :refer [start-rec-loop!]]
   [tieminos.compositions.garden-earth.moments.two.sections.fondo-oceanico :as fondo-oceanico]
   [tieminos.compositions.garden-earth.moments.two.sections.formacion-terrestre
    :as formacion-terrestre :refer [delayed-ps-estratos-base-config]]
   [tieminos.compositions.garden-earth.moments.two.synths :refer [delayed-ps
                                                                  delayed-ps2
                                                                  fade-rev]]
   [tieminos.compositions.garden-earth.routing :as ge.route :refer [fl-i1]]
   [tieminos.habitat.recording :as habitat.rec]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.utils
    :refer [rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(defn- rec-loop!
  [{:keys [id dur section subsection input-bus]
    :or {id :totalidad/rec-loop
         section "totalidad"
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
  (do
    (def section (nth sections 1))
    ((:on-start section)))
  ((:on-end section)))

(def totalidad-ndef-query-1
  {:section "totalidad"
   :subsection "totalidad-ndef-query-1"})

(def amp-weights-low-prominence
  {#(rrange 0.05 0.2) 3
   #(rrange 0.2 0.5) 4
   #(rrange 0.5 0.7) 3
   #(rrange 0.7 0.9) 1
   #(rrange 0.9 1) 1/2
   #(rrange 1 1.5) 2/3})

(def amp-weights-mid-prominence
  {#(rrange 0.05 0.2) 1
   #(rrange 0.2 0.5) 3
   #(rrange 0.5 0.7) 4
   #(rrange 0.7 0.9) 2
   #(rrange 0.9 1) 1/2
   #(rrange 1 1.5) 1})

(def amp-weights-high-prominence
  {#(rrange 0.05 0.2) 1
   #(rrange 0.2 0.5) 1
   #(rrange 0.5 0.7) 2
   #(rrange 0.7 0.9) 3
   #(rrange 0.9 1) 4
   #(rrange 1 1.5) 2})

;; TODO with frequency analyzer make pitch-class useful
(defn interval-seq-fn
  [_pitch-class scale]
  (let [direction (rand-nth [1 -1])
        offset (rrand -6 6)]
    (->> (range 0 (+ 9 (rand-int 7)) 2)
         (map #(scale/deg->freq scale (rand-nth [1 2 4 8 16]) (+ offset (* % direction)))))))

(def envs {:atk-reso1 {:a 0.1 :d 0.01 :r 5}
           :atk-reso2 {:a 0.1 :d 0.01 :r 0.5}})

(defn arp-reponse-3
  [{:keys [scale out interval-seq-fn
           env-min-dur env-max-dur
           amp-min amp-max]
    :or {out 0
         interval-seq-fn interval-seq-fn
         env-min-dur 3
         env-max-dur 5
         amp-min 1
         amp-max 1.5}
    :as _config-data}
   {:as arp-data :keys [pitch-class]}]
  (when pitch-class
    (let [intervals (interval-seq-fn pitch-class scale)
          durs ((rand-val bz)
                (count intervals)
                (rand-nth [#_0.01 0.1 0.3])
                (rand-nth [0.17 0.5 0.8]))
          env-durs ((rand-val bz) (count intervals) env-min-dur env-max-dur)
          env (rand-val envs)
          amps (fsf (count intervals) amp-min amp-max)
          d-level ((rand-val bz) (count intervals) 0.1 0.3)]
      (ref-rain :id (keyword "arp" (str "response1-" (rand-int 5000)))
                :durs durs
                :loop? false
                :on-event
                (on-event
                 (play-sample arp-data
                              {:out out
                               :rate (at-index intervals)
                               :d-level (at-index d-level)
                               :amp (at-index amps)
                               :adr-env (dur->env env (at-index env-durs))}))))))

(comment
  (->> (map
        (fn [_] (weighted amp-weights-mid-prominence))
        (range 100))
       (sort >)))

(defonce ps-rain-bus (atom nil))

(def formacion-de-ecosistema-query
  {:section "totalidad"
   :subsection "formacion-de-ecosistema"})

(def multiplicacion-de-ecosistemas-query
  {:section "totalidad"
   :subsection "multiplicacion-de-ecosistemas"})

(def sections
  [;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :formación-de-ecosistema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :formación-de-ecosistema
         dur 4
         subsection (name name*)
         rain-config {:id (keyword "totalidad" subsection)
                      :rec-query {:section "totalidad"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description ["meru-X, reafinar ligeramente cuerdas (nuevo ecosistema)"
                    "en algún momento levantar la perilla 3-4 (gradualmente las otras)"]
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                   (merge formacion-de-ecosistema-query
                          {:id :totalidad/formacion-de-ecosistema.rec-loop
                           :dur 20
                           :input-bus (fl-i1 :bus)}))

                  (aseq/async-event
                   {:wait-s 20
                    :on-start (fn []
                                (rain-simple-playbuf
                                 {:id :totalidad/formación-de-ecosistema
                                  :rec-query formacion-de-ecosistema-query
                                  :durs-fn (fn [_] (rand 10))
                                  :amp-fn (fn [_] (*  (weighted amp-weights-high-prominence)))
                                  :rates-fn (fn [_] (formacion-terrestre/tectonic-rates
                                                     (rrand 1 6)
                                                     (rrand 1 3)
                                                     {:direction (rand-nth [:up :down])
                                                      :harmony two.harmonies/meta-slendro2}))

                                  :old-weight 3
                                  :synth-config (fn [_]
                                                  {:a 0.3
                                                   :s 0.2
                                                   :r 0.3
                                                   :rev-mix 0.5
                                                   :amp-ctl-min 0.5
                                                   :amp-ctl-max 3
                                                   :delay (rand 10)})
                                  :out (ge.route/out :totalidad-ecosistema)}))}))
      :handlers (:exp/pedal-1 {:description "Samples volume"})
      :on-end (fn [])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :formación-de-montañas (sumando)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :formación-de-montañas-estratos.sumando
         dur 1/6
         subsection (name name*)
         rain-config {:id (keyword "totalidad" subsection)
                      :rec-query {:section "totalidad"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description "Sumando capas"
      :dur/minutes dur
      :on-start (fn []
                  #_(rec-loop!
                     {:subsection subsection
                      :dur 10
                      :input-bus (fl-i1 :bus)})
                  (rain-simple-playbuf
                   {:id :totalidad.formacion-terreste/mvts-tectónicos
                    :rec-query formacion-terrestre/mvts-tectonicos-query
                    :old-weight 8
                    :durs-fn (fn [_] (rrange 6 19))
                    :amp-fn (fn [_] (weighted amp-weights-low-prominence))
                    :rates-fn (fn [_] (formacion-terrestre/tectonic-rates (rrand 1 5) (rrand 1 9)))
                    :out (ge.route/out :rain-1)})

                  (aseq/async-event
                   {:wait-s 8
                    :on-start
                    (fn []
                      (rain-simple-playbuf
                       {:id :totalidad.formacion-terrestre/emergiendo-del-océano.simple-playbuf
                        :rec-query formacion-terrestre/emergiendo-del-océano-query
                        :old-weight 8
                        :durs-fn (fn [_] (rrange 9 18))
                        :amp-fn (fn [_] (weighted amp-weights-low-prominence))
                        :rates-fn (fn [_] (rand-nth [1 1/2 2]))
                        :out (ge.route/out :rain-1)}))}))
      :on-end (fn []
                #_(gp/stop :totalidad/rec-loop)
                ;; Cambio de prominencia
                #_(rain-simple-playbuf
                   {:id :totalidad.formacion-terreste/mvts-tectónicos
                    :rec-query formacion-terrestre/mvts-tectonicos-query
                    :durs-fn (fn [_] (rrange 6 19))
                    :amp-fn (fn [_] (weighted amp-weights-low-prominence))
                    :rates-fn (fn [_] (formacion-terrestre/tectonic-rates (rrand 1 5) (rrand 1 9)))
                    :out (ge.route/out :rain-1)})

                #_(rain-simple-playbuf
                   {:id :totalidad.formacion-terrestre/emergiendo-del-océano.simple-playbuf
                    :rec-query formacion-terrestre/emergiendo-del-océano-query
                    :durs-fn (fn [_] (rrange 9 18))
                    :amp-fn (fn [_] (weighted amp-weights-mid-prominence))
                    :rates-fn (fn [_] (rand-nth [1 1/2 2]))
                    :out (ge.route/out :rain-1)}))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :estratos (sumando)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :estratos.sumando
         dur 1/5
         subsection (name name*)
         rate-weights {1 1, 1/2 3, 2 3 3 1}
         rain-config {:id (keyword "totalidad" subsection)
                      :rec-query {:section "totalidad"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description "Sumando capas"
      :dur/minutes dur
      :on-start (fn []
                  #_(rec-loop!
                     {:subsection subsection
                      :dur 10
                      :input-bus (fl-i1 :bus)})

                  (rain-simple-playbuf
                   {:id :totalidad.formacion-terrestre/estratos-ecosistema-rain
                    :rec-query formacion-terrestre/estratos-ecosistema-rain-query
                    :old-weight 8
                    :durs-fn (fn [_] (rrange 3 18))
                    :amp-fn (fn [_] (rrange 0.1 1.2))
                    :rates-fn (fn [_] (weighted rate-weights))
                    :delay-fn formacion-terrestre/delay-fn
                    :out (ge.route/out :rain-1)})

                  (rain-simple-playbuf
                   {:id :totalidad/formación-de-ecosistema
                    :rec-query formacion-de-ecosistema-query
                    :durs-fn (fn [_] (rand 5))
                    :amp-fn (fn [_] (*  (weighted amp-weights-high-prominence)))
                    :rates-fn (fn [_] (formacion-terrestre/tectonic-rates
                                       (rrand 1 6)
                                       (rrand 1 6)
                                       {:direction (rand-nth [:up])
                                        :harmony two.harmonies/meta-slendro2}))

                    :old-weight 3
                    :synth-config (fn [_]
                                    {:a 0.3
                                     :s 0.2
                                     :r 0.3
                                     :rev-mix 0.5
                                     :amp-ctl-min 0.5
                                     :amp-ctl-max 4
                                     :delay (rand 3)})
                    :out (ge.route/out :totalidad-ecosistema)})

                  (aseq/async-event
                   {:wait-s 20
                    :on-start
                    (fn []
                      (rain-simple-playbuf
                       {:id :totalidad.formacion-terrestre/estratos
                        :rec-query formacion-terrestre/estratos-query
                        :old-weight 8
                        :durs-fn (fn [_] (rrange 3 18))
                        :amp-fn (fn [_] (rrange 0.1 1.2))
                        :rates-fn (fn [_] (weighted rate-weights))
                        :delay-fn formacion-terrestre/delay-fn
                        :out (ge.route/out :rain-1)}))})

                  (aseq/async-event
                   {:wait-s 40
                    :on-start
                    (fn []
                      (rain-simple-playbuf
                       {:id :totalidad.fondo-oceanico/simple-playbuf.fondo-oceanico-ndef
                        :durs-fn (fn [_] (rrange 2 18))
                        :amp-fn (fn [_] (rrange 0.1 0.8))
                        :rec-query fondo-oceanico/ecosistema-submarino-query
                        :old-weight 8
                        :rates-fn (fn [_] (rand-nth [1 2/3 8/7 3/2]))
                        :out (ge.route/out :ndef-1)}))}))
      :on-end (fn []
                (gp/stop :totalidad/rec-loop)
                ;; del momento anterior
                (gp/stop :totalidad.formacion-terreste/mvts-tectónicos)
                (gp/stop :totalidad.formacion-terrestre/emergiendo-del-océano.simple-playbuf))})

;;;;;;;;;;;;;;;;
;;; :multiplicación-de-ecosistemas
;;;;;;;;;;;;;;;;
   (let [name* :multiplicación-de-ecosistemas
         dur 1.5
         rate-weights {1 1, 1/2 3, 2 3, 3 1
                       [1/2 1 3] 1/4
                       [1/2 1 2] 1/2
                       1/4 1/4}
         transp-weights {1 1, 1/2 3, 2 2}
         rain-durs-weights {(fn [] (rrange 0.1 0.8)) 3
                            (fn [] (rrange 0.5 1.3)) 2
                            (fn [] (rrange 1.3 3))   1}
         harmonic-ecosystems [{:harmony two.harmonies/meta-pelog2
                               :pan-min -1
                               :pan-max -0.2}
                              {:harmony two.harmonies/meta-slendro2
                               :pan-min -0.4
                               :pan-max 0.5}
                              {:harmony two.harmonies/meta-pelog
                               :pan-min 0.3
                               :pan-max 1}]
         delay-weights {0 5
                        #(rrange 0.01 0.2) 5/2
                        #(rrange 0.5 3) 1
                        #(rrange 9 11) 3}
         ps-degrees-weights (merge {[0 2 4 6 7 9 11 12 14] 2
                                    [0 1 3 5 7 8 9 10 11] 1
                                    [-1 -6 -4 -3 -2 -5 -5 -5] 1
                                    [6 5 4 3 2 1 0] 1}
                                   (->> (range -2 10 2)
                                        (map
                                         (fn [x]
                                           [(mapv #(+ % x)  [10 5 0 1 0 -5]) 1/4]))
                                        (into {})))]
     {:name name*
      :dur/minutes dur
      :description ["muchos ecosistemas del pasado, delayed-ps se usa mucho."
                    "Aparece arp"]
      :on-start (fn []
                  (reset! ps-rain-bus (o/audio-bus 2 "ps-rain-bus"))
                  (rec-loop!
                   (merge multiplicacion-de-ecosistemas-query
                          {:dur 2
                           :input-bus (fl-i1 :bus)}))

                  (rain-simple-playbuf
                   {:id :totalidad/formación-de-ecosistema2
                    :rec-query formacion-de-ecosistema-query
                    :durs-fn (fn [_] (rand 10))
                    :amp-fn (fn [_] (*  (weighted amp-weights-high-prominence)))
                    :rates-fn (fn [_] (formacion-terrestre/tectonic-rates
                                       (rrand 1 10)
                                       (rrand 1 6)
                                       {:direction (rand-nth [:up])
                                        :harmony two.harmonies/meta-slendro2}))

                    :old-weight 3
                    :synth-config (fn [_]
                                    {:a 0.3
                                     :s 0.2
                                     :r 0.3
                                     :rev-mix 0.5
                                     :amp-ctl-min 0.5
                                     :amp-ctl-max 4
                                     :delay (rand 3)})
                    :out (ge.route/out :totalidad-ecosistema)})

                  (rain-simple-playbuf
                   {:id :totalidad/formacion-de-ecosistema.to-ps
                    :durs-fn (fn [_] (rrange 9 15))
                    :amp-fn (fn [i] (wrap-at i [1 3/2 2 3]))
                    :rec-query formacion-de-ecosistema-query
                    :rates-fn (fn [_] (weighted rate-weights))
                    :old-weight 4
                    :out @ps-rain-bus})

                  (ref-rain
                   :id :totalidad/ecosistema.ps-rain
                   :durs (fn [_] (weighted rain-durs-weights))
                   :on-event (on-event
                              (let [{:keys [pan-min pan-max harmony]} (rand-nth harmonic-ecosystems)]
                                ((rand-nth [delayed-ps delayed-ps2])
                                 (delayed-ps-estratos-base-config
                                  {:in @ps-rain-bus
                                   :pan-min pan-min
                                   :lpf (rrange 700 8000)
                                   :pan-max pan-max
                                   :amp (rrange 0.2 0.5) ;; TODO use bezier
                                   :ratio (if (> 0.3 (rand))
                                            (* (-> harmony rand-nth :bounded-ratio)
                                               (weighted transp-weights))
                                            (scale/deg->freq two.harmonies/meta-slendro2
                                                             1
                                                             (at-i (weighted ps-degrees-weights))))
                                   :delay-weights delay-weights
                                   :dry-amp (rand)
                                   :out (ge.route/out :estratos-rain)})))))

                  (ref-rain
                   :id :totalidad/ecosistema.arp
                   :durs (fn [_] (weighted rain-durs-weights))
                   :on-event (on-event
                              (when-let [buf (habitat.rec/weigthed-rand-queried-buf
                                              {:rec-query multiplicacion-de-ecosistemas-query
                                               :recent-amount 1
                                               :recent-weight 2
                                               :old-weight 0})]
                                (arp-reponse-2
                                 {:interval-seq-fn interval-seq-fn
                                  :scale two.harmonies/meta-slendro2
                                  :out (ge.route/out :rain-1)
                                  :amp-min 0.01
                                  :amp-max 0.1}
                                 {:pitch-class "A+92"
                                  :buf buf}))))

                  (aseq/async-event
                   {:wait-s 60
                    :on-start
                    (fn [] (rain-simple-playbuf
                            {:id :totalidad.formacion-terrestre/estratos-ecosistema-magma
                             :rec-query formacion-terrestre/estratos-ecosistema-magma-nutrients-query
                             :old-weight 8
                             :durs-fn (fn [_] (rrange 3 18))
                             :amp-fn (fn [_] (rrange 0.1 0.8))
                             :rates-fn (fn [_] (weighted rate-weights))
                             :delay-fn formacion-terrestre/delay-fn
                             :out (ge.route/out :rain-1)}))})

                  (aseq/async-event
                   {:wait-s 111
                    :on-start
                    (fn [] (rain-simple-playbuf
                            {:id :totalidad.formacion-terrestre/estratos-ecosistema-input
                             :rec-query formacion-terrestre/estratos-ecosistema-input-query
                             :old-weight 8
                             :durs-fn (fn [_] (rrange 3 18))
                             :amp-fn (fn [_] (rrange 0.2 0.7))
                             :rates-fn (fn [_] (weighted rate-weights))
                             :delay-fn formacion-terrestre/delay-fn
                             :out @ps-rain-bus}))}))

      :handlers {:exp/btn-a {:description "delayed-ps amp boost meta-pelog2 harmonies"}
                 :exp/btn-2 {:description "delayed-ps amp boost meta-pelog1 harmonies"}
                 :exp/btn-3 {:description "delayed-ps amp boost meta-slendro2"}}
      :on-end (fn []
                (gp/stop :totalidad/rec-loop))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :algún-lugar-hermoso->hermosura-en-general
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :algún-lugar-hermoso->hermosura-en-general
         dur 0.5
         subsection (name name*)
         rain-config {:id (keyword "totalidad" subsection)
                      :rec-query {:section "totalidad"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  #_(rec-loop!
                     {:subsection subsection
                      :dur 10
                      :input-bus (fl-i1 :bus)}))
      :on-end (fn []
                #_(gp/stop :totalidad/rec-loop))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :ecosistema-oceánico
;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (let [name* :ecosistema-oceánico
         dur 1
         subsection (name name*)
         rain-config {:id (keyword "totalidad" subsection)
                      :rec-query {:section "totalidad"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  #_(rec-loop!
                     {:subsection subsection
                      :dur 10
                      :input-bus (fl-i1 :bus)})

                  (rain-simple-playbuf
                   {:id :fondo-oceanico/simple-playbuf.fondo-oceanico-ndef
                    :rec-query fondo-oceanico/fondo-oceanico-ndef-query-1
                    :durs-fn (fn [_] (rrange 6 19))
                    :amp-fn (fn [_] (weighted amp-weights-mid-prominence))
                    :old-weight 8
                    ;; TODO use other kinds of rates
                    :rates-fn (fn [_] (formacion-terrestre/tectonic-rates (rrand 1 5) (rrand 1 9)))
                    :out (ge.route/out :ndef-1)})

                  (aseq/async-event
                   {:wait-s 10
                    :on-start
                    (fn []
                      (rain-simple-playbuf
                       {:id :totalidad.formacion-terreste/mvts-tectónicos
                        :rec-query formacion-terrestre/mvts-tectonicos-query
                        :old-weight 8
                        :durs-fn (fn [_] (rrange 6 19))
                        :amp-fn (fn [_]  (* 1/2 (weighted amp-weights-low-prominence)))
                        :rates-fn (fn [_] (formacion-terrestre/tectonic-rates (rrand 1 5) (rrand 1 9)))
                        :out (ge.route/out :rain-1)}))}))

      :on-end (fn []
                (gp/stop :totalidad/rec-loop))})

;;;;;;;;;;;;;;;;
;;; :totalidad
;;;;;;;;;;;;;;;;
   (let [name* :totalidad
         dur 1
         subsection (name name*)
         rain-config {:id (keyword "totalidad" subsection)
                      :rec-query {:section "totalidad"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :dur/minutes dur
      :description "Usar fade rev más o menos a la mitad"
      :on-start (fn []
                  #_(rec-loop!
                     {:subsection subsection
                      :dur 10
                      :input-bus (fl-i1 :bus)})
                  (fade-rev
                   {:id :totalidad/totalidad.fade-rev
                    :ins [(ge.route/fl-i1 :bus)
                          (ge.route/out :rain-1)
                          (ge.route/out :ndef-1)
                          (ge.route/out :totalidad-ecosistema)]
                    :out (ge.route/out :ndef-1)
                    :room 3
                    :amp-boost-bus (ge.route/ctl-bus :exp/btn-1)
                    :amp-boost-min 0
                    :amp-boost-max 1
                    :amp-boost-lag 10
                    :fade-in 8
                    :fade-out 50}))
      :handlers {:exp/btn-1 {:description "fade rev"}}
      :on-end (fn []
                (gp/stop :totalidad/rec-loop))})])

(comment
  (ge.route/set-ctl :exp/btn-1 127)
  (gp/stop)
  (-> @gp/refrains
      keys)
  (def section (nth sections 0))
  ((:on-start section))
  ((:on-end section)))
