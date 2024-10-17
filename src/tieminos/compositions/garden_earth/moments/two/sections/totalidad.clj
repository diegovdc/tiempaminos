(ns tieminos.compositions.garden-earth.moments.two.sections.totalidad
  "Vida: regalo de las profundidades"
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.moments.two.rec
    :refer [start-rec-loop!]]
   [tieminos.compositions.garden-earth.moments.two.synths
    :refer [buf-mvts-subterraneos ndef-mvts-subterraneos simple-playbuf]]
   [tieminos.compositions.garden-earth.routing :as ge.route :refer [fl-i1]]
   [tieminos.habitat.recording :as habitat.rec]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.utils
    :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))


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

#_(defn rain-mvts-submarinos
  [{:keys [id
           rates-fn
           durs-fn
           amp-fn
           rec-query]
    :or {amp-fn (fn [_i] 2)
         durs-fn (fn [_] (+ 0.1 (rand 5)))
         rec-query {:section "totalidad"
                    :subsection "totalidad"}}}]
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
  (do
    (def section (nth sections 1))
    ((:on-start section)))
  ((:on-end section)))

(def totalidad-ndef-query-1
  {:section "totalidad"
   :subsection "totalidad-ndef-query-1"})



(def sections
  [                                     ;
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
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})
                  )
      :on-end (fn []
                (gp/stop :totalidad/rec-loop))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :formación-de-montañas/estratos (sumando)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (let [name* :formación-de-montañas-estratos
         dur 2
         subsection (name name*)
         rain-config {:id (keyword "totalidad" subsection)
                      :rec-query {:section "totalidad"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :description "Submarina"
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})
                  )
      :on-end (fn []
                (gp/stop :totalidad/rec-loop))})

;;;;;;;;;;;;;;;;
;;; :multiplicación-de-ecosistemas
;;;;;;;;;;;;;;;;
   (let [name* :multiplicación-de-ecosistemas
         dur 2
         subsection (name name*)
         rain-config {:id (keyword "totalidad" subsection)
                      :rec-query {:section "totalidad"
                                  :subsection subsection}
                      :durs-fn (fn [_] (+ 0.1 (rand 6)))}]
     {:name name*
      :dur/minutes dur
      :on-start (fn []
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})
                  )
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
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})
                  )
      :on-end (fn []
                (gp/stop :totalidad/rec-loop))})

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
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})
                  )
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
      :on-start (fn []
                  (rec-loop!
                    {:subsection subsection
                     :dur 10
                     :input-bus (fl-i1 :bus)})
                  )
      :on-end (fn []
                (gp/stop :totalidad/rec-loop))})])

(comment
  (gp/stop)
  (-> @gp/refrains
      keys)
  (def section (nth sections 0))
  ((:on-start section))
  ((:on-end section)))
