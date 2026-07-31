(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.dev
  (:require
   [overtone.core :as o]
   [overtone.osc :as osc]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.async-events :as bardo.comms :refer [dispatch]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state]
   [tieminos.habitat.recording :refer [bufs]]
   [tieminos.math.utils :refer [linlin*]]
   [time-time.dynacan.players.gen-poly :as gp]))
;;;;;;;;;;;;;;;;;;
;; Controls
;;;;;;;;;;;;;;;;;;
(comment
  ;; osc
  (osc/osc-debug true)
  (osc/osc-debug false)
  ;; rec
  (dispatch {:type :start-recording :data {:input-k :mic-1}})
  (dispatch {:type :stop-recording :data {:input-k :mic-1}})

  ;; play
  (dispatch {:type :dev/trigger-clouds-event :data {:bank 0}})

  (-> @gp/refrains keys)
  (-> @gp/refrains :bardo.clouds/diego1)
  (gp/stop))

;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;

(comment
  (keys @bardo.live-state/live-state)
  (-> @bardo.live-state/live-state :processors :guitar)
  ;; preset configs keys are weird
  (->> @bardo.live-state/live-state :processors :guitar :preset-configs)
  (->> @bardo.live-state/live-state :processors :guitar :preset-configs keys (map :name))
  (-> @bardo.live-state/live-state :rec)
  (bardo.live-state/get-player-data :milo 0)
  (bardo.live-state/get-player-data :diego 0 :harmonic-active-voices)
  (-> (bardo.live-state/get-player-data :diego)
      :refrains)
  (apply dissoc (bardo.live-state/get-player-data :milo) (range 8))

  (bardo.live-state/get-selected-synth-data :diego)
  (bardo.live-state/get-player-data :diego :refrains)

  (bardo.live-state/get-gusano-data))

;;;;;;;;;;;;;;;;;;;;;
;; Guitar Processes
;;;;;;;;;;;;;;;;;;;;;

(comment
  ;; get current preset params
  (->> (bardo.live-state/get-processor-active-preset-data!)
       :preset
       bardo.live-state/get-preset-modified-params!
       vals
       (mapv (fn [{:keys [synth/param synth/value]}]
               [param value]))
       (sort-by first)
       (into {})))

;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;

(comment
  (-> @bardo.live-state/live-state
      :algo-2.2.9-clouds
      :diego
      (get 0)
      :panner-configs
      :lissajous)
  (remove-watch bardo.live-state/live-state ::state)
  (add-watch bardo.live-state/live-state ::state
             (fn [_ _ _ new-state]
               (let [{:keys [x y]} (-> new-state
                                       :algo-2.2.9-clouds
                                       :diego
                                       (get 0)
                                       :panner-configs
                                       :lissajous)
                     ratio (/ x y)]

                 (println ratio)))))

;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;
(comment
  (-> @bufs first second keys)
  (-> @bufs first second :rec/meta)
  (do
    (def bufs*
      (->> @bufs
           vals
           #_(map #(-> % :rec/meta :input-name))
           (filter #(-> % :rec/meta :input-name #{"mic-1-bus" "mic-2-bus"}))
           #_(map :rec/meta)))
    (->> bufs*
         (filter #(-> % :rec/meta :subsection (= 0)))))

  (o/defsynth bufy
    [buf 0
     amp 1]
    (o/out 0 (* amp (o/play-buf 1 buf :action o/FREE))))

  (def b (bufy (->> bufs*
                    (filter #(-> % :rec/meta :subsection (= 0)))
                    first)))
  (o/kill b)

  (o/defsynth inputy
    [in 0
     amp 1
     out 0]
    (o/out out (* amp (o/sound-in in))))
  (def inp (inputy :in 22
                   ;; :amp (o/db->amp 6)
                   ))
  (o/kill inp)
  (o/stop)

  (bufy (first bufs*)
        (* 8 64))
  (->> bufs*
       first
       (into {})))
