(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-controls
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.math :refer [round]]
   [clojure.string :as str]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.habitat.extended-sections.harmonies.chords
    :refer [fib-21 meta-pelog meta-pelog-11 meta-pelog-7 meta-slendro-5
            meta-slendro1 rate-chord-seq]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.clouds
    :refer [clouds-refrain2]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.gusanos.core
    :as bardo.gusano]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state
    :as bardo.live-state
    :refer [live-state]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec
    :as bardo.rec]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths
    :refer [play-synth]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :refer [inputs main-returns]]
   [tieminos.math.bezier-samples :as bzs]
   [tieminos.math.utils :refer [linlin]]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp]))

;;;;;;;;;;;;;;
;; Recording
;;;;;;;;;;;;;;

(defn- make-rec-id [input-k]
  (keyword "bardo.rec-loop" (name input-k)))

(defn get-rec-pulse [{:keys [dur pulse]}]
  (case pulse
    :dur*2 (* 2 dur)
    :rand-2 (+ dur (rand dur))
    :rand-4 (+ dur (rand (* 3 dur)))
    dur))

(comment
  (-> @live-state :rec :mic-1 :dur)
  (def input-k :mic-2))

(defn start-recording
  [{:keys [input-k]}]
  (timbre/info "starting rec on" input-k)
  (if-let [input-bus (-> @inputs input-k :bus)]
    (bardo.rec/start-rec-loop!
     {:id (make-rec-id input-k)
      :input-k input-k
      :input-bus input-bus
      :rec-dur-fn (fn [_]
                    (-> @live-state :rec input-k :dur))
      :rec-pulse (fn [_] (-> @live-state :rec input-k get-rec-pulse))
       ;; :print-info? true
      :on-rec-start (fn [_]
                      (swap! live-state
                             assoc-in
                             [:rec input-k :last-rec-timestamp]
                             (o/now)))})
    (timbre/error "No input bus for key:" input-k)))

(comment
  (gp/stop)
  (reset! rec/recording? {})
  (start-recording {:input-k :mic-1})
  (->> @rec/bufs
       vals
       (map :duration)
       frequencies))

(defn stop-recording [{:keys [input-k]}]
  (timbre/info "stopping rec on" input-k)
  (gp/stop (make-rec-id input-k)))

;;;;;;;;;;;;;;;;;;
;; Clouds
;;;;;;;;;;;;;;;;;;

(defn- make-clouds-id [player-k]
  (keyword "bardo.clouds" (name player-k)))

(def rit (bzs/f 20 0.5 10))
(def accel (bzs/s 20 0.5 10))

(defn get-dur [index rhythm-k lorentz]
  (case rhythm-k
    :lor-0.1_2 (lorentz/bound (lorentz (* 50 index)) :x 0.1 2)
    :lor-2_6 (lorentz/bound (lorentz (* 50 index)) :x 2 6)
    :rand-0_10 (max 0.1 (rand 10))
    :rit (wrap-at index rit)
    :accel (wrap-at index accel)
    (do
      (timbre/error (ex-info "Unknown rhtyhm key, using default"
                             {:rhythm-k rhythm-k}))
      1)))

(defn get-lorentz-envelope
  [index lorentz min* max*]
  {:a (lorentz/bound (lorentz (* 50 index)) :x min* max*)
   :d (lorentz/bound (lorentz (* 50 index)) :y min* max*)
   :r (lorentz/bound (lorentz (* 50 index)) :z min* max*)})

(defn get-envelope [index env-k lorentz]
  (case env-k
    :lor-1_4 (get-lorentz-envelope index lorentz 1 4)
    :lor-0.1_2 (get-lorentz-envelope index lorentz 0.1 2)
    :a-0.1_0.4*d-2*r-3 {:a (rrange 0.1 0.4) :d 2 :r 3}
    :weights-largos {:a (weighted {10 1
                                   15 0.3})
                     :d (weighted {40 1
                                   30 0.3})
                     :r (weighted {10 1
                                   20 0.3})}
    (do
      (timbre/error (ex-info "Unknown envelope key, using default"
                             {:env-k env-k}))
      {:a 3 :d 3 :r 2})))

(defn lorentz-chord
  [index lorentz lor-speed lowest-note highest-note]
  (let [index* (* lor-speed index)]
    [(round (lorentz/bound (lorentz index*) :x lowest-note highest-note))
     (round (lorentz/bound (lorentz index*) :y lowest-note highest-note))
     (round (lorentz/bound (lorentz index*) :z lowest-note highest-note))]))

(defn get-harmonic-data!
  [player-k bank]
  (let [data (bardo.live-state/get-player-data player-k)
        bank-data (get data bank)]
    (assoc bank-data :harmony (:harmony data))))

(comment
  (get-harmonic-data! :milo 0))

(defn get-harmony
  [harmony-k]
  (case harmony-k
    :meta-slendro-5 meta-slendro-5
    :meta-slendro-12 meta-slendro1
    :fib fib-21
    :meta-pelog-5 meta-pelog
    :meta-pelog-7 meta-pelog-7
    :meta-pelog-11 meta-pelog-11
    meta-slendro1))

(defn- get-rates-subset
  [rate-indexes rates]
  (keep
   #(nth rates % nil)
   rate-indexes))
#_(get-rates-subset #{0 1 2} [0 1 2])

(defn- get-active-synth-type
  [player-k state]
  (-> state :algo-2.2.9-clouds player-k :active-synth))

(defn- adjust-amp [db-delta amp]
  (* amp (o/db->amp db-delta)))
#_(adjust-amp 6 2)

(defn- granular-synth-amp-adjustment
  [config]
  (update config :amp (partial adjust-amp 9)))

(defn- mic-1-bank-0-aka-bell-sound-amp-adjustment
  [config]
  (if-not (and (= (-> config :buf :rec/meta :input-name)
                  "mic-1-bus")
               (= (-> config :buf :rec/meta :subsection)
                  0))
    config
    (update config :amp (partial adjust-amp -6))))

#_(defn start-clouds
    [{:keys [player-k]}]
    (clouds-refrain
     {:id (make-clouds-id player-k)
      :silence-thresh (o/db->amp -55)
      :durs-fn (fn [{:keys [index]}]
                 (let [state @live-state
                       rhythm (-> state :algo-2.2.9-clouds player-k :rhythm)]
                   (get-dur index
                            rhythm
                            (:lorentz state))))
      :buf-fn (fn [_]
                (let [lib-size (-> @live-state :algo-2.2.9-clouds player-k :sample-lib-size)
                      [k buf] (bardo.rec/get-buf
                               player-k
                               lib-size
                               (bardo.live-state/get-active-banks player-k))]
                  #_(println "get buf" k  (into {} buf))
                  buf))
      :rates-fn (fn [{:keys [index]}]
                  (let [{:keys [harmony harmonic-speed harmonic-range
                                rate-indexes ;; defines the number of voices to play, lorentz has 3 indexes so indexes can be a `set` of numbers 0 - 2
                                ]
                         :or {rate-indexes #{0 1 2}}}
                      ;; the bank has been hardcoded
                        (get-harmonic-data! player-k 0)]
                    (->> (lorentz-chord index
                                        (:lorentz @live-state)
                                        harmonic-speed
                                        (:low harmonic-range)
                                        (:high harmonic-range))
                         (#(rate-chord-seq (get-harmony harmony) [%]))
                         first
                         (get-rates-subset rate-indexes))))
      :amp-fn (fn [_]
               ;; the amp is adjusted at the call site of the synthdefs for different reasons:
               ;; 1. Milo's bank 1 is reserved for the bowed bell which is louder than other sounds
               ;; 2. The `granular` has less loudeness than the crystal synth
                (-> @live-state :algo-2.2.9-clouds player-k :amp (o/db->amp)))
      :on-play (fn [{:as config :keys [index buf rate]}]
                 (let [state @live-state
                       out (main-returns (case player-k
                                           :milo :percussion-processes
                                           :diego :guitar-processes))
                       synth-type (get-active-synth-type player-k state)
                      ;; TODO: update live state with event duration
                       synth (case synth-type
                               :crystal (let [dur (* rate (:duration buf))
                                              synth* (cristal-liquidizado (-> config
                                                                              mic-1-bank-0-aka-bell-sound-amp-adjustment
                                                                              (assoc :dur dur :out out)))]
                                          (bardo.synth-management/add-synth! synth* dur)
                                          synth*)
                               :granular (amanecer*guitar-clouds
                                          (-> config
                                              granular-synth-amp-adjustment
                                              (merge (get-envelope
                                                      index
                                                      (-> state :algo-2.2.9-clouds player-k :env)
                                                      (:lorentz state)))
                                              (assoc :out out)))
                               (amanecer*guitar-clouds
                                (-> config
                                    (merge (get-envelope
                                            index
                                            (-> state :algo-2.2.9-clouds player-k :env)
                                            (:lorentz state)))
                                    (assoc :out out))))]
                   (swap! bardo.rec/currently-playing-bufs update (:buf config) conj synth)))}))

(defn- clouds-buf
  [player bank]
  (let [lib-size (bardo.live-state/get-player-data player bank :sample-lib-size)
        [_k buf] (bardo.rec/get-buf
                  player
                  lib-size
                  #{bank})]
    buf))

(defn- clouds-rates
  [player index bank]
  (let [{:keys [harmony harmonic-speed harmonic-range
                harmonic-active-voices ;; defines the number of voices to play, lorentz has 3 indexes so indexes can be a `set` of numbers 0 - 2
                ]
         :or {harmonic-active-voices #{0 1 2}}} (get-harmonic-data! player bank)
        rates (->> (lorentz-chord index
                                  (:lorentz @live-state)
                                  harmonic-speed
                                  (:low harmonic-range)
                                  (:high harmonic-range))
                   (#(rate-chord-seq (get-harmony harmony) [%]))
                   first
                   (get-rates-subset harmonic-active-voices))]
    rates))

(defn- clouds-amp
  [player bank]
  ;; the amp is adjusted at the call site of the synthdefs for different reasons:
  ;; 1. Milo's bank 1 is reserved for the bowed bell which is louder than other sounds
  ;; TODO: remove this adjustment... where is it?
  ;; 2. The `granular` has less loudeness than the crystal synth

  (bardo.live-state/get-player-data player bank :amp))

(defn- clouds-out [player]
  (main-returns (case player
                  :milo :percussion-processes
                  :diego :guitar-processes)))

(defn- clouds-synth [player bank]
  (bardo.live-state/get-player-data player bank :active-synth))

(defn- clouds-pan [player bank]
  (let [{:as data :keys [active-panner]} (bardo.live-state/get-player-data player bank)]
    {:active-panner active-panner
     :panner-config (-> data :panner-configs active-panner)}))

(defn- clouds-filter [player bank]
  (let [{:as data :keys [active-filter]} (bardo.live-state/get-player-data player bank)]
    {:active-filter active-filter
     :filter-config (-> data :filter-configs active-filter)}))

(defn clouds-durs
  [player {:keys [index]}]
  (let [state @live-state
        rhythm (bardo.live-state/get-player-data player 0 :rhythm)]
    (get-dur index
             rhythm
             (:lorentz state))))

(defn clouds-synth-dur
  [player bank synth buf rate]
  (let [{:keys [max-dur%]} (bardo.live-state/get-player-data player bank)
        dur (:duration buf)]
    (case synth
      :crystal (let [max-dur (/ (* dur  max-dur%)
                                rate)
                     min-dur (min 0.5 max-dur)]
                 (first (linlin 0 1 min-dur max-dur [dur])))
      :granular (* 2 max-dur%))))

(defn make-voice-params
  [{:as synth-config :keys [synth player bank params]}
   rates]
  (->> rates
       (mapv (fn [rate]
               (let [d-level-weights {0.3 1}
                     room-weights {0.2 2, 2 1/2 4 1/2}
                     trig-rate (+ 90 (rand-int 20))
                     params* (assoc (:params synth-config)
                                    :dur (clouds-synth-dur player bank synth (:buf params) rate)
                                    :d-level (weighted d-level-weights)
                                    :rev-room (weighted room-weights)
                                    :trig-rate 100
                                    :grain-dur (/ 1 (/ trig-rate 2))
                                    :amp-lfo (rrange 0.1 0.4)
                                    :amp-lfo-min 0.95
                                    :lpf-max (rrange 2000 10000)
                                    :rate rate
                                    :interp (rand-nth [1 2 4]))]
                 (assoc synth-config :params params*))))))

(defn get-synth-data-vectors
  [player {:keys [index]}]
  (let [active-banks  (bardo.live-state/get-group-banks player)
        banks? (seq active-banks)
        bank (when banks? (rand-nth (into [] active-banks)))
        buf (when bank (clouds-buf player bank))
        synth (clouds-synth player bank)]
    (cond
      (not bank) (timbre/error "No bank selected, can't play cloud")
      (not buf) nil
      :else (let [rates (clouds-rates player index bank)
                  synth-config (merge
                                (clouds-pan player bank)
                                (clouds-filter player bank)
                                {:synth synth
                                 :player player
                                 :bank bank
                                 :params {:group (groups/mid)
                                          :buf buf
                                          :start 0
                                          :end 1
                                          :amp (clouds-amp player bank)
                                          :out-offset (clouds-out player)}})]
              (make-voice-params synth-config rates)))))

(defn clouds-on-event
  [player {refrain-event-data :data}]
  (doseq [data* (get-synth-data-vectors player refrain-event-data)]
    (play-synth data*)))

(comment
  (do ;; trigger clouds event on bank 0
    (bardo.live-state/toggle-active-bank! :milo 0 true)
    (clouds-on-event :milo {:index 0})))

;; FIXME: simplify workflow, the generation of params inside clouds-refrain2 seems somewhat redundant (as related to :get-param-data, probably merge both into on-play and exctract taht function so that i can be called via dispatch - for debugging purposes-)
(defn start-clouds
  [{:keys [player]}]
  (clouds-refrain2
   {:id (make-clouds-id player)
    :durs-fn (partial clouds-durs player)
    :on-event (partial clouds-on-event player)}))

(comment
  (-> @live-state :algo-2.2.9-clouds :milo)
  (swap! live-state assoc-in [:algo-2.2.9-clouds :milo :rhythm] :lor-0.1_2)
  (o/amp->db 0.0015420217847956035)
  (stop-clouds :milo)
  (start-clouds {:player :milo})
  (stop-clouds {:player :milo})

  (let [player-k :diego
        lib-size
        (-> @live-state :algo-2.2.9-clouds player-k :sample-lib-size)]
    (->> @rec/bufs
         (sort-by (comp :rec/time second))
         reverse
         (filter (fn [[k _]]
                   (str/includes? (name k)
                                  (if (= player-k :milo)
                                    "mic-"
                                    "guitar-"))))
         #_(take lib-size)
         #_(#(when (seq %) (rand-nth %)))))

  (->> @rec/bufs
       (sort-by (comp :rec/time second))
       reverse
       (filter (fn [[k _]]
                 (str/includes? (name k) "mic-")))))

(defn stop-clouds
  [{:keys [player]}]
  (gp/stop (make-clouds-id player)))

(defn start-gusano
  []
  (bardo.gusano/start))

(defn stop-gusano
  []
  (bardo.gusano/stop))

;;;;;;;;;;;;;;;;;
;; Event Handlers
;;;;;;;;;;;;;;;;;

(comment
  (timbre/set-level! :debug)
  (timbre/set-level! :info)
  (event-handler {:type :play-synth
                  :data {:synth :test
                         :params {:freq (rrange 100 300)}}}))
(defn event-handler
  [{:as _event
    :keys [type data]}]
  (case type
    :echo (timbre/info "Echoing:" data)
    :start-clouds (start-clouds data)
    :stop-clouds (stop-clouds data)
    :start-gusano (start-gusano)
    :stop-gusano (stop-gusano)
    :start-recording (start-recording data)
    :stop-recording (stop-recording data)
    :delete-bank-bufs (bardo.rec/delete-bank-bufs (:input-k data) (:active-bank data))
    ;; event for dev purpuses
    :dev/trigger-clouds-event (do ;; data {:bank int}
                                (bardo.live-state/toggle-active-bank! :milo (:bank data) true)
                                (clouds-on-event :milo {:data {:index 0}}))
    (timbre/error "[event-handler] No matching clause for `:type`:" type)))
