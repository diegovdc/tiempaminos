(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-controls
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.math :refer [round]]
   [clojure.string :as str]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.habitat.extended-sections.harmonies.chords
    :refer [get-harmony rate-chord-seq]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.gusanos.core
    :as bardo.gusano]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state
    :as bardo.live-state
    :refer [live-state]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc
    :as bardo.osc]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-helpers
    :as bardo.osc-helpers]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec
    :as bardo.rec]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.samplers
    :refer [play-synth]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.recording :as rec]
   [tieminos.habitat.routing :refer [inputs main-returns]]
   [tieminos.math.bezier-samples :as bzs]
   [tieminos.math.utils :refer [linlin]]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

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

(defn send-tick
  [input-k val]
  (bardo.osc-helpers/send-osc-msg (format "/Rec/%s-tick" (name input-k))
                                  (str val)))

(defn tick-countdown
  [rec-dur input-k]
  (let [tick 0.5]
    (rain.v2/ref-rain
     :id (make-rec-id (str (name input-k) "-countdown"))
     :durs (repeat (/ rec-dur tick) tick)
     :loop? false
     :on-event (rain.v2/on-event
                (send-tick input-k (- rec-dur (* i tick)))))))

(defn start-recording
  [{:keys [input-k]}]
  (timbre/info "starting rec on" input-k)
  (if-let [input-bus (-> @inputs input-k :bus)]
    (bardo.rec/start-rec-loop!
     {:id (make-rec-id input-k)
      :input-k input-k
      :input-bus input-bus
      :rec-dur-fn (fn [_] (-> @live-state :rec input-k :dur))
      :rec-pulse (fn [_] (-> @live-state :rec input-k get-rec-pulse))
       ;; :print-info? true
      :on-rec-start (fn [_rec-config]
                      (swap! live-state
                             assoc-in
                             [:rec input-k :last-rec-timestamp]
                             (o/now))
                      (tick-countdown (-> @live-state :rec input-k :dur)
                                      input-k))
      :on-rec-end (fn [_] (send-tick input-k ""))})
    (timbre/error "No input bus for key:" input-k)))

(comment
  (reset! rec/recording? {})
  (start-recording {:input-k :mic-1})
  (->> @rec/bufs
       vals
       (map :duration)
       frequencies))

(defn stop-recording
  [{:keys [input-k]}]
  (timbre/info "Stopping rec on:" input-k)
  (bardo.rec/stop-rec-loop! (make-rec-id input-k)))

;;;;;;;;;;;;;;;;;;
;; Clouds
;;;;;;;;;;;;;;;;;;

(defn- make-clouds-id
  ([player-k bank] (keyword "bardo.clouds" (str (name player-k) bank)))
  ([player-k] (keyword "bardo.clouds" (name player-k))))

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
  [#_:a (lorentz/bound (lorentz (* 50 index)) :x min* max*)
   #_:d (lorentz/bound (lorentz (* 50 index)) :y min* max*)
   #_:r (lorentz/bound (lorentz (* 50 index)) :z min* max*)])

(defn get-envelope [index env-k lorentz]
  (case env-k
    :lor-1_4 (get-lorentz-envelope index lorentz 1 4)
    :lor-0.1_2 (get-lorentz-envelope index lorentz 0.1 2)
    :a-0.1_0.4*d-2*r-3 [#_:a (rrange 0.1 0.4) #_:d 2 #_:r 3]
    :weights-largos [#_:a (weighted {10 1
                                     15 0.3})
                     #_:d (weighted {40 1
                                     30 0.3})
                     #_:r (weighted {10 1
                                     20 0.3})]
    (do
      (timbre/error (ex-info "Unknown envelope key, using default"
                             {:env-k env-k}))
      [3 3 2])))

(defn lorentz-chord
  [index lorentz lowest-note highest-note]
  [(round (lorentz/bound (lorentz index) :x lowest-note highest-note))
   (round (lorentz/bound (lorentz index) :y lowest-note highest-note))
   (round (lorentz/bound (lorentz index) :z lowest-note highest-note))])

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
    (when-not buf
      (timbre/warn "No buffer for bank" bank))
    buf))

(defonce ^:private lorentz-chord-indexes (atom {}))

(defn- get-next-lorentz-chord-index!
  [refrain-id harmonic-speed]
  (-> (swap! lorentz-chord-indexes update refrain-id (fnil + 0)  harmonic-speed)
      (get refrain-id)))

(defn- clouds-voice-config
  [player refrain-id bank]
  (let [{:keys [harmony harmonic-speed harmonic-range
                harmonic-active-voices ;; defines the number of voices to play, lorentz has 3 indexes so indexes can be a `set` of numbers 0 - 2
                harmonic-convergence-point]
         :or {harmonic-active-voices #{0 1 2}
              harmonic-convergence-point 0}} (bardo.live-state/get-harmonic-data! player bank)
        index (get-next-lorentz-chord-index! refrain-id harmonic-speed)
        rates (->> (lorentz-chord index
                                  (:lorentz @live-state)
                                  (:low harmonic-range)
                                  (:high harmonic-range))
                   (#(rate-chord-seq (get-harmony harmony) [%]))
                   first
                   (get-rates-subset harmonic-active-voices))]
    {:rates rates
     :convergence-point harmonic-convergence-point}))

(defn- clouds-amp
  [player bank]
  ;; the amp is adjusted at the call site of the synthdefs for different reasons:
  ;; 1. Milo's bank 1 is reserved for the bowed bell which is louder than other sounds
  ;; TODO: remove this adjustment... where is it?
  ;; 2. The `granular` has less loudeness than the crystal synth

  (o/db->amp (bardo.live-state/get-player-data player bank :amp)))

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
  [player bank {:keys [index]}]
  (let [state @live-state
        rhythm (bardo.live-state/get-player-data player bank :rhythm)]
    (get-dur index
             rhythm
             (:lorentz state))))

(defn- ranged-dur%
  [buf-dur rate max-dur%]
  (let [max-dur (/ buf-dur rate)
        min-dur 0.01
        dur-amp (first (linlin 0 1 0.001 1 [max-dur%]))]
    (max min-dur (* max-dur dur-amp))))

(defn- ranged-dur-abs
  [buf-dur rate max-dur%]
  (let [buf-dur* (/ buf-dur rate)
        max-dur-abs (if (> max-dur% 0.95)
                      buf-dur*
                      (first (linlin 0 1 0.01 20 [max-dur%])))]
    (min buf-dur* max-dur-abs)))
#_(ranged-dur 4 1/2 0.1)

(defn clouds-synth-dur
  [player bank synth buf rate]
  (let [{:keys [max-dur%]} (bardo.live-state/get-player-data player bank)
        dur (:duration buf)]
    (timbre/spy :debug "clouds-synth-dur"
                (case synth
                  :crystal (ranged-dur-abs dur rate max-dur%)
                  :granular (* 2 max-dur%)))))

(defn clouds-start-pos
  [dur {:as _buf
        :keys [rate n-samples duration]}]

  (timbre/spy :debug "SP"
              (if (< dur duration)
                0
                (rand-int n-samples))))

(defn- clouds-start-delays
  [convergence-point-% buf rates]
  (let [original-dur (:duration buf)
        rate-durs-pair (->> rates
                            (mapv #(vector % (/ original-dur %))))
        max-dur (->> rate-durs-pair (mapv second) sort last)]
    (->> rate-durs-pair
         (mapv (fn [[rate dur]]
                 (let [max-delay (- max-dur dur)]
                   [rate (* max-delay convergence-point-%)])))
         (into {}))))

#_(clouds-start-delays 0.5 {:duration 0.45} [3/16 7/32 3/8])

(defn make-voice-params
  [{:as synth-config :keys [synth player index bank params]}
   {:as voice-config :keys [rates convergence-point]}]
  (let [buf (:buf params)
        rate->start-delay (clouds-start-delays convergence-point buf rates)]
    (->> rates
         (mapv (fn [rate]
                 (let [d-level-weights {0.3 1}
                       room-weights {0.2 2, 2 1/2 4 1/2}
                       trig-rate (+ 90 (rand-int 20))

                       dur (clouds-synth-dur player bank synth buf rate)
                       start-pos (clouds-start-pos dur buf)
                       granular? (= :granular synth)
                       params* (-> synth-config
                                   :params
                                   (assoc :dur dur
                                          :start-pos start-pos
                                          :rate rate)
                                   (cond->
                                    granular?
                                     (assoc
                                      :grain-dur (/ 1 (/ trig-rate 2))
                                      :trig-rate 100
                                      :interp (rand-nth [1 2 4])
                                      :amp (adjust-amp 9 (:amp params))
                                      :amp-lfo (rrange 0.1 0.4)
                                      :amp-lfo-min 0.95
                                      :lpf-max (rrange 2000 10000)
                                      :amp-env-durations (get-envelope
                                                          index
                                                          (bardo.live-state/get-player-data player bank :env)
                                                          (:lorentz @bardo.live-state/live-state))
                                      :rev-room (weighted room-weights))))]
                   (assoc synth-config
                          :params params*
                          :event/start-delay (if granular?
                                               0
                                               (get rate->start-delay rate 0)))))))))
(comment
  (bardo.live-state/get-active-independent-banks :milo))
(defn get-synth-data-vectors
  [player independent-bank
   {:keys [index id] :as _refrain-config}]
  (let [active-banks (if independent-bank
                       #{independent-bank}
                       (bardo.live-state/get-active-group-banks player))
        banks? (seq active-banks)
        bank (when banks? (rand-nth (into [] active-banks)))
        buf (when bank (clouds-buf player bank))
        synth (clouds-synth player bank)]
    (cond
      (not bank) (timbre/error "No bank selected, can't play cloud")
      (not buf) nil
      :else (let [voice-config (clouds-voice-config player id bank)
                  synth-config (merge
                                (clouds-pan player bank)
                                (clouds-filter player bank)
                                {:synth synth
                                 :player player
                                 :bank bank
                                 :index index
                                 :params {:group (groups/mid)
                                          :buf buf
                                          :start 0
                                          :end 1
                                          :amp (clouds-amp player bank)
                                          :out-offset (clouds-out player)}})]
              (make-voice-params synth-config voice-config)))))

(comment
  (bardo.live-state/toggle-active-bank! :milo 0 true)
  (bardo.live-state/get-player-data :milo)
  (get-synth-data-vectors :milo {:index 0}))

(defonce delay-refrains (atom {}))

(defn- assoc-delay-refrain
  [path]
  (swap! delay-refrains assoc-in path true))

(defn- dissoc-delay-refrain
  [path]
  (let [[parent id] path]
    (swap! delay-refrains update parent dissoc id)))

(defn- stop-delay-refrains
  [parent-id]
  (let [delays (get parent-id @delay-refrains)]
    (doseq [[id] delays] (rain.v2/stop id))
    (swap! delay-refrains dissoc parent-id)))

(defn- clouds-delay-refrain
  [refrain-event-data start-delay f]
  (let [refrain-parent-id (-> refrain-event-data :refrain/config :id)
        id (str (random-uuid))
        path [refrain-parent-id id]]
    (assoc-delay-refrain path)
    (rain.v2/ref-rain
     :id id
     :durs [start-delay 1]
     :loop? false
     :on-event (rain.v2/on-event
                (when (= 1 i)
                  (f)
                  (dissoc-delay-refrain path))))))

(comment
  (require '[time-time.dynacan.players.refrain.v2 :as rain.v2])
  (rain.v2/ref-rain
   :id :test
   :durs [5 1]
   :on-event (rain.v2/on-event
              (println i)))
  (rain.v2/stop))

(defn clouds-on-event
  [player independent-bank {refrain-event-data :voice}]
  (doseq [{:keys [event/start-delay] :as data} (get-synth-data-vectors
                                                player
                                                independent-bank
                                                refrain-event-data)]
    (let [f #(play-synth data)]
      (if (> start-delay 0)
        (clouds-delay-refrain refrain-event-data start-delay f)
        (f)))))

(comment
  (do ;; trigger clouds event on bank 0
    (bardo.live-state/toggle-active-bank! :milo 0 true)
    (clouds-on-event :milo {:index 0})))

;; FIXME: simplify workflow, the generation of params inside clouds-refrain2 seems somewhat redundant (as related to :get-param-data, probably merge both into on-play and exctract taht function so that i can be called via dispatch - for debugging purposes-)
(defn start-clouds
  [{:keys [player bank independent?]}]
  (rain.v2/ref-rain
   {:id  (if independent?
           (make-clouds-id player bank)
           (make-clouds-id player))
    :durs (partial clouds-durs player bank)
    :on-event (partial clouds-on-event player bank)
    :on-stop (fn [{:keys [id]}] (stop-delay-refrains id))}))

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
  [{:keys [player bank independent?]}]
  (if independent?
    (rain.v2/stop (make-clouds-id player bank))
    (when-not (seq (bardo.live-state/get-active-group-banks player))
      (rain.v2/stop (make-clouds-id player)))))

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
    :bardo.event/delete-bank-bufs (bardo.rec/delete-bank-bufs data)
    :bardo.event/bufs-counted (bardo.osc/update-bufs-count data)
    ;; event for dev purpuses
    :dev/trigger-clouds-event (do ;; data {:bank int}
                                (bardo.live-state/toggle-active-bank! :milo (:bank data) true)
                                (clouds-on-event :milo {:data {:index 0}}))
    (timbre/error "[event-handler] No matching clause for `:type`:" type)))
