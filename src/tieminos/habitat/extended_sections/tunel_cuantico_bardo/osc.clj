(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc
  (:require
   [clojure.math :refer [round]]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.utils.core :refer [round2]]
   [org.httpkit.client :as http]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.init :as bardo.init]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-controls :as bardo.live-ctl]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state :refer [live-state]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.presets :as bardo.presets]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec :refer [delete-bank-bufs]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synth-management :refer [stop-long-running-synths!]]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.math.utils :refer [linlin]]
   [tieminos.osc.reaper :refer [reaeq-freq->lin]]
   [tieminos.osc.reaper :as reaper]
   [tieminos.utils :refer [cb-interpolate stop-all-interpolators! throttle]]))

(declare reaper-tracks* update-label)

(def default-rec-config
  {:on? true
   :pulse :dur
   :dur 0.5})

(defn toogle-rec [{:keys [input on?]}]
  (swap! live-state assoc-in [:rec input] (-> default-rec-config
                                              (merge (-> @live-state :rec input))
                                              (assoc :on? on?
                                                     :start-time (System/currentTimeMillis))))
  (if on?
    (bardo.live-ctl/start-recording {:input-k input})
    (bardo.live-ctl/stop-recording {:input-k input})))

(comment
  (toogle-rec {:input :guitar
               :on? true}))

(defn mute-input
  [input-k mute?]
  (if-let [track (reaper-tracks* input-k)]
    (do (osc/osc-send @habitat-osc/reaper-client
                      (format "/track/%s/mute" track)
                      (int mute?))
        (swap! live-state assoc-in
               [:rec input-k :muted?]
               (= 1 (int mute?))))
    (throw (ex-info "Unkown track to mute" {:input-k input-k}))))

(comment
  (-> @live-state)
  (mute-input :mic-1 1))

(defn switch-rec-durs [inputs dur]
  (let [dur* (case dur
               0 0.5
               1 1
               2 1.5
               3 2.5
               4 4
               5 10
               6 15
               7 20
               8 40
               9 60
               (throw (ex-info "Unkown rec dur" {:dur dur})))]
    (swap! live-state (fn [state]
                        (reduce (fn [state* input] (assoc-in state* [:rec input :dur] dur*))
                                state
                                inputs))))
  ;; we don't restart the recorder, but the recorder should receive a durs function instead that will deref the live-state somehow
  )

(defn set-active-recorded-bank
  [inputs bank]
  (swap! live-state (fn [state]
                      (reduce (fn [state* input] (assoc-in state* [:rec input :active-bank] bank))
                              state
                              inputs))))

(defn switch-rec-pulse [inputs pulse]
  (let [pulse (case pulse
                0 :dur
                1 :dur*2
                2 :rand-2
                3 :rand-4
                (throw (ex-info "Unkown rec pulse" {:pulse pulse})))]
    (swap! live-state (fn [state]
                        (reduce (fn [state* input] (assoc-in state* [:rec input :pulse] pulse))
                                state
                                inputs))))
  ;; we don't restart the recorder, but the recorder should receive a durs function instead that will deref the live-state somehow
  )

(def default-cloud-config
  {:sample-lib-size 1
   :env :lor-1_4
   :rhythm :lor-0.1_2
   :amp 0.7
   :reaper.send/reverb {:clean 0 :processes 0}
   :active-banks #{}
   :harmonic-speed 30
   :harmony :m-slendro
   :harmonic-range {:low -18 :high 18}})

(defn toggle-clouds
  [player on?]
  (swap! live-state
         assoc-in [:algo-2.2.9-clouds  player]
         (-> default-cloud-config
             (merge (-> @live-state :algo-2.2.9-clouds player))
             (assoc :on? on?)))
  (if on?
    (bardo.live-ctl/start-clouds player)
    (bardo.live-ctl/stop-clouds player)))

(comment
  (toggle-clouds :milo true))
(defn set-clouds-amp
  [player amp]
  (swap! live-state
         assoc-in [:algo-2.2.9-clouds player :amp]
         ;; TODO: lower extra vol
         (first (linlin 0 1 -36 36 [amp]))))

(comment
  (set-clouds-amp :diego 1))

(defn set-clouds-sample-lib-size
  [player opt-num]
  (let [env (case opt-num
              0 ##Inf
              1 1
              2 2
              3 3
              4 5
              5 8
              (throw (ex-info "Unkown clouds sample-lib-size" {:player player :opt-num opt-num})))]
    (swap! live-state assoc-in [:algo-2.2.9-clouds player :sample-lib-size] env)))
(defn set-active-synth
  [player opt-num]
  (let [synth-key (case opt-num
                    0 :granular
                    1 :crystal
                    (throw (ex-info "Unkown synth" {:player player :opt-num opt-num})))]
    (swap! live-state assoc-in [:algo-2.2.9-clouds player :active-synth] synth-key)))

(defn set-active-bank
  [{:keys [player bank on?]}]
  (swap! live-state update-in [:algo-2.2.9-clouds player :active-banks]
         (if on? set/union set/difference)
         #{(dec bank)}))

(defn set-clouds-env
  [player opt-num]
  (let [env (case opt-num
              0 :lor-1_4
              1 :lor-0.1_2
              2 :a-0.1_0.4*d-2*r-3
              3 :weights-largos
              (throw (ex-info "Unkown clouds env" {:player player :opt-num opt-num})))]
    (swap! live-state assoc-in [:algo-2.2.9-clouds player :env] env)))

(defn set-clouds-rhythm
  [player opt-num]
  (let [env (case opt-num
              0 :lor-0.1_2
              1 :lor-2_6
              2 :rand-0_10
              3 :rit
              4 :accel
              (throw (ex-info "Unkown clouds rhythm" {:player player :opt-num opt-num})))]
    (swap! live-state assoc-in [:algo-2.2.9-clouds player :rhythm] env)

    (when (-> @live-state :algo-2.2.9-clouds player :on?)
      (timbre/info "Restarting player-clouds" :player))))

(defn set-harmony
  [player opt-num]
  (let [harmony (nth [:fib
                      :meta-slendro-5
                      :meta-slendro-12
                      :meta-pelog-5
                      :meta-pelog-7
                      :meta-pelog-11]
                     opt-num
                     nil)]
    (if-not harmony
      (throw (ex-info "Unkown harmony:" {:player player :opt-num opt-num}))
      (swap! live-state assoc-in [:algo-2.2.9-clouds player :harmony] harmony))))

(defn set-harmonic-speed
  [player harmonic-speed]
  (let [speed (if (>= 0.2 harmonic-speed)
                (first (linlin 0 0.2 0 1 [harmonic-speed]))
                (round (first (linlin 0.2 1 1 70 [harmonic-speed]))))]
    (swap! live-state
           assoc-in [:algo-2.2.9-clouds player :harmonic-speed]
           speed)
    (update-label player :harmonic-speed (round2 2 speed))))

(defn- update-harmonic-range
  [hrange low? value]
  (assoc hrange
         (if low? :low :high)
         (round (first (linlin 0 1
                               (if low? -30 30)
                               (if low? 30 -30)
                               [value])))))

(defn set-harmonic-range
  [{:keys [player low? value]}]
  (let [path [:algo-2.2.9-clouds player :harmonic-range]
        state-data (swap! live-state
                          update-in
                          path
                          update-harmonic-range
                          low?
                          value)]

    (update-label player
                  (if low? :harmonic-lowest-note :harmonic-highest-note)
                  (get-in state-data (conj path (if low? :low :high))))))

(defn set-active-harmonic-voice
  [{:keys [player voice-index on?]}]
  (swap! live-state update-in [:algo-2.2.9-clouds player :harmonic-active-voices]
         (if on? set/union set/difference)
         #{voice-index}))

(defn set-rev-send
  [{:keys [player clean? value]}]
  (let [out (if clean? :clean :processes)]
    (swap! live-state
           assoc-in [:algo-2.2.9-clouds player :reaper.send/reverb out]
           value))

  (condp = [player clean?]
    [:diego true] (osc/osc-send @habitat-osc/reaper-client "/track/14/send/1/volume" (float value))
    [:diego false] (osc/osc-send @habitat-osc/reaper-client "/track/15/send/1/volume" (float value))
    [:milo true] (osc/osc-send @habitat-osc/reaper-client "/track/17/send/1/volume" (float value))
    [:milo false] (osc/osc-send @habitat-osc/reaper-client "/track/18/send/1/volume" (float value))))

(defn delete-bank
  "`k` is a key for where to find the active bank of the player"
  [inputs]
  (doseq [input-k inputs]
    (let [active-bank (-> @live-state :rec input-k (:active-bank 0))]
      (delete-bank-bufs input-k active-bank))))

(defn delete-all-banks
  [inputs]
  (doseq [bank (range 8)
          input-k inputs]
    (delete-bank-bufs input-k bank)))

(def default-gusano-config
  {:section 0
   :amp 0.5})

(defn toggle-gusano
  [on?]
  (swap! live-state
         assoc :gusano
         (-> default-gusano-config
             (merge (:gusano @live-state))
             (assoc :on? on?)))
  (if on?
    (bardo.live-ctl/start-gusano)
    (bardo.live-ctl/stop-gusano)))

(defn toggle-gusano-active-sources
  [src on?]
  (swap! live-state update-in [:gusano :sources]
         (fnil (if on? set/union set/difference) #{})
         #{src}))

(comment
  (reset! live-state {})

  (toggle-gusano-active-sources :milo true))

;; TODO: set the resulting values of gusano in the live-state just as with the other values
(defn set-gusano-rates
  [i]
  (swap! live-state assoc-in [:gusano :rates] i))

(defn set-gusano-rates-seq-speed
  [i]
  (swap! live-state assoc-in [:gusano :rates-seq-speed] i))

(defn set-gusano-amp
  [amp]
  (swap! live-state assoc-in [:gusano :amp] (first (linlin 0 1 0 1.5 [amp]))))

(defn set-gusano-period
  [i]
  (swap! live-state assoc-in [:gusano :period] i))

(defn set-gusano-durs
  [i]
  (swap! live-state assoc-in [:gusano :durs] i))

(defn set-gusano-grain-trig
  [x]
  (swap! live-state assoc-in [:gusano :grain-trig-rate] x))

(defn set-gusano-grain-dur
  [x]
  (swap! live-state assoc-in [:gusano :grain-dur] x))

(defn set-gusano-2nd-voice
  [x]
  (swap! live-state assoc-in [:gusano :second-voice-index] x))

(comment
  (require '[tieminos.network-utils :refer [get-local-host]])
  (def touchosc-fb-client (osc/osc-client (get-local-host) 16181)))

(do
  (def HACK-parse-path
    "Fixes a problem with the bank button which share the same address, so on feedback they all turn on or off."
    (let [indexed-paths #{"toggle-bank" "toggle-harmonic-voice"}]
      (memoize
       (fn [path]
         (let [spath  (str/split path #"/")]
           (if-not (seq (set/intersection (set spath) indexed-paths))
             path
             (->> (str/split path #"/")
                  (drop-last 1)
                  (str/join "/"))))))))
  #_(HACK-parse-path "/Diego/clouds-amp"))

;;;;;;;;;;;;;;;;;;;
;;  REAPER
;;;;;;;;;;;;;;;;;;;

(def ^:private reaper-tracks*
  ;; TODO: `:guitar` and `:mic-1` and `:mic-2` keys are used for other stuff, so can't namespace them right now as with the above. Ideally they could all be namespaced.
  {:guitar 3 ;; line-in
   :guitar/mic 4
   :mic-1 6
   :mic-2 7
   :guitar-input-track 3
   :guitar-clean-track 16
   :guitar-processes-track 17
   :percussion-processes-track 20
   :mixes-processes-2-track 22
   :eq-track 26})

(defn- reaper-tracks [k]
  (if-let [track-num (reaper-tracks* k)]
    track-num
    (throw (ex-info "Unknown track key" {:key k}))))

;; main eq controls
(def ^:private eq-param-defaults
  {:eq/loshelf.freq {:init-val 0 :path (format "/track/%s/fxeq/loshelf/freq" (reaper-tracks :eq-track))}
   :eq/loshelf.gain {:init-val 0 :path (format "/track/%s/fxeq/loshelf/gain" (reaper-tracks :eq-track))}
   :eq/hishelf.freq {:init-val 24000 :path (format "/track/%s/fxeq/hishelf/freq" (reaper-tracks :eq-track))}
   :eq/hishelf.gain {:init-val 0 :path (format "/track/%s/fxeq/hishelf/gain" (reaper-tracks :eq-track))}
   ;; a simple band pass assumed to have a gain > 0.5
   :eq/bell.freq {:init-val 1000 :path (format "/track/%s/fxeq/band/0/freq" (reaper-tracks :eq-track))}
   :eq/bell.gain {:init-val 0.5 :path (format "/track/%s/fxeq/band/0/gain" (reaper-tracks :eq-track))}
   ;; a simple band pass assumed to have a gain < 0.5
   :eq/notch.freq {:init-val 2000 :path (format "/track/%s/fxeq/band/1/freq" (reaper-tracks :eq-track))}
   :eq/notch.gain {:init-val 0.5 :path (format "/track/%s/fxeq/band/1/gain" (reaper-tracks :eq-track))}})

(defn interpolate-premaster-eq-band-vals
  ;; NOTE: for the ids to reference see the `eq-param-defaults` var.
  [{:keys [band freq gain dur-ms tick-ms]
    :or {tick-ms 200}}]
  (let [freq-id (keyword "eq" (str (name band) ".freq"))
        gain-id (keyword "eq" (str (name band) ".gain"))]
    (doseq [id [freq-id gain-id]]
      (when-not (eq-param-defaults id) (throw (ex-info "Unknown freq or gain id" {:band band :param-id id :known-param-ids (keys eq-param-defaults)}))))

    (cb-interpolate
     {:id freq-id
      :dur-ms dur-ms
      :tick-ms tick-ms
      :init-val (reaeq-freq->lin (:init-val (eq-param-defaults freq-id)))
      :target-val (reaeq-freq->lin freq)
      :cb (fn [{:keys [val]}]
            (osc/osc-send
             @habitat-osc/reaper-client
             (:path (eq-param-defaults freq-id))
             (float val)))})

    (cb-interpolate
     {:id gain-id
      :dur-ms dur-ms
      :tick-ms tick-ms
      :init-val (:init-val (eq-param-defaults gain-id))
      :target-val gain
      :cb (fn [{:keys [val]}]
            (osc/osc-send
             @habitat-osc/reaper-client
             (:path (eq-param-defaults gain-id))
             (float val)))})))

(comment
  (interpolate-premaster-eq-band-vals
   {:band :loshelf
    :freq 0
    :gain 0.5
    :dur-ms 5000})

  (interpolate-premaster-eq-band-vals
   {:band :hishelf
    :freq 24000
    :gain 0.02
    :dur-ms 5000})

  (interpolate-premaster-eq-band-vals
   {:band :notch
    :freq 3000
    :gain 0.02
    :dur-ms 2000})

  (interpolate-premaster-eq-band-vals
   {:band :bell
    :freq 1000
    :gain 1
    :dur-ms 5000})

  (stop-all-interpolators!)
  (osc/osc-send @habitat-osc/reaper-client (format "/track/%s/fxeq/band/0/freq" (reaper-tracks :eq-track)) (float 0.2))
  (osc/osc-send @habitat-osc/reaper-client (format "/track/%s/fxeq/loshelf/gain" (reaper-tracks :eq-track)) (float 0.2))
  (osc/osc-send @habitat-osc/reaper-client (format "/track/%s/fxeq/hishelf/freq/hz" (reaper-tracks :eq-track)) (float (+ 2000 (rand-int 2000)))))

(defn set-eq-interpolation-dur
  [opt-num]
  (if-let [dur (nth [5 10 20 40 60 120 180 240] opt-num nil)]
    (swap! live-state assoc-in [:main-eq :interpolation-dur-ms] (* dur 1000))
    (throw (ex-info "Unkown eq-interpolation-dur " {:opt-num opt-num}))))

(defn set-loshelf-freq
  [opt-num]
  (if-let [freq (nth [0 300 600 1000 2000] opt-num nil)]
    (let [gain 0.02]
      (swap! live-state assoc-in [:main-eq :loshelf :freq] freq)
      (swap! live-state assoc-in [:main-eq :loshelf :gain] gain)
      (interpolate-premaster-eq-band-vals
       {:band :loshelf
        :freq freq
        :gain gain
        :dur-ms (get-in @live-state [:main-eq :interpolation-dur-ms] 10000)}))
    (throw (ex-info "Unkown loshelf-freq " {:opt-num opt-num}))))

(defn manual-set-eq-param
  [param-key val]
  (osc/osc-send
   @habitat-osc/reaper-client
   (:path (eq-param-defaults param-key))
   (float val)))

(defn set-hishelf-freq
  [opt-num]
  (if-let [freq (nth [2000 3000 5000 10000 24000] opt-num nil)]
    (let [gain 0.02]
      (swap! live-state assoc-in [:main-eq :hishelf :freq] freq)
      (swap! live-state assoc-in [:main-eq :hishelf :gain] gain)
      (interpolate-premaster-eq-band-vals
       {:band :hishelf
        :freq freq
        :gain gain
        :dur-ms (get-in @live-state [:main-eq :interpolation-dur-ms] 10000)}))
    (throw (ex-info "Unkown hishelf-freq " {:opt-num opt-num}))))

(defn set-bell-freq
  [opt-num]
  (if-let [freq* (nth [:off 600 1000 2000 3000 5000 10000 15000] opt-num nil)]
    (let [freq (if (= :off freq*) 1000 freq*)
          gain (if (= :off freq*) 0.5 0.75)]
      (swap! live-state assoc-in [:main-eq :bell :freq] freq)
      (swap! live-state assoc-in [:main-eq :bell :gain] gain)
      (interpolate-premaster-eq-band-vals
       {:band :bell
        :freq freq
        :gain gain
        :dur-ms (get-in @live-state [:main-eq :interpolation-dur-ms] 10000)}))
    (throw (ex-info "Unkown hishelf-freq " {:opt-num opt-num}))))

(defn set-notch-freq
  [opt-num]
  (if-let [freq* (nth [:off 600 1000 2000 3000 5000 10000 15000] opt-num nil)]
    (let [freq (if (= :off freq*) 2000 freq*)
          gain (if (= :off freq*) 0.5 0.02)]
      (swap! live-state assoc-in [:main-eq :bell :freq] freq)
      (swap! live-state assoc-in [:main-eq :bell :gain] gain)
      (interpolate-premaster-eq-band-vals
       {:band :notch
        :freq freq
        :gain gain
        :dur-ms (get-in @live-state [:main-eq :interpolation-dur-ms] 10000)}))
    (throw (ex-info "Unkown hishelf-freq " {:opt-num opt-num}))))

(defn set-flat-eq
  []
  (let [dur-ms (get-in @live-state [:main-eq :interpolation-dur-ms] 10000)]
    (interpolate-premaster-eq-band-vals
     {:band :loshelf
      :freq 0
      :gain 0.5
      :dur-ms dur-ms})
    (interpolate-premaster-eq-band-vals
     {:band :hishelf
      :freq 24000
      :gain 0.5
      :dur-ms dur-ms})
    (interpolate-premaster-eq-band-vals
     {:band :bell
      :freq 1000
      :gain 0.5
      :dur-ms dur-ms})
    (interpolate-premaster-eq-band-vals
     {:band :notch
      :freq 2000
      :gain 0.5
      :dur-ms dur-ms})))

(defn make-volume-booster
  "Returns a function that takes a `level-index` and performs a fade on
  a `Volume Adjustment` Reaper plugin at the specified `track` and `amp-fx-position`."
  [{:keys [interpolator-id
           interpolation-time-ms
           track
           amp-fx-position
           level-offset]
    :or {interpolation-time-ms 1500
         level-offset 0}}]
  (let [initial-level-index (atom 0)]
    (fn [level-index]
      (let [level (+ 0.5 level-offset (* level-index 0.02))]
        (cb-interpolate
         {:id interpolator-id
          :dur-ms (* interpolation-time-ms
                      ;; increase dur-ms based on difference of level-indexes
                     (max 1 (abs (- level-index
                                    @initial-level-index))))
          :tick-ms 100
          :init-val 0.5
          :target-val level
          :cb (fn [{:keys [val]}] (reaper/set-fx track
                                                 amp-fx-position
                                                 1 val))})
        (reset! initial-level-index level-index)))))

;;;;;;;;;;;
;;; Guitar
;;;;;;;;;;;

(def guitar-input-amp-boost
  (make-volume-booster {:interpolator-id ::guitar-boost
                        :track (reaper-tracks :guitar-input-track)
                        :amp-fx-position 2}))

(comment
  (reaper/set-fx (reaper-tracks :guitar-input-track) 2 1 1)
  (guitar-input-amp-boost 0))

;;;;;;;;;;
;; Percussion
;;;;;;;;;;

(defn- set-track-volume
  [track volume]
  (osc/osc-send @habitat-osc/reaper-client
                (format "/track/%s/volume" track)
                (float volume)))

(defn- interpolate-track-volume
  ([reaper-track-kw target-volume] (interpolate-track-volume reaper-track-kw target-volume 0))
  ([reaper-track-kw target-volume init-val]
   (cb-interpolate
    {:id (keyword "volume" (name reaper-track-kw))
     :dur-ms 5000
     :tick-ms 100
     :init-val init-val
     :target-val target-volume
     :cb (fn [{:keys [val]}]
           (set-track-volume (reaper-tracks reaper-track-kw) val))})))

(def ^:private set-track-volume2
  (throttle #'interpolate-track-volume 200))

(def ^:private perc-processes-amp-boost
  (make-volume-booster {:interpolator-id ::percussion-processes-boost
                        :level-offset (* -3 0.02) ;; index 3 = 0db boost
                        :track (reaper-tracks :percussion-processes-track)
                        :amp-fx-position 2}))

(comment

  (set-track-volume2 :percussion-processes-track 1))

;;;;;;;;;;;;;;;
;;; Recording
;;;;;;;;;;;;;;;
(def ^:private automated-tracks
  (mapv reaper-tracks
        [:guitar-input-track
         :guitar-clean-track
         :guitar-processes-track
         :percussion-processes-track
         :mixes-processes-2-track
         :eq-track]))

(defn reaper-rec!
  []
  (doseq [track automated-tracks]
    (reaper/set-autowrite track))
  (reaper/rec))

(defn reaper-stop!
  []
  ;; set OSC EQ envelope to write
  (reaper/stop)
  (doseq [track automated-tracks]
    (reaper/set-autoread track)))

(def ^:private excluded-paths #{"/presets/load"})

(defn- update-clients
  [clients path args]
  (doseq [client (map second clients)]
    (when-not (excluded-paths path)
      (apply osc/osc-send client path args))))

(comment
  (update-clients @habitat-osc/receiver-clients "/Milo/harmonic-speed-label" [(str (round2 2 0.3455))]))

(defn- osc-responder
  [{:keys [path args] :as msg}]
  (let [HACKED-path (HACK-parse-path path) ;; FIXME: there should be a more elegant way to handle this (see fn definition).
        args-map (habitat-osc/args->map args)
        press? (= 1.0 (first args))]
    (case HACKED-path
      "/Milo/rec-mic-1-btn" (toogle-rec {:input :mic-1 :on? press? :dur (-> @live-state :rec :mic-1 :dur (or 0.5))})
      "/Milo/rec-mic-2-btn" (toogle-rec {:input :mic-2 :on? press? :dur (-> @live-state :rec :mic-2 :dur (or 0.5))})
      "/Milo/mute-mic-1"    (mute-input :mic-1 (first args))
      "/Milo/mute-mic-2"    (mute-input :mic-2 (first args))
      "/Milo/rec-durs-radio" (switch-rec-durs [:mic-1 :mic-2] (first args))
      "/Milo/rec-pulse-radio" (switch-rec-pulse [:mic-1 :mic-2] (first args))
      "/Milo/clouds-active-btn" (toggle-clouds :milo press?)
      "/Milo/clouds-amp" (set-clouds-amp :milo (first args))
      "/Milo/clouds-env-radio" (set-clouds-env :milo (first args))
      "/Milo/clouds-rhythm-radio" (set-clouds-rhythm :milo (first args))
      "/Milo/clouds-sample-lib-size-radio" (set-clouds-sample-lib-size :milo (first args))
      "/Milo/bank-rec-radio" (set-active-recorded-bank [:mic-1 :mic-2] (first args))
      "/Milo/bank-delete-btn" (when press? (delete-bank [:mic-1 :mic-2]))
      "/Milo/bank-delete-all-btn" (when press? (delete-all-banks [:mic-1 :mic-2]))
      "/Milo/toggle-bank" (set-active-bank {:player :milo :bank (:index args-map) :on? (== 1 (:on args-map))})
      "/Milo/synth-radio" (set-active-synth :milo (first args))
      "/Milo/harmony-radio" (set-harmony :milo (first args))
      "/Milo/harmonic-speed" (set-harmonic-speed :milo (first args))
      "/Milo/harmonic-lowest-note" (set-harmonic-range {:player :milo :low? true :value (first args)})
      "/Milo/harmonic-highest-note" (set-harmonic-range {:player :milo :low?  false :value (first args)})
      "/Milo/toggle-harmonic-voice" (set-active-harmonic-voice {:player :milo :voice-index (:index args-map) :on? (== 1 (:on args-map))})
      "/Milo/rev-send-clean" (set-rev-send {:player :milo :clean? true :value (first args)})
      "/Milo/rev-send-process" (set-rev-send {:player :milo :clean? false :value (first args)})
      "/Milo/processed-master" (set-track-volume2 :percussion-processes-track (first args))
      "/Milo/processes-amp-boost" (perc-processes-amp-boost (first args))
      "/Diego/rec-guitar-btn" (toogle-rec {:input :guitar :on? press? :dur (-> @live-state :rec :mic-1 :dur (or 0.5))})
      "/Diego/rec-durs-radio" (switch-rec-durs [:guitar] (first args))
      "/Diego/rec-pulse-radio" (switch-rec-pulse [:guitar] (first args))
      "/Diego/mute-line-in"    (mute-input :guitar (first args))
      "/Diego/mute-mic"    (mute-input :guitar/mic (first args))
      "/Diego/clouds-active-btn" (toggle-clouds :diego press?)
      "/Diego/clouds-amp" (set-clouds-amp :diego (first args))
      "/Diego/clouds-env-radio" (set-clouds-env :diego (first args))
      "/Diego/clouds-rhythm-radio" (set-clouds-rhythm :diego (first args))
      "/Diego/clouds-sample-lib-size-radio" (set-clouds-sample-lib-size :diego (first args))
      "/Diego/bank-rec-radio" (set-active-recorded-bank [:guitar] (first args))
      "/Diego/toggle-bank" (set-active-bank {:player :diego :bank (:index args-map) :on? (== 1 (:on args-map))})
      "/Diego/bank-delete-btn" (when press? (delete-bank [:guitar]))
      "/Diego/bank-delete-all-btn" (when press? (delete-all-banks [:guitar]))
      "/Diego/synth-radio" (set-active-synth :diego (first args))
      "/Diego/harmony-radio" (set-harmony :diego (first args))
      "/Diego/harmonic-speed" (set-harmonic-speed :diego (first args))
      "/Diego/harmonic-lowest-note" (set-harmonic-range {:player :diego :low? true :value (first args)})
      "/Diego/harmonic-highest-note" (set-harmonic-range {:player :diego :low?  false :value (first args)})
      "/Diego/toggle-harmonic-voice" (set-active-harmonic-voice {:player :diego :voice-index (:index args-map) :on? (== 1 (:on args-map))})
      "/Diego/rev-send-clean" (set-rev-send {:player :diego :clean? true :value (first args)})
      "/Diego/rev-send-process" (set-rev-send {:player :diego :clean? false :value (first args)})
      "/Diego/input-amp-boost" (guitar-input-amp-boost (first args))
      "/Diego/clean-master" (set-track-volume2 :guitar-clean-track (first args))
      "/Diego/processed-master" (set-track-volume2 :guitar-processes-track (first args))
      ;; synth management
      "/stop-long-running-synths" (when press? (stop-long-running-synths! (* 20 1000)))
      ;; gusano
      "/gusano/gusano-active-btn" (toggle-gusano press?)
      "/gusano/gusano-active-milo-src-btn" (toggle-gusano-active-sources :milo press?)
      "/gusano/gusano-active-diego-src-btn" (toggle-gusano-active-sources :diego press?)
      "/gusano/rates" (set-gusano-rates (first args))
      "/gusano/rates-seq-speed" (set-gusano-rates-seq-speed (first args))
      "/gusano/amp" (set-gusano-amp (first args))
      "/gusano/period" (set-gusano-period (first args))
      "/gusano/durs" (set-gusano-durs (first args))
      "/gusano/grain-trig" (set-gusano-grain-trig (first args))
      "/gusano/grain-durs" (set-gusano-grain-dur (first args))
      "/gusano/2nd-voice" (set-gusano-2nd-voice (first args))
      ;; presets
      "/save-preset" (when press? (bardo.presets/save-preset!))
      ;; "/presets/load" (bardo.presets/load-preset! internal-client @habitat-osc/receiver-clients (first args))
      ;; eq
      "/EQ/durs-radio"  (set-eq-interpolation-dur (first args))
      "/EQ/loshelf-radio" (set-loshelf-freq (first args))
      "/EQ/hishelf-radio" (set-hishelf-freq (first args))
      "/EQ/notch-radio" (set-notch-freq (first args))
      "/EQ/bell-radio" (set-bell-freq (first args))
      "/EQ/flat-eq" (set-flat-eq)
      ;; ;; eq manual
      "/EQ/loshelf-freq-knob" (manual-set-eq-param :eq/loshelf.freq (first args))
      "/EQ/loshelf-gain-knob" (manual-set-eq-param :eq/loshelf.gain (first args))
      "/EQ/hishelf-freq-knob" (manual-set-eq-param :eq/hishelf.freq (first args))
      "/EQ/hishelf-gain-knob" (manual-set-eq-param :eq/hishelf.gain (first args))
      "/EQ/bell-freq-knob" (manual-set-eq-param :eq/bell.freq (first args))
      "/EQ/bell-gain-knob" (manual-set-eq-param :eq/bell.gain (first args))
      "/EQ/notch-freq-knob" (manual-set-eq-param :eq/notch.freq (first args))
      "/EQ/notch-gain-knob" (manual-set-eq-param :eq/notch.gain (first args))

      ;; main controls
      "/System/rec-start" (when press? (reaper-rec!))
      "/System/rec-stop" (when press? (reaper-stop!))
      "/System/init" (when press? (bardo.init/all!))
      "/System/voces-master" (set-track-volume2 :mixes-processes-2-track (first args) reaper/zero-db)
      (timbre/warn "Unknown path for message: " HACKED-path msg args-map))

    ;; Save last update to touch-osc-state
    (swap! bardo.live-state/touch-osc-state assoc path args)

    ;; send update to other clients
    (update-clients @habitat-osc/receiver-clients path args)))

(defn init!
  "`clients` is a vector of [host port]"
  [clients]
  (habitat-osc/init)
  (reaper/init)
  (habitat-osc/make-reaper-osc-client)
  (habitat-osc/make-receiver-clients clients)
  (let [internal-client (habitat-osc/make-internal-osc-client)]
    (habitat-osc/responder #'osc-responder)))

(defn ping []
  (http/get "http://localhost:5000/ping"
            (fn [{:keys [status headers body error]}] ;; asynchronous response handling
              (if error
                (println "Failed, exception is " error)
                (println "Async HTTP POST: " status)))))

(comment
  (-> @live-state)
  (get-local-host)
  (init! [["127.0.0.1" 16181]
          #_["192.168.0.100" 16181]
          ["192.168.0.102" 16180]])
  (ping))

(defn post [endpoint body & {:keys [debug?]}]
  (http/post (str "http://localhost:5000" endpoint)
             {:body (pr-str body)}
             (fn [{:keys [status headers body error]}] ;; asynchronous response handling
               (if error
                 (println "Failed, exception is " error endpoint)
                 (when debug? (println "Async HTTP POST: " status))))))

(def throttled-post
  (throttle (fn [state] (post "/gusano-cuantico-bardo" state))
            50))

(defn post-live-state-to-ui!
  [& {:keys [print-instead?]}]
  (add-watch bardo.live-state/live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (if print-instead?
                 (println new-value)
                 (throttled-post (dissoc new-value :lorentz))))))

(def default-touch-osc-state
  (->> '{"/Diego/bank-rec-radio" (0),
         "/Diego/clouds-active-btn" (0.0), ;; NOTE: will cause log "Could not find refrain with id: :bardo.clouds/diego"
         "/Diego/clouds-amp" (0.0),
         "/Diego/clouds-env-radio" (0),
         "/Diego/clouds-rhythm-radio" (0),
         "/Diego/clouds-sample-lib-size-radio" (0),
         "/Diego/harmonic-highest-note" (0.5),
         "/Diego/harmonic-lowest-note" (0.5),
         "/Diego/harmonic-speed" (0.2),
         "/Diego/harmony-radio" (0),
         "/Diego/input-amp-boost" (0),
         "/Diego/clean-master" (0.0),
         "/Diego/processed-master" (0.0),
         "/Diego/rec-durs-radio" (0),
         "/Diego/rec-pulse-radio" (0),
         "/Diego/rev-send-clean" (0.0),
         "/Diego/rev-send-process" (0.0),
         "/Diego/synth-radio" (0),
         "/Diego/toggle-bank/1" ("on" 0.0 "index" 1),
         "/Diego/toggle-harmonic-voice/0" ("on" 1 "index" 0),
         "/Diego/toggle-harmonic-voice/1" ("on" 1 "index" 1),
         "/Diego/toggle-harmonic-voice/2" ("on" 1 "index" 2),
         "/EQ/bell-radio" (0),
         "/EQ/durs-radio" (0),
         "/EQ/flat-eq" (0.0),
         "/EQ/hishelf-radio" (0),
         "/EQ/loshelf-radio" (0),
         "/EQ/notch-radio" (0),
         "/gusano/amp" (0.0),
         "/gusano/durs" (0),
         "/gusano/grain-durs" (0.0),
         "/gusano/grain-trig" (0.0),
         "/gusano/period" (0),
         "/gusano/rates" (0),
         "/Milo/bank-rec-radio" (0),
         "/Milo/clouds-active-btn" (0.0), ;; NOTE: will cause log "Could not find refrain with id: :bardo.clouds/milo"
         "/Milo/clouds-amp" (0.0),
         "/Milo/clouds-env-radio" (0),
         "/Milo/clouds-rhythm-radio" (0),
         "/Milo/clouds-sample-lib-size-radio" (0),
         "/Milo/harmonic-highest-note" (0.5),
         "/Milo/harmonic-lowest-note" (0.5),
         "/Milo/harmonic-speed" (0.2),
         "/Milo/harmony-radio" (0),
         "/Milo/processed-master" (0.0),
         "/Milo/processes-amp-boost" (3),
         "/Milo/rec-durs-radio" (0),
         "/Milo/rec-pulse-radio" (0),
         "/Milo/rev-send-clean" (0.0),
         "/Milo/rev-send-process" (0.0),
         "/Milo/synth-radio" (0),
         "/Milo/toggle-bank/1" ("on" 0.0 "index" 1)
         "/Milo/toggle-harmonic-voice/0" ("on" 1 "index" 0),
         "/Milo/toggle-harmonic-voice/1" ("on" 1 "index" 1),
         "/Milo/toggle-harmonic-voice/2" ("on" 1 "index" 2)
         "/System/voces-master" (reaper/zero-db)}
       (map (fn [[k v]] [k (map #(cond
                                   (symbol? %) (eval %) ;; NOTE this may cause trouble
                                   (not (number? %)) %
                                   (float? %) (float %)
                                   :else (int %)) v)]))
       (into {})))
(comment
  (-> default-touch-osc-state))
(defn- get-label-path
  [player label-key]
  (case [player label-key]
    ;; other cases to come
    (format "/%s/%s"
            (case player :milo "Milo" :diego "Diego")
            (str (name label-key) "-label"))))

(defn- update-label
  [player label-key value]
  (let [path (get-label-path player label-key)]
    (update-clients @habitat-osc/receiver-clients path [(str value)])))

(defn reset-default-state!
  []
  (doseq [[path args] default-touch-osc-state]
    (osc-responder {:path path :args args})))

(comment

  (->> @live-state)
  (reset-default-state!)
  (reset! live-state {})
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (throttled-post (dissoc new-value :lorentz))))
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (println new-value))))
