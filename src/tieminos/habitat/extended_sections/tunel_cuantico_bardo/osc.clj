(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc
  (:require
   [clojure.math :refer [ceil]]
   [clojure.pprint :as pprint]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.utils.core :refer [round2]]
   [overtone.osc :as osc]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.config
    :as bardo.config]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state
    :as bardo.live-state
    :refer [activate-processor-preset! delete-all-banks delete-bank
            inc-gusano-rate-index! live-state mute-input
            save-touchosc-synth-param set-active-bank
            set-active-harmonic-voice set-active-recorded-bank set-clouds-amp
            set-clouds-env set-clouds-rhythm set-clouds-sample-lib-size
            set-filter-index set-filter-param set-gusano-2nd-voice
            set-gusano-amp set-gusano-durs set-gusano-grain-dur
            set-gusano-grain-trig set-gusano-period set-gusano-rates-seq-speed
            set-harmonic-range set-harmonic-speed
            set-harmonic-voice-convergence-point set-harmony
            set-independent-refrain set-next-gusano-harmonic-seq
            set-next-gusano-harmony set-panner-index set-panner-param
            set-processor-param-value! set-processor-preset-label-index!
            set-processor-preset-label-index2! set-rev-send
            set-selected-bank-synth set-synth-index switch-rec-durs
            switch-rec-pulse toggle-clouds toggle-gusano toogle-rec]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-helpers
    :refer [update-clients]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-helpers :as bardo.osc-helpers]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-router :as osc-router :refer [osc-router]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.presets
    :as bardo.presets]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.re-affect :as bardo.ræ]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synth-management
    :refer [stop-long-running-synths!]]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.math.utils :refer [linexp* linlin*]]
   [tieminos.osc.reaper :as reaper :refer [reaeq-freq->lin]]
   [tieminos.utils
    :refer
    [cb-interpolate stop-all-interpolators! str->int]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.touch-osc :as-alias bardo.tosc]))

(comment
  (require '[tieminos.network-utils :refer [get-local-host]])
  (def touchosc-fb-client (osc/osc-client (get-local-host) 16181)))

(do
  (def HACK-parse-path
    "Fixes a problem with the bank button which share the same address, so on feedback they all turn on or off."
    (let [indexed-paths #{"toggle-gusano-bank" "toggle-harmonic-voice"}]
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

(defn- reaper-tracks [k]
  (if-let [track-num (bardo.config/reaper-tracks* k)]
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

(def ^:private set-track-volume2 #'interpolate-track-volume)

(def ^:private perc-processes-amp-boost
  (make-volume-booster {:interpolator-id ::percussion-processes-boost
                        :level-offset (* -3 0.02) ;; index 3 = 0db boost
                        :track (reaper-tracks :percussion-processes-track)
                        :amp-fx-position 2}))

(comment
  (stop-all-interpolators!)
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
         :eq-track
         :subwoofer-track]))

(defn reaper-rec!
  []
  (doseq [track automated-tracks]
    (reaper/set-autowrite track))
  (reaper/rec)
  (bardo.live-state/start-recording)
  (bardo.osc-helpers/send-osc-msg "/System/not-recording-label" ""))

(defn reaper-stop!
  []
  (reaper/stop)
  ;; set OSC EQ envelope to write
  (doseq [track automated-tracks]
    (reaper/set-autoread track))
  (bardo.live-state/stop-recording)
  (bardo.osc-helpers/send-osc-msg "/System/not-recording-label" "NOT REC"))

(comment
  (update-clients @habitat-osc/receiver-clients "/Milo/harmonic-speed-label" [(str (round2 2 0.3455))]))

(defn- press? [args]
  (= 1.0 (first args)))

(def router
  (osc-router
   "/EQ/bell-freq-knob" (manual-set-eq-param :eq/bell.freq (first args))
   "/EQ/bell-gain-knob" (manual-set-eq-param :eq/bell.gain (first args))
   "/EQ/bell-radio" (set-bell-freq (first args))
   "/EQ/durs-radio" (set-eq-interpolation-dur (first args))
   "/EQ/flat-eq" (set-flat-eq)
   "/EQ/hishelf-freq-knob" (manual-set-eq-param :eq/hishelf.freq (first args))
   "/EQ/hishelf-gain-knob" (manual-set-eq-param :eq/hishelf.gain (first args))
   "/EQ/hishelf-radio" (set-hishelf-freq (first args))
   "/EQ/loshelf-freq-knob" (manual-set-eq-param :eq/loshelf.freq (first args))
   "/EQ/loshelf-gain-knob" (manual-set-eq-param :eq/loshelf.gain (first args))
   "/EQ/loshelf-radio" (set-loshelf-freq (first args))
   "/EQ/notch-freq-knob" (manual-set-eq-param :eq/notch.freq (first args))
   "/EQ/notch-gain-knob" (manual-set-eq-param :eq/notch.gain (first args))
   "/EQ/notch-radio" (set-notch-freq (first args))
   "/System/rec-start" (when (press? args) (reaper-rec!))
   "/System/rec-stop" (when (press? args) (reaper-stop!))
   "/System/subwoofer-master" (set-track-volume2 :subwoofer-track (first args) reaper/zero-db)
   "/System/voces-master" (set-track-volume2 :mixes-processes-2-track (first args) reaper/zero-db)
   "/gusano/2nd-voice" (set-gusano-2nd-voice (first args))
   "/gusano/amp" (set-gusano-amp (first args))
   "/gusano/durs" (set-gusano-durs (first args))
   "/gusano/grain-dur" (set-gusano-grain-dur (first args))
   "/gusano/grain-trig" (set-gusano-grain-trig (first args))
   "/gusano/gusano-active-btn" (toggle-gusano (press? args))
   "/gusano/harmonic-chord-inc-btn" (when (press? args) (inc-gusano-rate-index!))
   "/gusano/harmonic-seq-speed" (set-gusano-rates-seq-speed (first args))
   "/gusano/harmonic-seq-up-btn" (when (press? args) (set-next-gusano-harmonic-seq))
   "/gusano/harmony-up-btn" (when (press? args) (set-next-gusano-harmony))
   "/gusano/period" (set-gusano-period (first args))
   "/save-preset" (when (press? args) (bardo.presets/save-preset!))
   "/stop-long-running-synths" (when (press? args) (stop-long-running-synths! 20))
   "/:player/bank-delete-all-btn" (when (press? args) (delete-all-banks
                                                       (case player-k
                                                         :milo [:mic-1 :mic-2]
                                                         :diego [:guitar])))
   "/:player/bank-delete-btn" (when (press? args) (delete-bank (case player-k
                                                                 :milo [:mic-1 :mic-2]
                                                                 :diego [:guitar])))
   "/:player/bank-rec-radio" (set-active-recorded-bank (case player-k
                                                         :milo [:mic-1 :mic-2]
                                                         :diego [:guitar])
                                                       (first args))
   "/:player/clean-master" (set-track-volume2 (case player-k
                                                :milo :percussion-clean-track
                                                :diego :guitar-clean-track)
                                              (first args))
   "/:player/clouds-active-btn" (do (toggle-clouds player-k (press? args))
                                    (save-touchosc-synth-param player-k path args))
   "/:player/clouds-amp" (do (set-clouds-amp player-k (first args))
                             (save-touchosc-synth-param player-k path args))
   "/:player/clouds-env-radio" (do (set-clouds-env player-k (first args))
                                   (save-touchosc-synth-param player-k path args))
   "/:player/clouds-rhythm-radio" (do (set-clouds-rhythm player-k (first args))
                                      (save-touchosc-synth-param player-k path args))
   "/:player/clouds-sample-lib-size-radio" (do (set-clouds-sample-lib-size player-k (first args))
                                               (save-touchosc-synth-param player-k path args))
   "/:player/filter-down-btn" (when (press? args) (set-filter-index player-k -1))
   "/:player/filter-hpf-fader" (do (set-filter-param player-k :hpf (first args) :val-fn #(linexp* 0 1 40 20000 %))
                                   (save-touchosc-synth-param player-k path args))
   "/:player/filter-lpf-fader" (do (set-filter-param player-k :lpf (first args) :val-fn #(linexp* 0 1 40 20000 %))
                                   (bardo.live-state/save-touchosc-filter-param player-k path args))
   "/:player/filter-q-fader" (do (set-filter-param player-k :q (first args))
                                 (bardo.live-state/save-touchosc-filter-param player-k path args))
   "/:player/filter-reso-fader" (do (set-filter-param player-k :reso (first args))
                                    (save-touchosc-synth-param player-k path args))
   "/:player/filter-up-btn" (when (press? args) (set-filter-index player-k 1))
   "/:player/harmonic-highest-note" (do (set-harmonic-range
                                         {:player player-k :low? false :value (first args)})
                                        (save-touchosc-synth-param player-k path args))
   "/:player/harmonic-lowest-note" (do (set-harmonic-range {:player player-k :low? true :value (first args)})
                                       (save-touchosc-synth-param player-k path args))
   "/:player/harmonic-speed" (do (set-harmonic-speed player-k (first args))
                                 (save-touchosc-synth-param player-k path args))
   "/:player/harmonic-voice-cp" (do (set-harmonic-voice-convergence-point
                                     {:player player-k :value (first args)})
                                    (save-touchosc-synth-param player-k path args))
   "/:player/harmony-radio" (set-harmony player-k (first args))
   "/:player/independent-sequencer-btn" (do (set-independent-refrain player-k (press? args))
                                            (save-touchosc-synth-param player-k path args))
   "/Diego/input-amp-boost" (guitar-input-amp-boost (first args)) ;; FIXME: review
   "/:player/max-dur-fader" (do (bardo.live-state/set-clouds-max-dur% player-k (first args))
                                (save-touchosc-synth-param player-k path args))
   "/Diego/mute-line-in" (mute-input :guitar (first args))
   "/Diego/mute-mic" (mute-input :guitar/mic (first args))
   "/Milo/mute-mic-1" (mute-input :mic-1 (first args))
   "/Milo/mute-mic-2" (mute-input :mic-2 (first args))
   "/:player/panner-arrows-pos-fader" (set-panner-param player-k :pos (first args))
   "/:player/panner-arrows-range-fader" (set-panner-param player-k :range (first args))
   "/:player/panner-arrows-vel-fader" (set-panner-param player-k :vel (first args))
   "/:player/panner-down-btn" (when (press? args) (set-panner-index player-k -1))
   "/:player/panner-lissajous-direction-btn" (set-panner-param player-k :direction (first args))
   "/:player/panner-lissajous-radius-fader" (set-panner-param player-k :radius (first args))
   "/:player/panner-lissajous-vel-fader" (set-panner-param player-k :vel (first args))
   "/:player/panner-lissajous-x-fader" (set-panner-param player-k :x (first args) :value-fn #(linlin* 0 1 1 13 %))
   "/:player/panner-lissajous-y-fader" (set-panner-param player-k :y (first args) :value-fn #(linlin* 0 1 1 13 %))
   "/:player/panner-manual-xy" (set-panner-param player-k :xy args)
   "/:player/panner-rand-vel-fader" (set-panner-param player-k :vel (first args))
   "/:player/panner-up-btn" (when (press? args) (set-panner-index player-k 1))
   "/:player/processed-master" (set-track-volume2 (case player-k
                                                    :milo :percussion-processes-track
                                                    :diego :guitar-processes-track)
                                                  (first args))
   "/Milo/processes-amp-boost" (perc-processes-amp-boost (first args))
   "/:player/rec-durs-radio" (switch-rec-durs (case player-k
                                                :milo [:mic-1 :mic-2]
                                                :diego [:guitar])
                                              (first args))
   "/Diego/rec-guitar-btn" (toogle-rec {:input :guitar
                                        :on? (press? args)
                                        :dur (-> @live-state :rec :guitar :dur (or 0.5))})
   "/Milo/rec-mic-1-btn" (toogle-rec {:input :mic-1
                                      :on? (press? args)
                                      :dur (-> @live-state :rec :mic-1 :dur (or 0.5))})
   "/Milo/rec-mic-2-btn" (toogle-rec {:input :mic-2
                                      :on? (press? args)
                                      :dur (-> @live-state :rec :mic-2 :dur (or 0.5))})
   "/:player/rec-pulse-radio" (switch-rec-pulse (case player-k
                                                  :milo [:mic-1 :mic-2]
                                                  :diego [:guitar])
                                                (first args))
   "/:player/rev-send-clean" (set-rev-send {:player player-k :clean? true :value (first args)})
   "/:player/rev-send-process" (set-rev-send {:player player-k :clean? false :value (first args)})
   "/:player/selected-synth-radio" (set-selected-bank-synth player-k (first args))
   "/:player/synth-down-btn" (when (press? args) (set-synth-index player-k -1))
   "/:player/synth-up-btn" (when (press? args) (set-synth-index player-k 1))
   "/:player/toggle-gusano-bank/:index" (set-active-bank {:player player-k
                                                          :bank (-> path-params :index str->int)
                                                          :on? (== 1 (first args))})
   "/:player/toggle-harmonic-voice/:index" (do (set-active-harmonic-voice
                                                {:player player-k
                                                 :voice-index (-> path-params :index str->int)
                                                 :on? (== 1 (first args))})
                                               (save-touchosc-synth-param player-k path args))
   "/presets/guitar/next-label-btn" (when (press? args) (set-processor-preset-label-index! true))
   "/presets/guitar/prev-label-btn" (when (press? args) (set-processor-preset-label-index! false))
   "/presets/guitar/activate-preset-btn" (when (press? args) (activate-processor-preset!))
   "/presets/guitar/toggle-preset-buttons-view-btn" (bardo.ræ/dispatch
                                                     {::bardo.tosc/toggle-visibility
                                                      {:path "/presets/guitar/preset-buttons-visible"
                                                       :visible? (press? args)}})

   "/presets/guitar/buttons/:index" (when (press? args)
                                       ;; Because the way touch osc works (non-generative UI)
                                       ;; For simplicity, the UI was created by alternating btn/label in the document tree.
                                       ;; Therefore all buttons are odd numbers and labels are even.
                                       ;; Thus the preset index should be converted from a odd number to an index.
                                      (let [index (-> path-params :index str->int
                                                      (/ 2)
                                                      ceil
                                                      dec)]
                                        (set-processor-preset-label-index2! index)
                                        (activate-processor-preset! index)))
   "/guitar-fx-params/control/:index" (let
                                       [index (-> path-params :index str->int)
                                            ;; Because the way touch osc works (non-generative UI)
                                            ;; For simplicity, the UI was created by alternating knob/name/value in the document tree.
                                            ;; Therefore all knobs are mod 3 = 0 
                                            ;; Thus the preset index should be converted from an index into an quot 3 value
                                        index* (quot index 3)]
                                        (set-processor-param-value! index* (first args)))))

(comment
  (osc-router/match-by-path router "/presets/guitar/next-label-btn" '(1)))

(defn osc-responder
  [{:keys [path args] :as msg}]
  (let [HACKED-path (HACK-parse-path path) ;; FIXME: there should be a more elegant way to handle this (see fn definition).
        args-map (habitat-osc/args->map args)
        press? (= 1.0 (first args))]
    ;; TODO: delete once it's confirmed the router has no problems
    #_(case HACKED-path
        "/Milo/rec-mic-1-btn" (toogle-rec {:input :mic-1 :on? press? :dur (-> @live-state :rec :mic-1 :dur (or 0.5))})
        "/Milo/rec-mic-2-btn" (toogle-rec {:input :mic-2 :on? press? :dur (-> @live-state :rec :mic-2 :dur (or 0.5))})
        "/Milo/mute-mic-1"    (mute-input :mic-1 (first args))
        "/Milo/mute-mic-2"    (mute-input :mic-2 (first args))
        "/Milo/rec-durs-radio" (switch-rec-durs [:mic-1 :mic-2] (first args))
        "/Milo/rec-pulse-radio" (switch-rec-pulse [:mic-1 :mic-2] (first args))
        "/Milo/clouds-active-btn" (do (toggle-clouds :milo press?)
                                      (save-touchosc-synth-param :milo path args))
        "/Milo/independent-sequencer-btn" (do (set-independent-refrain :milo press?)
                                              (save-touchosc-synth-param :milo path args))
        "/Milo/clouds-amp" (do (set-clouds-amp :milo (first args))
                               (save-touchosc-synth-param :milo path args))
        "/Milo/clouds-env-radio" (do (set-clouds-env :milo (first args))
                                     (save-touchosc-synth-param :milo path args))
        "/Milo/clouds-rhythm-radio" (do (set-clouds-rhythm :milo (first args))
                                        (save-touchosc-synth-param :milo path args))
        "/Milo/clouds-sample-lib-size-radio" (do (set-clouds-sample-lib-size :milo (first args))
                                                 (save-touchosc-synth-param :milo path args))
        "/Milo/synth-up-btn" (when press? (set-synth-index :milo 1))
        "/Milo/synth-down-btn" (when press? (set-synth-index :milo -1))
        "/Milo/max-dur-fader" (do (bardo.live-state/set-clouds-max-dur% :milo (first args))
                                  (save-touchosc-synth-param :milo path args))
        "/Milo/filter-up-btn" (when press? (set-filter-index :milo 1))
        "/Milo/filter-down-btn" (when press? (set-filter-index :milo -1))
        "/Milo/filter-lpf-fader" (do (set-filter-param :milo :lpf (first args) :val-fn #(linexp* 0 1 40 20000 %))
                                     (save-touchosc-synth-param :milo path args))
        "/Milo/filter-hpf-fader" (do (set-filter-param :milo :hpf (first args) :val-fn #(linexp* 0 1 40 20000 %))
                                     (save-touchosc-synth-param :milo path args))
        "/Milo/filter-reso-fader" (do (set-filter-param :milo :reso (first args))
                                      (save-touchosc-synth-param :milo path args))
        "/Milo/filter-q-fader" (set-filter-param :milo :q (first args))
        "/Milo/panner-up-btn" (when press? (set-panner-index :milo 1))
        "/Milo/panner-down-btn" (when press? (set-panner-index :milo -1))
        "/Milo/panner-rand-vel-fader" (set-panner-param :milo :vel (first args))
        "/Milo/panner-arrows-pos-fader" (set-panner-param :milo :pos (first args))
        "/Milo/panner-arrows-range-fader" (set-panner-param :milo :range (first args))
        "/Milo/panner-arrows-vel-fader" (set-panner-param :milo :vel (first args))
        "/Milo/panner-lissajous-x-fader" (set-panner-param :milo :x (first args) :value-fn #(linlin* 0 1 1 13 %))
        "/Milo/panner-lissajous-y-fader" (set-panner-param :milo :y (first args) :value-fn #(linlin* 0 1 1 13 %))
        "/Milo/panner-lissajous-radius-fader" (set-panner-param :milo :radius (first args))
        "/Milo/panner-lissajous-vel-fader" (set-panner-param :milo :vel (first args))
        "/Milo/panner-lissajous-direction-btn" (set-panner-param :milo :direction (first args))
        "/Milo/panner-manual-xy" (set-panner-param :milo :xy args)
        "/Milo/bank-rec-radio" (set-active-recorded-bank [:mic-1 :mic-2] (first args))
        "/Milo/bank-delete-btn" (when press? (delete-bank [:mic-1 :mic-2]))
        "/Milo/bank-delete-all-btn" (when press? (delete-all-banks [:mic-1 :mic-2]))
        "/Milo/toggle-gusano-bank" (set-active-bank {:player :milo, :bank (:index args-map), :on? (== 1 (:on args-map))})
        "/Milo/harmony-radio" (set-harmony :milo (first args))
        "/Milo/harmonic-speed" (do (set-harmonic-speed :milo (first args))
                                   (save-touchosc-synth-param :milo path args))
        "/Milo/harmonic-lowest-note" (do (set-harmonic-range {:player :milo :low? true :value (first args)})
                                         (save-touchosc-synth-param :milo path args))
        "/Milo/harmonic-highest-note" (do (set-harmonic-range {:player :milo :low?  false :value (first args)})
                                          (save-touchosc-synth-param :milo path args))
        "/Milo/harmonic-voice-cp" (do (set-harmonic-voice-convergence-point {:player :milo :value (first args)})
                                      (save-touchosc-synth-param :milo path args))
        "/Milo/toggle-harmonic-voice" (do (set-active-harmonic-voice {:player :milo :voice-index (:index args-map) :on? (== 1 (:on args-map))})
                                          (save-touchosc-synth-param :milo path args))
        "/Milo/rev-send-clean" (set-rev-send {:player :milo :clean? true :value (first args)})
        "/Milo/rev-send-process" (set-rev-send {:player :milo :clean? false :value (first args)})
        "/Milo/selected-synth-radio" (set-selected-bank-synth :milo (first args))
        "/Milo/clean-master" (set-track-volume2 :percussion-clean-track (first args))
        "/Milo/processed-master" (set-track-volume2 :percussion-processes-track (first args))
        "/Milo/processes-amp-boost" (perc-processes-amp-boost (first args))
        "/Diego/rec-guitar-btn" (toogle-rec {:input :guitar :on? press? :dur (-> @live-state :rec :mic-1 :dur (or 0.5))})
        "/Diego/rec-durs-radio" (switch-rec-durs [:guitar] (first args))
        "/Diego/rec-pulse-radio" (switch-rec-pulse [:guitar] (first args))
        "/Diego/mute-line-in"    (mute-input :guitar (first args))
        "/Diego/mute-mic"    (mute-input :guitar/mic (first args))

        "/Diego/clouds-active-btn" (do (toggle-clouds :diego press?)
                                       (save-touchosc-synth-param :diego path args))
        "/Diego/independent-sequencer-btn" (do (set-independent-refrain :diego press?)
                                               (save-touchosc-synth-param :diego path args))
        "/Diego/clouds-amp" (do (set-clouds-amp :diego (first args))
                                (save-touchosc-synth-param :diego path args))
        "/Diego/clouds-env-radio" (do (set-clouds-env :diego (first args))
                                      (save-touchosc-synth-param :diego path args))
        "/Diego/clouds-rhythm-radio" (do (set-clouds-rhythm :diego (first args))
                                         (save-touchosc-synth-param :diego path args))
        "/Diego/clouds-sample-lib-size-radio" (do (set-clouds-sample-lib-size :diego (first args))
                                                  (save-touchosc-synth-param :diego path args))
        "/Diego/synth-up-btn" (when press? (set-synth-index :diego 1))
        "/Diego/synth-down-btn" (when press? (set-synth-index :diego -1))
        "/Diego/max-dur-fader" (do (bardo.live-state/set-clouds-max-dur% :diego (first args))
                                   (save-touchosc-synth-param :diego path args))
        "/Diego/filter-up-btn" (when press? (set-filter-index :diego 1))
        "/Diego/filter-down-btn" (when press? (set-filter-index :diego -1))
        "/Diego/filter-lpf-fader" (do (set-filter-param :diego :lpf (first args) :val-fn #(linexp* 0 1 40 20000 %))
                                      (bardo.live-state/save-touchosc-filter-param :diego path args))
        "/Diego/filter-hpf-fader" (do (set-filter-param :diego :hpf (first args) :val-fn #(linexp* 0 1 40 20000 %))
                                      (bardo.live-state/save-touchosc-filter-param :diego path args))
        "/Diego/filter-reso-fader" (do (set-filter-param :diego :reso (first args))
                                       (bardo.live-state/save-touchosc-filter-param :diego path args))
        "/Diego/filter-q-fader" (do (set-filter-param :diego :q (first args))
                                    (bardo.live-state/save-touchosc-filter-param :diego path args))
        "/Diego/panner-up-btn" (when press? (set-panner-index :diego 1))
        "/Diego/panner-down-btn" (when press? (set-panner-index :diego -1))
        "/Diego/panner-rand-vel-fader" (set-panner-param :diego :vel (first args))
        "/Diego/panner-arrows-pos-fader" (set-panner-param :diego :pos (first args))
        "/Diego/panner-arrows-range-fader" (set-panner-param :diego :range (first args))
        "/Diego/panner-arrows-vel-fader" (set-panner-param :diego :vel (first args))
        "/Diego/panner-lissajous-x-fader" (set-panner-param :diego :x (first args) :value-fn #(linlin* 0 1 1 13 %))
        "/Diego/panner-lissajous-y-fader" (set-panner-param :diego :y (first args) :value-fn #(linlin* 0 1 1 13 %))
        "/Diego/panner-lissajous-radius-fader" (set-panner-param :diego :radius (first args))
        "/Diego/panner-lissajous-vel-fader" (set-panner-param :diego :vel (first args))
        "/Diego/panner-lissajous-direction-btn" (set-panner-param :diego :direction (first args))
        "/Diego/panner-manual-xy" (set-panner-param :diego :xy args)
        "/Diego/bank-rec-radio" (set-active-recorded-bank [:guitar] (first args))
        "/Diego/toggle-gusano-bank" (set-active-bank {:player :diego,:bank (:index args-map), :on? (== 1 (:on args-map))})
        "/Diego/bank-delete-btn" (when press? (delete-bank [:guitar]))
        "/Diego/bank-delete-all-btn" (when press? (delete-all-banks [:guitar]))
        "/Diego/harmony-radio" (set-harmony :diego (first args))
        "/Diego/harmonic-speed" (do (set-harmonic-speed :diego (first args))
                                    (save-touchosc-synth-param :diego path args))
        "/Diego/harmonic-lowest-note" (do (set-harmonic-range {:player :diego :low? true :value (first args)})
                                          (save-touchosc-synth-param :diego path args))
        "/Diego/harmonic-highest-note" (do (set-harmonic-range {:player :diego :low?  false :value (first args)})
                                           (save-touchosc-synth-param :diego path args))
        "/Diego/toggle-harmonic-voice" (do (set-active-harmonic-voice {:player :diego :voice-index (:index args-map) :on? (== 1 (:on args-map))})
                                           (save-touchosc-synth-param :diego path args))
        "/Diego/harmonic-voice-cp" (do (set-harmonic-voice-convergence-point {:player :diego :value (first args)})
                                       (save-touchosc-synth-param :diego path args))
        "/Diego/rev-send-clean" (set-rev-send {:player :diego :clean? true :value (first args)})
        "/Diego/rev-send-process" (set-rev-send {:player :diego :clean? false :value (first args)})
        "/Diego/input-amp-boost" (guitar-input-amp-boost (first args))
        "/Diego/selected-synth-radio" (set-selected-bank-synth :diego (first args))
        "/Diego/clean-master" (set-track-volume2 :guitar-clean-track (first args))
        "/Diego/processed-master" (set-track-volume2 :guitar-processes-track (first args))
        ;; synth management
        "/stop-long-running-synths" (when press? (stop-long-running-synths! 20))
        ;; gusano
        "/gusano/gusano-active-btn" (toggle-gusano press?)
        "/gusano/amp" (set-gusano-amp (first args))
        "/gusano/period" (set-gusano-period (first args))
        "/gusano/durs" (set-gusano-durs (first args))
        "/gusano/grain-trig" (set-gusano-grain-trig (first args))
        "/gusano/grain-dur" (set-gusano-grain-dur (first args))
        "/gusano/2nd-voice" (set-gusano-2nd-voice (first args))
        "/gusano/harmony-up-btn" (when press? (set-next-gusano-harmony))
        "/gusano/harmonic-seq-up-btn" (when press? (set-next-gusano-harmonic-seq))
        "/gusano/harmonic-seq-speed" (set-gusano-rates-seq-speed (first args))
        "/gusano/harmonic-chord-inc-btn" (when press? (inc-gusano-rate-index!))
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
        "/System/voces-master" (set-track-volume2 :mixes-processes-2-track (first args) reaper/zero-db)
        "/System/subwoofer-master" (set-track-volume2 :subwoofer-track (first args) reaper/zero-db)
        (timbre/warn "Unknown path for message: " HACKED-path msg args-map))

    (osc-router/match-by-path router path args)
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
  (habitat-osc/make-internal-osc-client)
  (habitat-osc/responder #'osc-responder))

(comment
  (remove-watch bardo.live-state/live-state ::post-live-state))

;;;;;;;;;;;;;;;;;;;;;;;;
;; * UI
;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-bufs-count
  [bufs-count-data]
  (let [val
        (with-out-str
          (-> bufs-count-data
              (dissoc nil) ;; TODO: figure out why there is `nil` value here

              (->> (map (fn [[k v]]
                          (assoc
                           (into {} (map (fn [[k v]] [(inc k) v]) v))
                           :input k)))
                   (pprint/print-table (concat [:input] (range 1 9))))))]
    (bardo.osc-helpers/send-osc-msg "/Rec/buffer-data-label" val)))

