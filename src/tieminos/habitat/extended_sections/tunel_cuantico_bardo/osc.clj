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
   [tieminos.math.utils :refer [linexp* linlin]]
   [tieminos.osc.reaper :as reaper :refer [reaeq-freq->lin]]
   [tieminos.utils :refer [cb-interpolate stop-all-interpolators! throttle
                           wrap-at]]))

(declare reaper-tracks* update-label send-osc-msg)

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
  (reset! live-state {})
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

(comment
  (toggle-clouds :milo true))

(declare update-clients default-touch-osc-state)
(comment
  (update-clients @habitat-osc/receiver-clients
                  "/Milo/selected-synth-label"
                  ["#1" (rand-nth ["0000FF"
                                   "FF00FF"
                                   "FFF0FF"])])
  (update-clients @habitat-osc/receiver-clients
                  "/Milo/synth-section-box"
                  ["FF00FF33"])

  (update-clients @habitat-osc/receiver-clients
                  "/Milo/selected-synth-label/color"
                  (map float [1.0 0.0 0.0]))

  (-> @live-state)
  (reset! live-state {}))
(def ^:private synth-ui-params
  "They should be prefaced with `/Milo` or `/Diego`"
  ["/clouds-active-btn"
   "/clouds-amp"
   "/clouds-env-radio"
   "/clouds-rhythm-radio"
   "/clouds-sample-lib-size-radio"
   "/synth-radio"
   "/harmonic-highest-note"
   "/harmonic-lowest-note"
   "/harmonic-speed"
   "/toggle-harmonic-voice/0"
   "/toggle-harmonic-voice/1"
   "/toggle-harmonic-voice/2"])

(def ^:private bank-colors
  ;; many more color pallettes to try: https://colorkit.co/palettes/8-colors/
  (map #(str % "99") ["c7522a" "e5c185" "f0daa5" "fbf2c4" "b8cdab" "74a892" "008585" "004343"]))

(defn- set-touchosc-synth-ui
  [player selected-synth-bank
   {:keys [touch-osc-data]
    :as _synth-data}]
  (let [path-base (case player
                    :milo "/Milo"
                    :diego "/Diego")
        bg-color (wrap-at selected-synth-bank bank-colors)]
    (update-clients @habitat-osc/receiver-clients
                    (str path-base "/selected-synth-label")
                    [(str "#" (inc selected-synth-bank))
                     bg-color])
    (update-clients @habitat-osc/receiver-clients
                    (str path-base "/synth-section-box")
                    [bg-color])
    (doseq [[path args] touch-osc-data]
      (println path args)
      (update-clients @habitat-osc/receiver-clients
                      path args))))

(defn- synth-bank-path
  [player & keys]
  (concat [:algo-2.2.9-clouds player] keys))

(defn- get-selected-synth-bank
  [player]
  (get-in @live-state (synth-bank-path player :selected-bank) :default-bank))

(defn- get-selected-synth-data
  [player]
  (let [bank (get-in @live-state (synth-bank-path player :selected-bank) :default-bank)]
    (get-in @live-state (synth-bank-path player bank))))

(defn- selected-synth-bank-path
  [player & keys]
  (apply synth-bank-path player (get-selected-synth-bank player) keys))

(defn- save-touchosc-synth-param
  ([player {:keys [path value]}]
   (save-touchosc-synth-param player path value))
  ([player osc-path value]
   (swap! live-state
          assoc-in
          (selected-synth-bank-path player :touch-osc-data osc-path)
          value)))

(defn- osc-bool [bool] (int (if bool 1 0)))

(comment
  (get-in @live-state (synth-bank-path :milo 0)))

(defn- init-synth-data
  [player bank]
  (let [path-base (case player
                    :milo "/Milo"
                    :diego "/Diego")
        paths (map #(str path-base %) synth-ui-params)]
    (swap! live-state
           assoc-in
           (synth-bank-path player bank :touch-osc-data)
           (select-keys default-touch-osc-state paths))))

(defn- set-selected-bank-synth
  [player bank]
  (let [path (synth-bank-path player :selected-bank)
        state (swap! live-state assoc-in path bank)
        synth-data (get-in state (synth-bank-path player bank))]
    (when (nil? synth-data)
      (init-synth-data player bank))
    (set-touchosc-synth-ui player bank synth-data)))

(defn- show-active-bank-label [player show?]
  (send-osc-msg (format
                 "/%s/bank%s-active-label-visible"
                 (-> player name str/capitalize)
                 (inc (get-selected-synth-bank player)))
                (str show?)))

(defn toggle-clouds
  [player on?]
  (swap! live-state
         assoc-in
         (selected-synth-bank-path player)
         (-> default-cloud-config
             (merge (get-selected-synth-data player))
             (assoc :on? on?)))

  (if on?
    (do #_(bardo.live-ctl/start-clouds player)
     (show-active-bank-label player on?))
    (do #_(bardo.live-ctl/stop-clouds player)
     (show-active-bank-label player on?))))

(defn set-clouds-amp
  [player amp]
  ;; TODO: finish integrating
  (swap! live-state
         assoc-in
         (selected-synth-bank-path player :amp)
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
    (swap! live-state
           assoc-in
           (selected-synth-bank-path player :sample-lib-size)
           env)))
(def ^:private synth-keys
  [:granular :crystal])

(defn- set-active-synth
  "For a given bank, it selects the active synth based on the index of the `synth-keys`"
  [player]
  (let [synth-index (:synth-index (get-selected-synth-data player))
        synth-key (wrap-at synth-index synth-keys)]
    (swap! live-state assoc-in
           (selected-synth-bank-path player :active-synth)
           synth-key)
    synth-key))
(defn- update&save-synth-label
  "Updates the label of a synth param.
  Expects `label-key` to be provided as `:my-label` when in touchosc is defined as `/player/my-label-label`, with the `-label` suffix."
  [player label-key value]
  (->> (update-label player label-key value)
       (save-touchosc-synth-param player)))

(defn- set-synth-index
  [player direction]
  (let [_ (swap! live-state update-in
                 (selected-synth-bank-path player :synth-index)
                 (fnil + 0)
                 direction)
        synth-key (name (set-active-synth player))]
    (update&save-synth-label player :synth synth-key)
    (update-label player
                  (format "bank%s-active" (inc (get-selected-synth-bank player)))
                  (str (first synth-key)))))

(comment
  (get-selected-synth-bank :milo)
  (get-selected-synth-data :milo))

(comment
  (linexp* 0 1 40 20000 0))

;;;;;;;;;;;;
;; Filters
;;;;;;;;;;;;

(def ^:private filter-data
  ;; TODO: find good defaults and proper param mappings, This is just a place holder.
  ;; NOTE: for params to be proporly updated, they should be present in the the particular filter data map. Otherwise the `:path` will be missing and no update will happen.
  {:lpf {:lpf {:path "/filter-lpf-fader"
               :default-value (float 1)}
         :hpf {:path "/filter-hpf-fader"
               :visible? false
               :default-value (float 1)}
         :reso {:path "/filter-reso-fader"
                :default-value (float 0.5)}
         :q {:path "/filter-q-fader"
             :default-value (float 0.5)}}
   :hpf {:lpf {:path "/filter-lpf-fader"
               :visible? false
               :default-value (float 1)}
         :hpf {:path "/filter-hpf-fader"
               :default-value (float 1)}
         :reso {:path "/filter-reso-fader"
                :default-value (float 0.5)}
         :q {:path "/filter-q-fader"
             :default-value (float 0.5)}}
   :moog-ladder {:lpf {:path "/filter-lpf-fader"
                       :default-value (float 1)}
                 :hpf {:path "/filter-hpf-fader"
                       :default-value (float 1)}
                 :reso {:path "/filter-reso-fader"
                        :default-value (float 0.5)}
                 :q {:path "/filter-q-fader"
                     :default-value (float 0.5)}}})

(def ^:private all-filter-params (->> filter-data vals (apply merge) keys))

(def ^:private filter-keys (keys filter-data))

(defn- set-filter-config
  "Sets the appropriate filter configuration and updates UI"
  [player]
  (let [{:keys [active-filter filter-configs]} (get-selected-synth-data player)
        filter-data (get filter-data active-filter)
        current-config* (get filter-configs active-filter)
        current-config (->> all-filter-params
                            (map (fn [k]
                                   [k (get current-config* k
                                           (get-in filter-data [k :default-value]))]))
                            (into {}))
        base-path (case player :milo "/Milo" :diego "/diego")
        osc-msgs (->> current-config
                      (map (fn [[k v]]
                             (let [path (str base-path (-> filter-data k :path))
                                   visible? (get-in filter-data [k :visible?] true)]
                               {path [v]
                                (str path "-visible") [(osc-bool visible?)]})))
                      (apply merge))]

    (doseq [[path v] osc-msgs] (apply send-osc-msg path v))

    (swap! live-state
           (fn [state]
             (-> state
                 (assoc-in (selected-synth-bank-path player :filter-configs active-filter)
                           current-config)
                 (update-in (selected-synth-bank-path player :touch-osc-data)
                            merge
                            osc-msgs))))
    nil))

(defn- set-active-filter
  "For a given bank, it selects the active filter based on the index of the `filter-keys`"
  [player]
  (let [filter-index (:filter-index (get-selected-synth-data player))
        filter-key (wrap-at filter-index filter-keys)]
    (swap! live-state assoc-in
           (selected-synth-bank-path player :active-filter)
           filter-key)
    filter-key))

(defn- set-filter-index
  [player direction]
  (let [_ (swap! live-state update-in
                 (selected-synth-bank-path player :filter-index)
                 (fnil + 0)
                 direction)
        filter-key (name (set-active-filter player))]
    (set-filter-config player)
    (update&save-synth-label player :filter filter-key)))

(defn- set-filter-param
  [player param-k value]
  (let [active-filter (:active-filter (get-selected-synth-data player))]
    (swap! live-state
           #(-> %
                (assoc-in (selected-synth-bank-path
                           player
                           :filter-configs
                           active-filter
                           param-k)
                          value)))))

(comment
  (reset! live-state {})
  (get-selected-synth-bank :milo)
  (get-selected-synth-data :milo))

;;;;;;;;;;;;
;; Panners
;;;;;;;;;;;;

(def ^:private panner-data
  ;; TODO: find good defaults. This is just a place holder.
  ;; NOTE: for params to be proporly updated, they should be present in the the particular panner data map. Otherwise the `:path` will be missing and no update will happen.
  {:random {:vel {:path "/panner-rand-vel-fader"
                  :default-value (float 0.1)}}
   :manual {:xy {:path "/panner-manual-xy"
                 :default-value (map float [0.5 0.5])}}
   :lissajous {:x {:path "/panner-lissajous-x-fader"
                   :default-value (float 0.1)}
               :y  {:path "/panner-lissajous-y-fader"
                    :default-value (float 0.1)}
               :radius {:path "/panner-lissajous-radius-fader"
                        :default-value (float 0.1)}
               :vel   {:path "/panner-lissajous-vel-fader"
                       :default-value (float 0.1)}
               :direction {:path "/panner-lissajous-direction-btn"
                           :default-value (int 0)}}
   :arrows {:pos {:path "/panner-arrows-pos-fader"
                  :default-value (float 0.1)}
            :range {:path "/panner-arrows-range-fader"
                    :default-value (float 0.1)}
            :vel {:path "/panner-arrows-vel-fader"
                  :default-value (float 0.1)}}})

(def ^:private panner-keys (keys panner-data))

(defn- player-path
  "Creates an OSC path of the form /player/some/path "
  [player param-path]
  (format "%s%s" (case player :milo "/Milo" :diego "/diego")
          (if (str/starts-with? param-path "/")
            param-path
            (str "/" param-path))))

(defn- set-panner-config
  "Sets the appropriate panner configuration and updates UI"
  [player]
  (let [{:keys [active-panner panner-configs]} (get-selected-synth-data player)
        panner-data (get panner-data active-panner)
        ;; NOTE: keeping panner configs may not be necessary because they are kept in the UI due to groups (vs the case with filters which reuse the UI)
        current-config* (get panner-configs active-panner)
        current-config (merge (->> panner-data ;; the defaults
                                   (map (juxt first (comp :default-value second)))
                                   (into {}))
                              current-config*)
        base-path (case player :milo "/Milo" :diego "/diego")
        groups-visibility-osc (->> panner-keys
                                   (map (fn [k] [(format "%s/panner-%s-group" base-path (name k)) [(osc-bool (= k active-panner))]]))
                                   (into {}))
        osc-msgs (->> (keys panner-data)
                      (map (fn [k]
                             (let [path (str base-path (-> panner-data k :path))]
                               [path (flatten ;; the `:manual` config's value :xy is a list
                                      [(current-config k)])])))
                      (into groups-visibility-osc))]

    (doseq [[path v] osc-msgs] (apply send-osc-msg path v))

    (swap! live-state
           (fn [state]
             (-> state
                 (assoc-in (selected-synth-bank-path player :panner-configs active-panner)
                           current-config)
                 (update-in (selected-synth-bank-path player :touch-osc-data)
                            merge
                            osc-msgs))))
    nil))

(defn- set-active-panner
  "For a given bank, it selects the active panner based on the index of the `panner-keys`"
  [player]
  (let [panner-index (:panner-index (get-selected-synth-data player))
        panner-key (wrap-at panner-index panner-keys)]
    (swap! live-state assoc-in
           (selected-synth-bank-path player :active-panner)
           panner-key)
    panner-key))

(defn- set-panner-index
  [player direction]
  (let [_ (swap! live-state update-in
                 (selected-synth-bank-path player :panner-index)
                 (fnil + 0)
                 direction)
        panner-key (name (set-active-panner player))]
    (set-panner-config player)
    (update&save-synth-label player :panner panner-key)))

(defn- set-panner-param
  [player param-k value]
  (let [active-panner (:active-panner (get-selected-synth-data player))]
    (swap! live-state
           #(-> %
                (assoc-in (selected-synth-bank-path
                           player
                           :panner-configs
                           active-panner
                           param-k)
                          value)
                (assoc-in (selected-synth-bank-path
                           player
                           :touch-osc-data
                           (player-path player
                                        (get-in panner-data
                                                [active-panner param-k :path])))
                          ;; the `:manual` config's value :xy is a list
                          (flatten [value]))))))

(comment
  (reset! live-state {})
  (add-watch live-state :debug
             (fn [_ _ _ _]
               #_(clojure.pprint/pprint (get-selected-synth-data :milo))))
  (get-in panner-data [:random :vel :path])
  (get-selected-synth-bank :milo)
  (get-selected-synth-data :milo))

;;;;;;;;;;;;;;;;;
;; Envelopes
;;;;;;;;;;;;;;;;

;; TODO eliminate?
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
    (swap! live-state assoc-in
           (selected-synth-bank-path player :env)
           env)))

(defn set-clouds-rhythm
  [player opt-num]
  (let [env (case opt-num
              0 :lor-0.1_2
              1 :lor-2_6
              2 :rand-0_10
              3 :rit
              4 :accel
              (throw (ex-info "Unkown clouds rhythm" {:player player :opt-num opt-num})))]
    (swap! live-state assoc-in
           (selected-synth-bank-path player :rhythm)
           env)
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
           assoc-in
           (selected-synth-bank-path player :harmonic-speed)
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
  (let [path (selected-synth-bank-path player :harmonic-range)
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
  (swap! live-state update-in
         (selected-synth-bank-path player :harmonic-active-voices)
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
    (reaper/set-autotrim track)))

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
      ;; TODO: << eliminate following
      "/Milo/clouds-active-btn" (do (toggle-clouds :milo press?)
                                    (save-touchosc-synth-param :milo path args))
      "/Milo/clouds-amp" (do (set-clouds-amp :milo (first args))
                             (save-touchosc-synth-param :milo path args))
      "/Milo/clouds-env-radio" (do (set-clouds-env :milo (first args))
                                   (save-touchosc-synth-param :milo path args))
      "/Milo/clouds-rhythm-radio" (do (set-clouds-rhythm :milo (first args))
                                      (save-touchosc-synth-param :milo path args))
      "/Milo/clouds-sample-lib-size-radio" (do (set-clouds-sample-lib-size :milo (first args))
                                               (save-touchosc-synth-param :milo path args))
      ;; TODO: end eliminate >>
      "/Milo/selected-synth-radio" (set-selected-bank-synth :milo (first args)) ;; TODO eliminate
      "/Milo/synth-up-btn" (when press? (set-synth-index :milo 1))
      "/Milo/synth-down-btn" (when press? (set-synth-index :milo -1))
      "/Milo/max-dur-fader" (timbre/warn "TODO Implement")
      "/Milo/filter-up-btn" (when press? (set-filter-index :milo 1))
      "/Milo/filter-down-btn" (when press? (set-filter-index :milo -1))
      "/Milo/filter-lpf-fader" (set-filter-param :milo :lpf (first args))
      "/Milo/filter-hpf-fader" (set-filter-param :milo :hpf (first args))
      "/Milo/filter-reso-fader" (set-filter-param :milo :reso (first args))
      "/Milo/filter-q-fader" (set-filter-param :milo :q (first args))
      "/Milo/panner-up-btn" (when press? (set-panner-index :milo 1))
      "/Milo/panner-down-btn" (when press? (set-panner-index :milo -1))
      "/Milo/panner-rand-vel-fader" (set-panner-param :milo :vel (first args))
      "/Milo/panner-arrows-pos-fader" (set-panner-param :milo :pos (first args))
      "/Milo/panner-arrows-range-fader" (set-panner-param :milo :range (first args))
      "/Milo/panner-arrows-vel-fader" (set-panner-param :milo :vel (first args))
      "/Milo/panner-lissajous-x-fader" (set-panner-param :milo :x (first args))
      "/Milo/panner-lissajous-y-fader" (set-panner-param :milo :y (first args))
      "/Milo/panner-lissajous-radius-fader" (set-panner-param :milo :radius (first args))
      "/Milo/panner-lissajous-vel-fader" (set-panner-param :milo :vel (first args))
      "/Milo/panner-lissajous-direction-btn" (set-panner-param :milo :direction (first args))
      "/Milo/panner-manual-xy" (set-panner-param :milo :xy args)
      ;; TODO implement other panners
      "/Milo/bank-rec-radio" (set-active-recorded-bank [:mic-1 :mic-2] (first args))
      "/Milo/bank-delete-btn" (when press? (delete-bank [:mic-1 :mic-2]))
      "/Milo/bank-delete-all-btn" (when press? (delete-all-banks [:mic-1 :mic-2]))
      "/Milo/toggle-bank" (set-active-bank {:player :milo :bank (:index args-map) :on? (== 1 (:on args-map))})
      "/Milo/harmony-radio" (set-harmony :milo (first args))
      "/Milo/harmonic-speed" (do (set-harmonic-speed :milo (first args))
                                 (save-touchosc-synth-param :milo path args))
      "/Milo/harmonic-lowest-note" (do (set-harmonic-range {:player :milo :low? true :value (first args)})
                                       (save-touchosc-synth-param :milo path args))
      "/Milo/harmonic-highest-note" (do (set-harmonic-range {:player :milo :low?  false :value (first args)})
                                        (save-touchosc-synth-param :milo path args))
      "/Milo/toggle-harmonic-voice" (do (set-active-harmonic-voice {:player :milo :voice-index (:index args-map) :on? (== 1 (:on args-map))})
                                        (save-touchosc-synth-param :milo path args))
      "/Milo/rev-send-clean" (set-rev-send {:player :milo :clean? true :value (first args)})
      "/Milo/rev-send-process" (set-rev-send {:player :milo :clean? false :value (first args)})
      "/Milo/processed-master" (set-track-volume2 :percussion-processes-track (first args))
      "/Milo/processes-amp-boost" (perc-processes-amp-boost (first args))
      "/Diego/rec-guitar-btn" (toogle-rec {:input :guitar :on? press? :dur (-> @live-state :rec :mic-1 :dur (or 0.5))})
      "/Diego/rec-durs-radio" (switch-rec-durs [:guitar] (first args))
      "/Diego/rec-pulse-radio" (switch-rec-pulse [:guitar] (first args))
      "/Diego/mute-line-in"    (mute-input :guitar (first args))
      "/Diego/mute-mic"    (mute-input :guitar/mic (first args))
      ;; TODO: << eliminate following
      "/Diego/clouds-active-btn" (toggle-clouds :diego press?)
      "/Diego/clouds-amp" (set-clouds-amp :diego (first args))
      "/Diego/clouds-env-radio" (set-clouds-env :diego (first args))
      "/Diego/clouds-rhythm-radio" (set-clouds-rhythm :diego (first args))
      "/Diego/clouds-sample-lib-size-radio" (set-clouds-sample-lib-size :diego (first args))
      ;; TODO: end eliminate >>
      "/Diego/bank-rec-radio" (set-active-recorded-bank [:guitar] (first args))
      "/Diego/toggle-bank" (set-active-bank {:player :diego :bank (:index args-map) :on? (== 1 (:on args-map))})
      "/Diego/bank-delete-btn" (when press? (delete-bank [:guitar]))
      "/Diego/bank-delete-all-btn" (when press? (delete-all-banks [:guitar]))
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

(defn- cast-osc-data [data]
  (map (fn [[k v]] [k (map #(cond
                              (symbol? %) (eval %) ;; NOTE this may cause trouble
                              (not (number? %)) %
                              (float? %) (float %)
                              :else (int %)) v)])
       data))

(defn ^:private make-synth-defaults
  [player]
  {:active-filter :lpf,
   :active-panner :random,
   :filter-configs {:lpf {:lpf 1.0, :hpf 0.0, :reso 0.0, :q 0.0}},
   :panner-configs {:random {:vel 0.1}},
   :active-synth :crystal,
   :amp -36.0,
   :sample-lib-size ##Inf,
   :env :lor-1_4,
   :harmonic-speed 1,
   :rhythm :lor-0.1_2,
   :synth-index 1,
   :touch-osc-data (->> {"/%s/filter-lpf-fader-visible" [1],
                         "/%s/harmonic-speed" '(0.20449468),
                         "/%s/panner-random-group" [1],
                         "/%s/panner-manual-group" [0],
                         "/%s/panner-lissajous-group" [0],
                         "/%s/panner-arrows-group" [0],
                         "/%s/filter-q-fader" [0.0],
                         "/%s/panner-label" ["random"],
                         "/%s/filter-lpf-fader" [1.0],
                         "/%s/toggle-harmonic-voice/1" '("on" 1 "index" 1),
                         "/%s/filter-hpf-fader-visible" [0],
                         "/%s/toggle-harmonic-voice/0" '("on" 1 "index" 0),
                         "/%s/clouds-rhythm-radio" '(0),
                         "/%s/toggle-harmonic-voice/2" '("on" 1 "index" 2),
                         "/%s/filter-hpf-fader" [1.0],
                         "/%s/harmonic-highest-note" '(0.5190911),
                         "/%s/clouds-sample-lib-size-radio" '(0),
                         "/%s/clouds-active-btn" '(0.0),
                         "/%s/synth-label" ["crystal"],
                         "/%s/clouds-amp" '(0.0),
                         "/%s/harmonic-lowest-note" '(0.48726025),
                         "/%s/panner-rand-vel-fader" '(0.1),
                         "/%s/filter-reso-fader-visible" [1],
                         "/%s/filter-q-fader-visible" [1],
                         "/%s/clouds-env-radio" '(0),
                         "/%s/filter-label" ["lpf"],
                         "/%s/filter-reso-fader" [0.0]}
                        (map (fn [[k v]] [(format k player) v]))
                        cast-osc-data
                        (into {})),
   :harmonic-active-voices #{0 1 2},
   :panner-index -4,
   :filter-index 3,
   :harmonic-range {:low -1, :high -1}})

#_(make-synth-defaults "Milo")
(def default-touch-osc-state
  (->> '{"/Milo/bank1-active-label-visible" (0),
         "/Milo/bank2-active-label-visible" (0),
         "/Milo/bank3-active-label-visible" (0),
         "/Milo/bank4-active-label-visible" (0),
         "/Milo/bank5-active-label-visible" (0),
         "/Milo/bank6-active-label-visible" (0),
         "/Milo/bank7-active-label-visible" (0),
         "/Milo/bank8-active-label-visible" (0)
         "/Diego/bank1-active-label-visible" (0),
         "/Diego/bank2-active-label-visible" (0),
         "/Diego/bank3-active-label-visible" (0),
         "/Diego/bank4-active-label-visible" (0),
         "/Diego/bank5-active-label-visible" (0),
         "/Diego/bank6-active-label-visible" (0),
         "/Diego/bank7-active-label-visible" (0),
         "/Diego/bank8-active-label-visible" (0)
         "/Diego/bank-rec-radio" (0),
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
       cast-osc-data
       (#(merge %
                (:touch-osc-data (make-synth-defaults "Milo"))
                (:touch-osc-data (make-synth-defaults "Diego"))))
       (into {})))
(-> @live-state)

(defn init-state!
  []
  (bardo.live-state/init!
   (let [init-player (fn [player]
                       {player (apply merge
                                      {:selected-bank 0}
                                      (map (fn [i]
                                             {i (make-synth-defaults
                                                 (-> player
                                                     name
                                                     str/capitalize))})
                                           (range 8)))})]
     {:algo-2.2.9-clouds ;; TODO: is this key seems unnecessary? At least it is misnamed.
      (merge
       (init-player :milo)
       (init-player :diego))})))

(comment

  (-> default-touch-osc-state))
(defn- get-label-path
  [player label-key]
  (case [player label-key]
    ;; other cases to come
    (format "/%s/%s"
            (case player :milo "Milo" :diego "Diego")
            (str (name label-key) "-label"))))
(comment
  (update-label :milo "harmonic-lowest-note" 100))

(defn- update-label
  "Expects `label-key` to be provided as `:my-label` when in touchosc it is defined as `/player/my-label-label` (note the `-label` suffix)."
  [player label-key value]
  (let [path (get-label-path player label-key)]
    (update-clients @habitat-osc/receiver-clients path [(str value)])
    {:path path :value [(str value)]}))

(defn- send-osc-msg
  [path & values]
  (update-clients @habitat-osc/receiver-clients path values)
  {:path path :value values})

(comment
  (send-osc-msg "/Milo/bank2-active-label-visible" "true")
  (send-osc-msg "/Milo/panner-manual-group" (osc-bool 1)))

(defn reset-default-state!
  []
  (doseq [[path args] default-touch-osc-state]
    (osc-responder {:path path :args args}))
  (init-state!))

(comment

  (->> @live-state)
  (get-selected-synth-data :milo)
  (reset-default-state!)
  (reset! live-state {})
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (throttled-post (dissoc new-value :lorentz))))
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (println new-value))))
