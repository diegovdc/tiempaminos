(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state
  (:require
   [clojure.math :refer [floor round]]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.utils.core :refer [round2]]
   [org.httpkit.client :as http]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.async-events
    :as bardo.comms]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.config
    :as bardo.config]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.gusanos.harmony
    :refer [gusano-harmonic-seqs gusano-harmonies]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-helpers
    :as bardo.osc-helpers :refer [osc-bool]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths.processors-utils
    :refer [input->in&outs&group]]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.math.utils :refer [linexp* linlin]]
   [tieminos.osc.reaper :as reaper]
   [tieminos.utils :refer [throttle wrap-at]]))

(defonce touch-osc-state (atom {}))

(defonce live-state (atom {}))

(defn init! [data]
  (reset! live-state
          (merge {:lorentz (lorentz/init-system :x (+ 0.3 (rand 0.01))
                                                :y (+ 0.02 (rand 0.01))
                                                :z (+ 0.012 (rand 0.01)))
                  :system/recording? false}
                 data)))

(comment
  (->> @live-state)
  (def lorentz (->> @live-state :lorentz))
  (lorentz 2)
  (nth [1 2 3 4] 2))

;;;;;;;;;;;;;;;;;;
;; * Recording
;;;;;;;;;;;;;;;;;;

(defn start-recording
  []
  (swap! live-state assoc :system/recording? true))

(defn stop-recording
  []
  (swap! live-state assoc :system/recording? false))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Processors
;; Live input processors
;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE this code assumes only the guitar input.
(defn get-preset-modified-params!
  [preset]
  (get-in @live-state [:processors :guitar :preset-params preset]))

(defn get-processor-latest-config! [preset]
  (let [modified-params (->> (get-preset-modified-params! preset)
                             vals
                             (reduce (fn [m {:keys [synth/param synth/value]}]
                                       (assoc m param value))
                                     {}))
        {:keys [input default-config]} preset
        io-config (input->in&outs&group input)]
    (merge default-config io-config modified-params)))

(defn get-processor-active-preset-data!
  []
  (-> @live-state :processors :guitar :active-preset))

(defn set-processor-active-preset!
  [preset synth]
  (swap! live-state assoc-in [:processors :guitar :active-preset] {:preset preset :synth synth}))

(def ^:private processor-preset-index-path
  [:processors :guitar :preset-label-index])

(defn set-processor-preset-label-index!
  "Sets a new preset-label-index by increasing or decreasing it."
  [next?]
  (let [index (-> (swap! live-state update-in processor-preset-index-path (fnil (if next? inc dec) -1))
                  (get-in processor-preset-index-path))
        {:keys [preset]} (get-processor-active-preset-data!)]
    (bardo.comms/dispatch {:type :bardo.processor/on-preset-label-index-change
                           :data {:active-preset preset
                                  :label-index index}})))

(defn set-processor-preset-label-index2!
  "Sets a new preset-label-index to a specific number"
  [index]
  (let [{:keys [preset]} (get-processor-active-preset-data!)]
    (swap! live-state assoc-in processor-preset-index-path index)
    (bardo.comms/dispatch {:type :bardo.processor/on-preset-label-index-change
                           :data {:active-preset preset
                                  :label-index index}})))

(defn activate-processor-preset!
  ([] (activate-processor-preset! (get-in @live-state processor-preset-index-path)))
  ([index]
   (bardo.comms/dispatch {:type :bardo.processor/activate-preset
                          :data {:preset-index index}})))

(defn set-processor-param-value!
  [param-index value]
  (let [{:keys [preset]} (get-processor-active-preset-data!)
        {:keys [param mapping label-mapping]
         :or {label-mapping #(round2 2 %)}
         :as param-data} (-> preset
                             :controls
                             (nth param-index nil))
        synth-value (mapping value)
        data {:touch-osc/param-index param-index
              :touch-osc/value value
              :touch-osc/value-label (str (label-mapping synth-value))
              :synth/param param
              :synth/value synth-value}]
    (if-not param-data
      (timbre/warn "Unknown param-index")
      (do (swap! live-state assoc-in
                 [:processors :guitar :preset-params preset param]
                 data)
          (bardo.comms/dispatch {:type :bardo.processor/on-synth-param-change
                                 :data data})))))

(comment
  (get-processor-active-preset-data!)
  (-> (get-processor-active-preset-data!)
      :preset
      :controls
      first)
  (-> @live-state
      (get-in [:processors :guitar]))
  (-> @live-state
      (get-in [:processors :guitar])
      keys))
(defn processor-update-ui!
  [preset config]
  (timbre/warn "TODO: implement preset UI"))

(comment
  (bardo.osc-helpers/send-osc-msg "/presets/guitar/activate-button-group-visible"
                                  (str false)))

;;;;;;;;;;;;;;;;;;
;; * Synth
;; Sample synths
;;;;;;;;;;;;;;;;;;

(defn synth-bank-path
  [player & keys]
  (concat [:algo-2.2.9-clouds player] keys))

(defn get-player-data
  [player & keys]
  (get-in @live-state (apply synth-bank-path player keys)))

(comment (get-player-data :milo 0))

(defn get-selected-synth-bank
  [player]
  (get-in @live-state (synth-bank-path player :selected-bank) :default-bank))

(defn get-selected-synth-data
  [player]
  (let [bank (get-in @live-state (synth-bank-path player :selected-bank) :default-bank)]
    (get-in @live-state (synth-bank-path player bank))))

(defn selected-synth-bank-path
  [player & keys]
  (apply synth-bank-path player (get-selected-synth-bank player) keys))

(defn save-touchosc-synth-param
  ([player {:keys [path value]}]
   (save-touchosc-synth-param player path value))
  ([player osc-path value]
   (swap! live-state
          assoc-in
          (selected-synth-bank-path player :touch-osc-data osc-path)
          value)))

;;;;;;;;;;;;;;;;;;
;; Banks
;;;;;;;;;;;;;;;;;;

(defn toggle-active-bank!
  [player bank on?]
  (swap! live-state
         assoc-in
         (synth-bank-path player :refrains bank :on?)
         on?))

(defn- get-banks!
  [pred player]
  (->> player
       get-player-data
       :refrains
       (keep (fn [[k v]] (when (pred v) k)))
       set))

(defn get-active-banks
  "Get all active banks, group and independent."
  [player]
  (get-banks! #(:on? %) player))

(defn independent-bank?
  "All banks are independent by default (i.e. if no `:independent?` value has been set)"
  [bank]
  (let [indy? (:independent? bank)]
    (or indy? (nil? indy?))))

(defn get-active-independent-banks
  "Get banks that are independent."
  ([player]
   (get-banks! #(and (:on? %) (independent-bank? %))
               player)))

(defn get-independent-banks
  "Get banks that are independent. All banks are independent by default (i.e. if no `:independent?` value has been set)"
  ([player]
   (get-banks! #(independent-bank? %) player)))

(defn get-active-group-banks
  "Get that are not indepedent."
  ([player]
   (get-banks! #(and (:on? %) (not (independent-bank? %)))
               player)))

(defn get-gusano-banks
  "Get that are not indepedent."
  ([player]
   (get-banks! #(:gusano? %) player)))

(comment [(get-active-banks :milo)
          (get-active-independent-banks :milo)
          (get-active-group-banks :milo)
          (get-gusano-banks :milo)])

(def default-rec-config
  {:on? true
   :pulse :dur
   :dur 0.5})

(defn toogle-rec
  [{:keys [input on?]}]
  (swap! live-state
         assoc-in
         [:rec input]
         (-> default-rec-config
             (merge (-> @live-state :rec input))
             (assoc :on? on?
                    :start-time (System/currentTimeMillis))))

  (bardo.comms/dispatch {:type (if on? :start-recording :stop-recording)
                         :data {:input-k input}}))

(comment
  (toogle-rec {:input :guitar
               :on? true}))

(defn mute-input
  [input-k mute?]
  (if-let [track (bardo.config/reaper-tracks* input-k)]
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

(declare default-touch-osc-state)
(comment
  (bardo.osc-helpers/update-clients @habitat-osc/receiver-clients
                                    "/Milo/selected-synth-label"
                                    ["#1" (rand-nth ["0000FF"
                                                     "FF00FF"
                                                     "FFF0FF"])])
  (bardo.osc-helpers/update-clients @habitat-osc/receiver-clients
                                    "/Milo/synth-section-box"
                                    ["FF00FF33"])

  (bardo.osc-helpers/update-clients @habitat-osc/receiver-clients
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

(defn- set-touchosc-params
  [{:keys [touch-osc-data
           active-filter
           filter-touch-osc-data]
    :as _synth-data}]
  (let [filter-touch-osc-data* (get filter-touch-osc-data active-filter {})]
    (doseq [[path args] (merge touch-osc-data filter-touch-osc-data*)]
      (bardo.osc-helpers/update-clients
       @habitat-osc/receiver-clients
       path args))))

(defn set-touchosc-synth-ui
  [player selected-synth-bank synth-data]
  (let [path-base (case player
                    :milo "/Milo"
                    :diego "/Diego")
        bg-color (wrap-at selected-synth-bank bank-colors)]

    ;; selected synth label, at the top left corner (e.g. "#1")
    (bardo.osc-helpers/update-clients
     @habitat-osc/receiver-clients
     (str path-base "/selected-synth-label")
     [(str "#" (inc selected-synth-bank))
      bg-color])

    ;; synth section box
    (bardo.osc-helpers/update-clients
     @habitat-osc/receiver-clients
     (str path-base "/synth-section-box")
     [bg-color])

    ;; touch osc params
    (set-touchosc-params synth-data)))

(comment
  (get-in @live-state (synth-bank-path :milo 0)))

(defn set-selected-bank-synth
  [player bank]
  (let [path (synth-bank-path player :selected-bank)
        state (swap! live-state assoc-in path bank)
        synth-data (get-in state (synth-bank-path player bank))]
    (set-touchosc-synth-ui player bank synth-data)))

(defn show-active-bank-label [player show?]
  (bardo.osc-helpers/send-osc-msg (format
                                   "/%s/bank%s-active-label-visible"
                                   (-> player name str/capitalize)
                                   (inc (get-selected-synth-bank player)))
                                  (str show?)))

(defn independent-refrain?
  [player bank]
  (contains? (get-independent-banks player) bank))

(defn get-refrain-data [player bank]
  (get-player-data player :refrains bank))

(defn refrain-on? [player bank]
  (get-player-data player :refrains bank :on?))

(defn other-refains-on? [player excluded-bank]
  (seq (set/difference (get-active-group-banks player)
                       #{excluded-bank})))

(defn set-independent-refrain
  [player independent?]
  (let [bank (get-selected-synth-bank player)
        refrain-on? (refrain-on? player 0)
        other-refains-on?* (other-refains-on? player bank)]
    (swap! live-state
           assoc-in
           (synth-bank-path player :refrains bank :independent?)
           independent?)
    (cond
      ;; stop independent refrain if playing
      (and (not independent?) refrain-on?)
      (do (bardo.comms/dispatch {:type :stop-clouds
                                 :data {:player player
                                        :independent? true
                                        :bank (get-selected-synth-bank player)}})
          (when-not other-refains-on?*
            (bardo.comms/dispatch {:type :start-clouds
                                   :data {:player player
                                          :independent? false}})))
      (and independent? refrain-on?)
      (do
        (bardo.comms/dispatch {:type :start-clouds
                               :data {:player player
                                      :independent? true
                                      :bank (get-selected-synth-bank player)}})
        (when-not other-refains-on?*
          (bardo.comms/dispatch {:type :stop-clouds
                                 :data {:player player
                                        :independent? false}})))
      :else nil)))

(defn set-clouds-amp
  [player amp]
  (swap! live-state
         assoc-in
         (selected-synth-bank-path player :amp)
         ;; TODO: lower extra vol
         (first (linlin 0 1 -36 36 [amp]))))

(defn set-clouds-max-dur%
  [player max-dur]
  (swap! live-state
         assoc-in
         (selected-synth-bank-path player :max-dur%)
         max-dur))

(comment
  (-> (get-player-data :milo) (#(apply dissoc % (range 8))))
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

(defn set-active-synth
  "For a given bank, it selects the active synth based on the index of the `synth-keys`"
  [player]
  (let [synth-index (:synth-index (get-selected-synth-data player))
        synth-key (wrap-at synth-index synth-keys)]
    (swap! live-state assoc-in
           (selected-synth-bank-path player :active-synth)
           synth-key)
    synth-key))

(defn update&save-synth-label
  "Updates the label of a synth param.
  Expects `label-key` to be provided as `:my-label` when in touchosc is defined as `/player/my-label-label`, with the `-label` suffix."
  [player label-key value]
  (->> (bardo.osc-helpers/update-label player label-key value)
       (save-touchosc-synth-param player)))

(defn update-active-synth-label
  [player synth-key]
  (bardo.osc-helpers/update-label player
                                  (format "bank%s-active" (inc (get-selected-synth-bank player)))
                                  (str (first (name synth-key)))))

(defn set-synth-index
  [player direction]
  (let [_ (swap! live-state update-in
                 (selected-synth-bank-path player :synth-index)
                 (fnil + 0)
                 direction)
        synth-key (name (set-active-synth player))]
    (update&save-synth-label player :synth synth-key)
    (update-active-synth-label player synth-key)))

(defn toggle-clouds
  [player on?]
  (let [bank (get-selected-synth-bank player)
        synth-key (:active-synth (get-selected-synth-data player))]
    (toggle-active-bank! player bank on?)
    (update-active-synth-label player synth-key)
    (show-active-bank-label player on?)
    (bardo.comms/dispatch {:type (if on? :start-clouds :stop-clouds)
                           :data {:player player
                                  :independent? (independent-refrain? player bank)
                                  :bank (get-selected-synth-bank player)}})))
(comment
  (get-selected-synth-bank :milo)
  (get-selected-synth-data :diego))

(comment
  (linexp* 0 1 40 20000 0))

;;;;;;;;;;;;
;; Filters
;;;;;;;;;;;;

(def ^:private filter-data*
  ;; TODO: find good defaults and proper param mappings, This is just a place holder.
  ;; NOTE: for params to be proporly updated, they should be present in the the particular filter data map. Otherwise the `:path` will be missing and no update will happen.
  [[:none {:lpf {:path "/filter-lpf-fader"
                 :visible? false
                 :default-value (float 1)}
           :hpf {:path "/filter-hpf-fader"
                 :visible? false
                 :default-value (float 0)}
           :reso {:path "/filter-reso-fader"
                  :visible? false
                  :default-value (float 0.5)}
           :q {:path "/filter-q-fader"
               :visible? false
               :default-value (float 0.5)}}]
   [:lpf {:lpf {:path "/filter-lpf-fader"
                :default-value (float 1)}
          :hpf {:path "/filter-hpf-fader"
                :visible? false
                :default-value (float 1)}
          :reso {:path "/filter-reso-fader"
                 :default-value (float 0)}
          :q {:path "/filter-q-fader"
              :visible? false
              :default-value (float 0.5)}}]
   [:hpf {:lpf {:path "/filter-lpf-fader"
                :visible? false
                :default-value (float 1)}
          :hpf {:path "/filter-hpf-fader"
                :default-value (float 0)}
          :reso {:path "/filter-reso-fader"
                 :default-value (float 0)}
          :q {:path "/filter-q-fader"
              :visible? false
              :default-value (float 0.5)}}]
   [:moog-ladder {:lpf {:path "/filter-lpf-fader"
                        :default-value (float 1)}
                  :hpf {:path "/filter-hpf-fader"
                        :visible? false
                        :default-value (float 1)}
                  :reso {:path "/filter-reso-fader"
                         :default-value (float 0.5)}
                  :q {:path "/filter-q-fader"
                      :visible? false
                      :default-value (float 0.5)}}]
   [:moog-ladhp {:lpf {:path "/filter-lpf-fader"
                       :default-value (float 1)}
                 :hpf {:path "/filter-hpf-fader"
                       :default-value (float 0)}
                 :reso {:path "/filter-reso-fader"
                        :default-value (float 0.5)}
                 :q {:path "/filter-q-fader"
                     :default-value (float 0.5)}}]
   [:moog-hplad {:lpf {:path "/filter-lpf-fader"
                       :default-value (float 1)}
                 :hpf {:path "/filter-hpf-fader"
                       :default-value (float 0)}
                 :reso {:path "/filter-reso-fader"
                        :default-value (float 0.5)}
                 :q {:path "/filter-q-fader"
                     :default-value (float 0.5)}}]
   [:moog-bp {:lpf {:path "/filter-lpf-fader"
                    :default-value (float 0.4)}
              :hpf {:path "/filter-hpf-fader"
                    :visible? false
                    :default-value (float 1)}
              :reso {:path "/filter-reso-fader"
                     :visible? false
                     :default-value (float 0.5)}
              :q {:path "/filter-q-fader"
                  :default-value (float 0.2)}}]])
(def ^:private filter-data
  (into {} filter-data*))

(def ^:private all-filter-params (->> filter-data vals (apply merge) keys))

(def ^:private filter-keys (map first filter-data*))

(defn- save-filter-touch-osc-data
  [live-state player osc-msgs]
  (swap! live-state
         (fn [state]
           (-> state
               (update-in (selected-synth-bank-path player :touch-osc-data)
                          merge
                          osc-msgs)))))

(defn set-filter-config
  "Sets the appropriate filter configuration and updates UI"
  [player]
  (let [{:keys [active-filter filter-configs filter-touch-osc-data]} (get-selected-synth-data player)
        filter-data (get filter-data active-filter)
        current-config* (get filter-configs active-filter)
        current-config (->> all-filter-params
                            (map (fn [k]
                                   [k (get current-config* k
                                           (get-in filter-data [k :default-value]))]))
                            (into {}))
        base-path (case player :milo "/Milo" :diego "/Diego")
        osc-msgs (->> current-config
                      (map (fn [[k _v]]
                             (let [{:keys [path default-value]} (k filter-data)
                                   path* (str base-path path)
                                   v* (get-in filter-touch-osc-data [active-filter path*])
                                   visible? (get-in filter-data [k :visible?] true)]
                               {path* (or v* [default-value])
                                (str path* "-visible") [(osc-bool visible?)]})))
                      (apply merge))]

    (save-filter-touch-osc-data live-state player osc-msgs)
    (doseq [[path v] osc-msgs]
      (apply bardo.osc-helpers/send-osc-msg path v)
      ;; set params values via `osc-responder`
      (when-not (str/ends-with? path "-visible")
        (apply bardo.osc-helpers/send-osc-msg-to-self path v)))
    nil))

(defn set-active-filter
  "For a given bank, it selects the active filter based on the index of the `filter-keys`"
  [player]
  (let [filter-index (:filter-index (get-selected-synth-data player))
        filter-key (wrap-at filter-index filter-keys)]
    (swap! live-state assoc-in
           (selected-synth-bank-path player :active-filter)
           filter-key)
    filter-key))

(defn set-filter-index
  [player direction]
  (let [_ (swap! live-state update-in
                 (selected-synth-bank-path player :filter-index)
                 (fnil + 0)
                 direction)
        filter-key (name (set-active-filter player))]
    (set-filter-config player)
    (update&save-synth-label player :filter filter-key)))

(defn set-filter-param
  [player param-k value
   & {:keys [val-fn]
      :or {val-fn identity}}]
  (let [active-filter (:active-filter (get-selected-synth-data player))]
    (swap! live-state
           #(-> %
                (assoc-in (selected-synth-bank-path
                           player
                           :filter-configs
                           active-filter
                           param-k)
                          (val-fn value))))))

(defn save-touchosc-filter-param
  "A variation of `save-touchosc-synth-param` to save the different filter configs"
  [player path args]
  (let [active-filter (:active-filter (get-selected-synth-data player))]
    (swap! live-state
           #(-> %
                (assoc-in (selected-synth-bank-path
                           player
                           :filter-touch-osc-data
                           active-filter
                           path)
                          args)))))

(comment
  (reset! live-state {})
  (get-selected-synth-bank :milo)
  (get-selected-synth-data :diego))

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

(defn player-path
  "Creates an OSC path of the form /player/some/path "
  [player param-path]
  (format "%s%s" (case player :milo "/Milo" :diego "/Diego")
          (if (str/starts-with? param-path "/")
            param-path
            (str "/" param-path))))

(defn set-panner-config
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
        base-path (case player :milo "/Milo" :diego "/Diego")
        groups-visibility-osc (->> panner-keys
                                   (map (fn [k] [(format "%s/panner-%s-group" base-path (name k)) [(osc-bool (= k active-panner))]]))
                                   (into {}))
        osc-msgs (->> (keys panner-data)
                      (map (fn [k]
                             (let [path (str base-path (-> panner-data k :path))]
                               [path (flatten ;; the `:manual` config's value :xy is a list
                                      [(current-config k)])])))
                      (into groups-visibility-osc))]

    (doseq [[path v] osc-msgs] (apply bardo.osc-helpers/send-osc-msg path v))

    (swap! live-state
           (fn [state]
             (-> state
                 (assoc-in (selected-synth-bank-path player :panner-configs active-panner)
                           current-config)
                 (update-in (selected-synth-bank-path player :touch-osc-data)
                            merge
                            osc-msgs))))
    nil))

(defn set-active-panner
  "For a given bank, it selects the active panner based on the index of the `panner-keys`"
  [player]
  (let [panner-index (:panner-index (get-selected-synth-data player))
        panner-key (wrap-at panner-index panner-keys)]
    (swap! live-state assoc-in
           (selected-synth-bank-path player :active-panner)
           panner-key)
    panner-key))

(defn set-panner-index
  [player direction]
  (let [_ (swap! live-state update-in
                 (selected-synth-bank-path player :panner-index)
                 (fnil + 0)
                 direction)
        panner-key (name (set-active-panner player))]
    (set-panner-config player)
    (update&save-synth-label player :panner panner-key)))

(defn set-panner-param
  [player param-k value
   & {:keys [value-fn]
      :or {value-fn identity}}]
  (let [active-panner (:active-panner (get-selected-synth-data player))]
    (swap! live-state
           #(-> %
                (assoc-in (selected-synth-bank-path
                           player
                           :panner-configs
                           active-panner
                           param-k)
                          (value-fn value))
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
  (get-in panner-data [:random :vel :path])
  (get-selected-synth-bank :milo)
  (get-selected-synth-data :milo)
  (:active-panner (get-selected-synth-data :diego)))

;;;;;;;;;;;;;;;;;
;; Envelopes
;;;;;;;;;;;;;;;;

(defn set-active-bank
  [{:keys [player bank on?]}]
  (swap! live-state
         assoc-in
         (synth-bank-path player
                          :refrains
                          (dec bank)
                          :gusano?)
         on?))
(comment
  (-> @live-state  :algo-2.2.9-clouds :milo keys))
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
    (bardo.osc-helpers/update-label player :harmonic-speed (round2 2 speed))))

(defn update-harmonic-range
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

    (bardo.osc-helpers/update-label
     player
     (if low? :harmonic-lowest-note :harmonic-highest-note)
     (get-in state-data (conj path (if low? :low :high))))))

(defn set-harmonic-voice-convergence-point
  [{:keys [player value]}]
  (let [path (selected-synth-bank-path player :harmonic-convergence-point)]
    (swap! live-state assoc-in path value)))

(defn set-active-harmonic-voice
  [{:keys [player voice-index on?]}]
  (swap! live-state update-in
         (selected-synth-bank-path player :harmonic-active-voices)
         (if on? set/union set/difference)
         #{voice-index}))

(defn get-harmonic-voices
  [player bank]
  (get-player-data player bank :harmonic-active-voices))

(comment
  (get-player-data :milo 0))

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
      (bardo.comms/dispatch {:type :bardo.event/delete-bank-bufs
                             :data {:input-ks [input-k] :banks [active-bank]}}))))

(defn delete-all-banks
  [inputs]
  (bardo.comms/dispatch {:type :bardo.event/delete-bank-bufs
                         :data {:input-ks inputs :banks (range 8)}}))

(def default-gusano-config
  {:section 0
   :amp 0.5})

(defn toggle-gusano
  [on?]
  (swap! live-state
         assoc
         :gusano
         (-> default-gusano-config
             (merge (:gusano @live-state))
             (assoc :on? on?)))

  (bardo.comms/dispatch
   {:type (if on? :start-gusano :stop-gusano)
    :data {}}))

(comment
  (-> @live-state)
  (reset! live-state {}))

;;;;;;;;;;;;;;;;;;
;; * Gusano
;;;;;;;;;;;;;;;;;;

;; TODO: set the resulting values of gusano in the live-state just as with the other values

(declare set-gusano-chord-index)

(defn get-gusano-data
  []
  (:gusano @live-state))

(defn- get-gusano-harmonic-seq*
  [{:keys [harmony harmonic-seq-index]
    :as _gusano-data}]
  (let [harmonic-seqs (get gusano-harmonic-seqs harmony)]
    (->> harmonic-seqs
         (wrap-at harmonic-seq-index))))

#_(defn get-gusano-harmony!
    []
    (get-in @live-state [:gusano :harmony]))

(defn get-cached-gusano-harmonic-seq!
  []
  (get-in @live-state [:gusano :harmonic-seq]))

(defn get-harmonic-data!
  [player-k bank]
  (let [data (get-player-data player-k)
        bank-data (get data bank)]
    (assoc bank-data :harmony (:harmony data))))

(let [prev-index (atom 0)]
  (defn- gusano-next-rate-index! [speed]
    (let [prev-i @prev-index]
      (int (floor (reset! prev-index (+ prev-i speed)))))))

(defn- get-gusano-rates-seq-speed!
  []
  (-> @live-state :gusano (:rates-seq-speed 1)))

(defn get-gusano-chord* [rates-seq]
  (let [i (gusano-next-rate-index! (get-gusano-rates-seq-speed!))]
    (set-gusano-chord-index i)
    (wrap-at i rates-seq)))

(defn get-gusano-chord! []
  (get-gusano-chord* (get-cached-gusano-harmonic-seq!)))

(defn inc-gusano-rate-index! []
  (let [i (gusano-next-rate-index! 1)]
    (set-gusano-chord-index i)))

(defn set-gusano-rates-seq-speed
  [i]
  (swap! live-state assoc-in [:gusano :rates-seq-speed] i))

(defn set-gusano-amp
  [amp]
  (swap! live-state assoc-in [:gusano :amp] (first (linlin 0 1 0.01 1.5 [amp]))))

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

(defn- update-gusano-harmonic-index-label!
  [{:keys [harmony harmonic-seq harmonic-seq-index harmonic-seq-item-index]}]
  (bardo.osc-helpers/update-label
   :gusano :harmonic-seq
   (try
     (let [name* (-> harmonic-seq meta :name)
           len (count harmonic-seq)]
       (if name*
         (format "#%s %s - %s"
                 (if-let [harmonic-seq (-> gusano-harmonic-seqs harmony)]
                   (mod harmonic-seq-index (count harmonic-seq))
                   "?")
                 (if harmonic-seq-item-index (str (inc harmonic-seq-item-index) "/" len) "")
                 name*)
         (format "#%s (acordes: %s)"
                 (mod harmonic-seq-index len)
                 (count harmonic-seq))))
     (catch Exception e (do
                          (timbre/error e)
                          "Error with label")))))

(defn set-next-gusano-harmony
  []
  (let [{:keys [harmony-index harmonic-seq-index]
         :or {harmony-index 0
              harmonic-seq-index 0}} (:gusano @live-state)
        i (inc harmony-index)
        next-harmony (wrap-at i gusano-harmonies)
        next-harmonic-seq  (get-gusano-harmonic-seq* {:harmony next-harmony
                                                      :harmonic-seq-index harmonic-seq-index})]
    (swap! live-state update :gusano merge {:harmony-index i
                                            :harmony next-harmony
                                            :harmonic-seq next-harmonic-seq})
    (bardo.osc-helpers/update-label :gusano :harmony (name next-harmony))
    (update-gusano-harmonic-index-label! (get-gusano-data))))

(defn set-next-gusano-harmonic-seq
  []
  (let [{:keys [harmony harmonic-seq-index]
         :or {harmonic-seq-index 0}} (:gusano @live-state)
        harmonic-seqs (get gusano-harmonic-seqs harmony)
        i (-> harmonic-seq-index
              inc
              (mod (count harmonic-seqs)))
        harmonic-seq (get-gusano-harmonic-seq* {:harmony harmony
                                                :harmonic-seq-index i})]
    (swap! live-state update :gusano merge {:harmonic-seq-index i
                                            :harmonic-seq harmonic-seq})
    (update-gusano-harmonic-index-label! (get-gusano-data))))

(defn set-gusano-chord-index
  "For the label"
  [index]
  (let [i (mod index (count (get-cached-gusano-harmonic-seq!)))]
    (swap! live-state assoc-in [:gusano :harmonic-seq-item-index] i)
    (update-gusano-harmonic-index-label! (get-gusano-data))))

(defn set-gusano-chord-as-harmonic-seq
  [chord]
  (swap! live-state update :gusano
         merge
         {:harmonic-seq-index 0
          :harmonic-seq (with-meta [chord]
                          {:name (str "Custom chord: " chord)})})
  (update-gusano-harmonic-index-label! (get-gusano-data)))

(comment
  (get-harmonic-data! :milo 0)
  (set-gusano-chord-as-harmonic-seq [1 2 7/2]))

;;;;;;;;;;;;;;;;;;
;; * Network
;;;;;;;;;;;;;;;;;;

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
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (if print-instead?
                 (timbre/info new-value)
                 (throttled-post (dissoc new-value :lorentz))))))

;;;;;;;;;;;;;;;;;;
;; * Defaults
;;;;;;;;;;;;;;;;;;

(defn cast-osc-data [data]
  (map (fn [[k v]] [k (map #(cond
                              (symbol? %) (eval %) ;; NOTE this may cause trouble
                              (not (number? %)) %
                              (float? %) (float %)
                              :else (int %)) v)])
       data))

(def ^:private gusano-defaults
  {:harmony-index 0
   :harmony :fib
   :harmonic-seq-index -1})

(defn add-player-to-osc-msg-map [player m]
  (map (fn [[k v]] [(format k player) v]) m))

(def synth-osc-defaults
  {"/%s/clouds-active-btn" '(0.0),
   "/%s/clouds-amp" '(0.0),
   "/%s/clouds-env-radio" '(0),
   "/%s/clouds-rhythm-radio" '(1),
   "/%s/clouds-sample-lib-size-radio" '(0),
   "/%s/filter-hpf-fader-visible" [0],
   "/%s/filter-label" ["none"],
   "/%s/filter-q-fader-visible" [0],
   "/%s/filter-reso-fader-visible" [0],
   "/%s/harmonic-voice-cp" '(0),
   "/%s/harmonic-highest-note" '(0.5190911),
   "/%s/harmonic-lowest-note" '(0.48726025),
   "/%s/harmonic-speed" '(0.20449468),
   "/%s/independent-sequencer-btn" [1.0]
   "/%s/max-dur-fader" [1.0]
   "/%s/panner-arrows-group" [0],
   "/%s/panner-label" ["random"],
   "/%s/panner-lissajous-group" [0],
   "/%s/panner-manual-group" [0],
   "/%s/panner-rand-vel-fader" '(0.1),
   "/%s/panner-random-group" [1],
   "/%s/synth-label" ["crystal"],
   "/%s/toggle-harmonic-voice/0" '(1),
   "/%s/toggle-harmonic-voice/1" '(1),
   "/%s/toggle-harmonic-voice/2" '(1),
   "/%s/filter-lpf-fader-visible" [0]})

(def shared-general-ui-osc-defaults
  {"/%s/independent-sequencer-btn" '(1.0)
   "/%s/bank1-active-label-visible" '(0),
   "/%s/bank2-active-label-visible" '(0),
   "/%s/bank3-active-label-visible" '(0),
   "/%s/bank4-active-label-visible" '(0),
   "/%s/bank5-active-label-visible" '(0),
   "/%s/bank6-active-label-visible" '(0),
   "/%s/bank7-active-label-visible" '(0),
   "/%s/bank8-active-label-visible" '(0),
   "/%s/bank-rec-radio" '(0),
   "/%s/harmony-radio" '(0),
   "/%s/clean-master" '(0.0),
   "/%s/processed-master" '(0.0),
   "/%s/rec-durs-radio" '(0),
   "/%s/rec-pulse-radio" '(0),
   "/%s/rev-send-clean" '(0.0),
   "/%s/rev-send-process" '(0.0),
   "/%s/selected-synth-radio" '(0),
   "/%s/toggle-bank/1" '("on" 0.0 "index" 1)})

(def general-ui-osc-defaults
  (merge
   (->> ["Diego" "Milo"]
        (mapcat #(add-player-to-osc-msg-map
                  % shared-general-ui-osc-defaults))
        (into {}))
   {"/EQ/bell-radio" '(0),
    "/EQ/durs-radio" '(0),
    "/EQ/flat-eq" '(0.0),
    "/EQ/hishelf-radio" '(0),
    "/EQ/loshelf-radio" '(0),
    "/EQ/notch-radio" '(0),
    "/gusano/amp" '(0.0),
    "/gusano/durs" '(0),
    "/gusano/grain-dur" '(0.0),
    "/gusano/grain-trig" '(0.0),
    "/gusano/harmony-label" [(-> gusano-defaults :harmony name)]
    "/gusano/period" '(0),
    "/gusano/harmony-up-btn" '(1.0)
    "/gusano/harmonic-seq-speed" '(1.0)
    "/gusano/harmonic-seq-up-btn" '(1.0)
    "/Milo/processes-amp-boost" '(3),
    "/Diego/input-amp-boost" '(0),
    "/System/voces-master" [reaper/zero-db]
    "/System/subwoofer-master" [(reaper/from-db -6)]}))

(defn make-synth-defaults
  [player]
  {:active-filter :none,
   :active-panner :random,
   :max-dur% 1.0,
   :filter-configs {:none {:lpf 1.0, :hpf 0.0, :reso 0.0, :q 0.0}},
   :panner-configs {:random {:vel 0.1}},
   :active-synth :crystal,
   :amp -36.0,
   :sample-lib-size ##Inf,
   :env :lor-1_4,
   :harmonic-speed 1,
   :rhythm :lor-2_6,
   :synth-index 1,
   :touch-osc-data (->> synth-osc-defaults
                        (add-player-to-osc-msg-map player)
                        cast-osc-data
                        (into {})),
   :harmonic-active-voices #{0 1 2},
   :harmonic-convergence-point 0
   :panner-index -4,
   :filter-index 3,
   :harmonic-range {:low -1, :high -1}})

(def default-touch-osc-state
  (->> general-ui-osc-defaults
       cast-osc-data
       (#(merge %
                (:touch-osc-data (make-synth-defaults "Milo"))
                (:touch-osc-data (make-synth-defaults "Diego"))))
       (into {})))

;;;;;;;;;;;;;;;;;;;;;;
;; * Initialization
;;;;;;;;;;;;;;;;;;;;;;

(defn init-state!
  []
  (init!
   (let [init-player (fn [player]
                       {player (apply merge
                                      {:selected-bank 0}
                                      (map (fn [i]
                                             {i (make-synth-defaults
                                                 (-> player
                                                     name
                                                     str/capitalize))})
                                           (range 8)))})]
     {:gusano gusano-defaults
       ;; TODO: is this key seems unnecessary? At least it is misnamed.
      :algo-2.2.9-clouds (merge
                          (init-player :milo)
                          (init-player :diego))})))


