(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state
  (:require
   [clojure.math :refer [round]]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.utils.core :refer [round2]]
   [overtone.osc :as osc]
   [taoensso.timbre :as timbre]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.async-events
    :as bardo.comms]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.config
    :as bardo.config]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc-helpers
    :as bardo.osc-helpers]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.habitat.routing :refer [inputs]]
   [tieminos.math.utils :refer [linexp* linlin]]
   [tieminos.utils :refer [wrap-at]]))

(defonce touch-osc-state (atom {}))

(defonce live-state (atom {:lorentz (lorentz/init-system :x (+ 0.3 (rand 0.01))
                                                         :y (+ 0.02 (rand 0.01))
                                                         :z (+ 0.012 (rand 0.01)))}))
(defn init! [data]
  (reset! live-state
          (merge {:lorentz (lorentz/init-system :x (+ 0.3 (rand 0.01))
                                                :y (+ 0.02 (rand 0.01))
                                                :z (+ 0.012 (rand 0.01)))}
                 data)))

(comment
  (->> @live-state)
  (def lorentz (->> @live-state :lorentz))
  (lorentz 2)
  (nth [1 2 3 4] 2))

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

(defn get-independent-banks
  "Get banks that are independent."
  ([player]
   (get-banks! #(and (:on? %) (:independent? %))
               player)))

(defn get-group-banks
  "Get that are not indepedent."
  ([player]
   (get-banks! #(and (:on? %) (not (:independent? %)))
               player)))

(defn get-gusano-banks
  "Get that are not indepedent."
  ([player]
   (get-banks! #(:gusano? %) player)))

(comment [(get-active-banks :milo)
          (get-independent-banks :milo)
          (get-group-banks :milo)
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

(defn set-touchosc-synth-ui
  [player selected-synth-bank
   {:keys [touch-osc-data]
    :as _synth-data}]
  (let [path-base (case player
                    :milo "/Milo"
                    :diego "/Diego")
        bg-color (wrap-at selected-synth-bank bank-colors)]
    (bardo.osc-helpers/update-clients
     @habitat-osc/receiver-clients
     (str path-base "/selected-synth-label")
     [(str "#" (inc selected-synth-bank))
      bg-color])
    (bardo.osc-helpers/update-clients
     @habitat-osc/receiver-clients
     (str path-base "/synth-section-box")
     [bg-color])
    (doseq [[path args] touch-osc-data]
      (println path args)
      (bardo.osc-helpers/update-clients
       @habitat-osc/receiver-clients
       path args))))

(defn osc-bool [bool] (int (if bool 1 0)))

(comment
  (get-in @live-state (synth-bank-path :milo 0)))

(defn init-synth-data
  [player bank]
  (let [path-base (case player
                    :milo "/Milo"
                    :diego "/Diego")
        paths (map #(str path-base %) synth-ui-params)]
    (swap! live-state
           assoc-in
           (synth-bank-path player bank :touch-osc-data)
           (select-keys default-touch-osc-state paths))))

(defn set-selected-bank-synth
  [player bank]
  (let [path (synth-bank-path player :selected-bank)
        state (swap! live-state assoc-in path bank)
        synth-data (get-in state (synth-bank-path player bank))]
    (when (nil? synth-data)
      (init-synth-data player bank))
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

(defn toggle-clouds
  [player on?]
  (let [bank (get-selected-synth-bank player)]
    (toggle-active-bank! player bank on?)
    (show-active-bank-label player on?)
    (bardo.comms/dispatch {:type (if on? :start-clouds :stop-clouds)
                           :data {:player player
                                  :independent? (independent-refrain? player bank)
                                  :bank (get-selected-synth-bank player)}})))

(defn get-refrain-data [player bank]
  (get-player-data player :refrains bank))

(defn refrain-on? [player bank]
  (get-player-data player :refrains bank :on?))

(defn other-refains-on? [player excluded-bank]
  (seq (set/difference (get-group-banks player)
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
  (reset-default-state!)
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

(defn set-synth-index
  [player direction]
  (let [_ (swap! live-state update-in
                 (selected-synth-bank-path player :synth-index)
                 (fnil + 0)
                 direction)
        synth-key (name (set-active-synth player))]
    (update&save-synth-label player :synth synth-key)
    (bardo.osc-helpers/update-label player
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
  {:none {:lpf {:path "/filter-lpf-fader"
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
              :default-value (float 0.5)}}
   :lpf {:lpf {:path "/filter-lpf-fader"
               :default-value (float 1)}
         :hpf {:path "/filter-hpf-fader"
               :visible? false
               :default-value (float 1)}
         :reso {:path "/filter-reso-fader"
                :default-value (float 0)}
         :q {:path "/filter-q-fader"
             :visible? false
             :default-value (float 0.5)}}
   :hpf {:lpf {:path "/filter-lpf-fader"
               :visible? false
               :default-value (float 1)}
         :hpf {:path "/filter-hpf-fader"
               :default-value (float 0)}
         :reso {:path "/filter-reso-fader"
                :default-value (float 0)}
         :q {:path "/filter-q-fader"
             :visible? false
             :default-value (float 0.5)}}
   :moog-ladder {:lpf {:path "/filter-lpf-fader"
                       :default-value (float 1)}
                 :hpf {:path "/filter-hpf-fader"
                       :visible? false
                       :default-value (float 1)}
                 :reso {:path "/filter-reso-fader"
                        :default-value (float 0.5)}
                 :q {:path "/filter-q-fader"
                     :visible? false
                     :default-value (float 0.5)}}
   :moog-ladhp {:lpf {:path "/filter-lpf-fader"
                      :default-value (float 1)}
                :hpf {:path "/filter-hpf-fader"
                      :default-value (float 0)}
                :reso {:path "/filter-reso-fader"
                       :default-value (float 0.5)}
                :q {:path "/filter-q-fader"
                    :default-value (float 0.5)}}
   :moog-hplad {:lpf {:path "/filter-lpf-fader"
                      :default-value (float 1)}
                :hpf {:path "/filter-hpf-fader"
                      :default-value (float 0)}
                :reso {:path "/filter-reso-fader"
                       :default-value (float 0.5)}
                :q {:path "/filter-q-fader"
                    :default-value (float 0.5)}}
   :moog-bp {:lpf {:path "/filter-lpf-fader"
                   :default-value (float 0.4)}
             :hpf {:path "/filter-hpf-fader"
                   :visible? false
                   :default-value (float 1)}
             :reso {:path "/filter-reso-fader"
                    :visible? false
                    :default-value (float 0.5)}
             :q {:path "/filter-q-fader"
                 :default-value (float 0.2)}}})

(def ^:private all-filter-params (->> filter-data vals (apply merge) keys))

(def ^:private filter-keys (keys filter-data))

(defn set-filter-config
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

    (doseq [[path v] osc-msgs] (apply bardo.osc-helpers/send-osc-msg path v))

    (swap! live-state
           (fn [state]
             (-> state
                 (assoc-in (selected-synth-bank-path player :filter-configs active-filter)
                           current-config)
                 (update-in (selected-synth-bank-path player :touch-osc-data)
                            merge
                            osc-msgs))))
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

(defn player-path
  "Creates an OSC path of the form /player/some/path "
  [player param-path]
  (format "%s%s" (case player :milo "/Milo" :diego "/diego")
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
      (bardo.comms/dispatch {:type :delete-bank-bufs
                             :data {:input-k input-k :active-bank active-bank}}))))

(defn delete-all-banks
  [inputs]
  (doseq [bank (range 8)
          input-k inputs]
    (bardo.comms/dispatch {:type :delete-bank-bufs
                           :data {:input-k input-k :active-bank bank}})))

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
   {:type (if on? :start-gusano :stop-stop)
    :data {}}))

(comment
  (-> @live-state)
  (reset! live-state {}))

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

(defn get-harmonic-data!
  [player-k bank]
  (let [data (get-player-data player-k)
        bank-data (get data bank)]
    (assoc bank-data :harmony (:harmony data))))

(comment
  (get-harmonic-data! :milo 0))
