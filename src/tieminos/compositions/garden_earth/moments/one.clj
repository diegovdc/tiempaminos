(ns tieminos.compositions.garden-earth.moments.one
  "First piece or section from garden earth.
  Audio routing assumes the use of `garden-earth/one.rpp`"
  (:require
   [clojure.core.async :as a]
   [clojure.string :as str]
   [erv.scale.core :refer [+names]]
   [overtone.core :as o]
   [re-affect.alpha.core :as ræ]
   [taoensso.timbre :as timbre]
   [tieminos.blackhole :as bh]
   [tieminos.compositions.garden-earth.analysis
    :refer [pitch-class->note-set]]
   [tieminos.compositions.garden-earth.base
    :refer [base-freq interval-from-pitch-class2 subcps]]
   [tieminos.compositions.garden-earth.fl-grain-1.sample-arp
    :refer [arp arp-reponse-2 default-interval-seq-fn]]
   [tieminos.compositions.garden-earth.init :as ge.init]
   [tieminos.compositions.garden-earth.routing :as ge.route]
   [tieminos.compositions.garden-earth.synths.live-signal
    :refer [freq-history pan-verb start-signal-analyzer]]
   [tieminos.compositions.garden-earth.web.ajax
    :refer [post-live-state post-note-tuning]]
   [tieminos.midi.core :refer [get-pacer! midi-in-event]]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.recording.v1 :as sc.rec.v1]
   [tieminos.utils :refer [wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

;;;;;;;;;;;;;;;;;;
;; * Config
;;;;;;;;;;;;;;;;;;

(def outputs
  {:main-synth (bh/bus 20)
   :arp (bh/bus 22)
   :harmonizer (bh/bus 24)})

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Re-affect/State init
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private initial-state
  {:section 0
   :arp.refrain/on? false
   :arp/pattern-index 0
   :arp/cps-index 0
   :harmonizer/on? false
   :harmonizer/harmony-index 0})

(defonce ^:private live-state (atom initial-state))

(ræ/reg-state ::db live-state)
(declare reg-event-db dispatch get-subval reg-event-fx reg-fx reg-sub)
(ræ/defapi ::db)

;;;;;;;;;;;;;;;;;;
;; * Arp
;;;;;;;;;;;;;;;;;;

(declare make-repeat-cell)

(defn simple-pattern
  [pattern pitch-class scale]
  (swap! live-state assoc :arp/pattern-str (str (into [] pattern))) ;; just for the UI's benefit
  (map #(interval-from-pitch-class2 scale pitch-class %)
       pattern))

(defn negate-seq [coll] (map #(* -1 %) coll))

(defn make-seq-range
  "Make a sequenential range, always starting from the zeroth degree.
  If `converge?` then the sequence will be reversed."
  [{:keys [len interval down? converge?]}]
  (let [seq* (range 0 (* interval (max 1 len)) interval)]
    (cond-> seq*
      down? negate-seq
      converge? reverse)))

(comment
  (make-seq-range {:len 0
                   :interval 3
                   :down? true
                   :converge? false}))

(def arp-patterns
  "`:fn` is a function that takes a pitch-class and a scale as arguments and should return a sequence of ratios.
  The `pitch-class` is the one with which the sample was tagged."
  [;; S.0
   [{:name (str [0 2])
     :fn #(make-repeat-cell [0 2] %1 %2 {:max-len 15})}
    {:name (str [0 -2])
     :fn #(make-repeat-cell [0 -2] %1 %2 {:max-len 15})}
    {:name (str [0 3 1 -2])
     :fn #(make-repeat-cell [0 3 1 -2] %1 %2 {:max-len 15})}
    {:name (str [6 -6 -12 -6 0 6])
     :fn #(make-repeat-cell (shuffle [6 -6 -12 -6 0 6]) %1 %2 {:max-len 9})}
    {:name ":div-conv"
     :fn #(simple-pattern
           (make-seq-range {:len (rand-int 5)
                            :interval (inc (rand-int 4))
                            :down? (rand-nth [true false])
                            :converge? (rand-nth [true false])})
           %1 %2)}]])

(comment
  ((-> arp-patterns
       (nth 0)
       (nth 3)
       :fn)
   "A+92"
   (subcps "2)4 of 3)6 11-1.5.7.9")))

(defn get-arp-pattern-data
  [pattern-index section]
  (let [index pattern-index
        pattern (->> section :arp :patterns
                     (wrap-at index))]
    {:arp/pattern-index index
     :arp/pattern-name (:name pattern)
     :arp/pattern-fn (:fn pattern)}))

(defn get-arp-scale-data
  [cps-index section]
  (let [index cps-index
        cps (-> section :arp :cps)
        subcps-name (wrap-at index cps)
        scale (subcps subcps-name)]
    {:arp/cps-index  index
     :arp/subcps-name subcps-name
     :arp/harmony-strs [(str/replace subcps-name #"of 3\)6" "")
                        (str/join " " (map (comp :class :pitch) scale))]
     :arp/scale scale}))

(comment
  (dispatch {::inc-arp-cps-index {}})
  (dispatch {::inc-arp-pattern-index {}})
  (dispatch {::inc-harmonizer-harmony-index {}})
  (get-subval ::arp-cps-data)
  (get-subval ::arp-pattern-data))

(comment
  (add-watch live-state
             ::live-state-portal
             (fn [_ _ _ s] (user/tap :live-state s))))

(defonce ^:private last-sets (atom '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Signal Analyzer
;; Signal analyzer
;; Expects the webapp to be running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn on-receive-pitch
  [{:keys [pitch-class diff-cents]
    :as freq-analysis-data}]
  (let [set* (pitch-class->note-set pitch-class)]
    (when (not= (first @last-sets) set*)
      (swap! last-sets #(take 6 (conj % set*))))
    (when-not pitch-class
      (timbre/error (ex-info "Pitch class not found"  freq-analysis-data)))
    #_(println (format "%s {%s} - %s" pitch-class (str/join "." set*) diff-cents))
    (post-note-tuning (assoc freq-analysis-data
                             :label (format "%s {%s}" pitch-class (str/join "." set*))
                             :last-sets @last-sets
                             :diff-cents diff-cents))))

(defn start-signal-analyzer!
  [in]
  (start-signal-analyzer {:in in
                          :freq 10
                          :analyzer-amp 3
                          :pitch-path "/receive-pitch-5"
                          #_#_:scale-freqs-ranges (make-scale-freqs-ranges
                                                   scale-freqs-map
                                                   (set (map (comp :class :pitch)
                                                             scale-1)))
                          :on-receive-pitch #'on-receive-pitch}))

;;;;;;;;;;;;;;;;;;;;
;; * SAMPLE & Hold
;;;;;;;;;;;;;;;;;;;;

;; NOTE: `ge-live-sig/start-signal-analyzer' should be running

(defn stop-sample-arp! []
  (timbre/info :stopping-arp)
  (gp/stop ::arp-rain))

(defn start-sample-arp!
  []
  (timbre/info :starting-arp)
  (ref-rain :id ::arp-rain
            :durs [5 3 8 2 1 5]
            :ratio 1/3
            :on-event (on-event
                       (let [scale (:arp/scale (get-subval ::arp-cps-data))
                             pattern-fn (:arp/pattern-fn (get-subval ::arp-pattern-data))]
                         (if-not (or scale pattern-fn)
                           (timbre/warn ":arp-rain needs scale and pattern" {:scale scale :pattern pattern-fn})
                           (arp {:bufs-atom sc.rec.v1/bufs
                                 :dur 0.5
                                 :index index
                                 :in (ge.route/fl-i1 :bus)
                                 :play-fn #_(partial #'arp-reponse-1 {:scale scale
                                                                      :out (bh 0)})
                                 (partial #'arp-reponse-2 {:scale scale
                                                           :interval-seq-fn (or pattern-fn default-interval-seq-fn)
                                                           :out (outputs :arp)})}))))))
(defn make-repeat-cell
  [pattern-cell
   pitch-class
   scale
   & {:keys [min-len max-len]
      :or {min-len 3 max-len 9}}]
  (let [len (max min-len (rand-int max-len))
        pattern (->> pattern-cell
                     repeat
                     flatten
                     (take len))]
    ;; just for the UI's benefit
    (swap! live-state assoc :arp/pattern-str (str (into [] pattern)))

    (map #(interval-from-pitch-class2 scale pitch-class %)
         pattern)))

;;;;;;;;;;;;;;;;;
;; * Harmonizer
;;;;;;;;;;;;;;;;;

(defn make-harmony
  [root-deg subcps-name]
  (let [scale (+names base-freq (subcps subcps-name))
        root (wrap-at root-deg scale)
        root-ratio (:bounded-ratio root)]
    {:root root
     :harmony (->> scale
                   (map (fn [d] (/ (:bounded-ratio d) root-ratio)))
                   (remove #(= 1 %)))}))

(defn get-harmonizer-data
  [harmony-index section]
  (let [index harmony-index
        [root-deg subcps-name] (->> section :harmonizer :harmonies
                                    (wrap-at index))
        {:keys [root harmony]} (make-harmony root-deg subcps-name)
        subcps-name* (str/replace subcps-name #"of 3\)6" "")
        pitch-class (-> root :pitch :class)
        set* (->> root :set sort (str/join ".") (#(str "{" % "}")))]
    {:harmonizer/harmony-index index
     :harmonizer/harmony harmony
     :harmonizer/harmony-str (str (into [] harmony) " - " subcps-name* " on " pitch-class " " set*)}))

(defn start-harmonizer! []
  (timbre/info "(re)starting-harmonizer")
  (if-let [ratios (:harmonizer/harmony (get-subval ::harmonizer-data))]
    (do
      (println ratios)
      (ndef/ndef
       ::harmonizer
       (-> (o/sound-in (ge.route/fl-i1 :in))
           #_(o/delay-l 1 1)
           (o/pitch-shift 0.1 ratios)
           ((fn [sig] (if (> (count ratios) 1) (o/mix sig) sig)))
           (o/free-verb 0.5 3)
           (o/pan2)
           (* 8))
       {:out (outputs :harmonizer)}))
    (timbre/error "No :harmonizer/harmony found")))

(defn stop-harmonizer! []
  (timbre/info :stopping-harmonizer)
  (ndef/stop ::harmonizer)
  (swap! live-state assoc :harmonizer/on? false))

;;;;;;;;;;;;;;;;;;
;; * Sections
;;;;;;;;;;;;;;;;;;

(def sections*
  [{:arp {:cps ["2)4 of 3)6 11-1.5.7.9"
                "2)4 of 3)6 9-1.5.7.11"]
          :patterns (nth arp-patterns 0)}
    :harmonizer {:harmonies [[0 "3)4 of 3)6 1.3.5.9"]
                             [0 "1)4 of 3)6 3.9-1.5.7.11"]
                             [2 "1)4 of 3)6 5.9-1.3.7.11"]
                             [1 "1)4 of 3)6 1.5-3.7.9.11"]]}}])

#_(defn sections
    "`config-key` is something like `:arp` or `:harmonizer`.
  All configs should be wrapped in a `fn`"
    [config-key live-state-data]
    (let [sections*
          {0 {:arp (fn [] {:subcps-name (wrap-at (:arp/cps-index live-state-data 0)
                                                 ["2)4 of 3)6 11-1.5.7.9"
                                                  "2)4 of 3)6 9-1.5.7.11"])
                           :interval-seq-fn (partial make-repeat-cell
                                                     (wrap-at (:arp/pattern-index live-state-data 0)
                                                              [[0 2]
                                                               [0 -2]
                                                               [0 3 1 -2]]))})}}]

      ((get-in sections* [(:section live-state-data 0) config-key]))))

;;;;;;;;;;;;;;;;;;
;; * UI
;;;;;;;;;;;;;;;;;;

(defn post-live-state*
  [live-state-data]
  (post-live-state (-> live-state-data
                       (update :arp/pattern :name)
                       (update :synth/main str)
                       (update :synth/signal-analyzer str)
                       (merge (get-subval ::arp-cps-data)
                              {:arp/pattern (:arp/pattern-name (get-subval ::arp-pattern-data))}
                              (get-subval ::harmonizer-data)))))
;;;;;;;;;;;;;;;;;;
;; * Events
;;;;;;;;;;;;;;;;;;

(reg-event-fx
 ::init
 (fn [_ {:keys [midi?]}]
   {:db initial-state
    :fx [[::init.fx]
         (when midi? [::init-midi.fx])]}))

(reg-event-db
 ::change-section
 (fn [db {:keys [inc?]}]
   (let [op (if inc? inc dec)]
     (update db :section op))))

(reg-event-db
 ::inc-arp-cps-index
 (fn [db _]
   (update db :arp/cps-index (fnil inc 0))))

(reg-event-db
 ::inc-arp-pattern-index
 (fn [db _]
   (update db :arp/pattern-index (fnil inc 0))))

(reg-event-db
 ::inc-harmonizer-harmony-index
 (fn [db _]
   (update db :harmonizer/harmony-index (fnil inc 0))))

(reg-event-fx
 ::toggle-sample-arp
 (fn [{:keys [db]} _]
   (let [on? (:arp.refrain/on? db)]
     {:db (assoc db :arp.refrain/on? (not on?))
      :fx (if on?
            {::stop-sample-arp {}}
            {::start-sample-arp {}})})))

(reg-event-fx
 ::toggle-harmonizer
 (fn [{:keys [db]} _]
   (let [on? (:harmonizer/on? db)]
     {:db (assoc db :harmonizer/on? (not on?))
      :fx (if on?
            {::stop-harmonizer {}}
            {::start-harmonizer {}})})))

(def ^:private main-synth-default-params
  {:amp 2
   :min-mix 0.3 :mix 1
   :min-room 0.6 :room 1
   :damp-min 0.6 :damp 0.7
   :pan-min -0.5 :pan 0.5})

(reg-event-fx
 ::start-main-synth
 (fn [{:keys [db]} _]
   (let [sy (:synth/main db)
         sy* (if (o/node-active? sy)
               sy
               (do
                 (timbre/info "Starting main synth")
                 (pan-verb (merge {:in (ge.route/fl-i1 :in)
                                   :out (outputs :main-synth)}
                                  main-synth-default-params))))]
     {:db (assoc db :synth/main sy*)})))

(reg-event-fx
 ::stop-main-synth
 (fn [{:keys [db]} _]
   (let [sy (:synth/main db)]
     {:db (dissoc db :synth/main)
      :fx (when (o/node-active? sy)
            {::stop-synth {:synth sy}})})))

(reg-event-fx
 ::ctl-synth
 (fn [{:keys [db]} {:keys [synth-k params]}]
   (let [sy (get db synth-k)]
     (if-not sy
       (timbre/warn "Synth not found:" synth-k)
       {:fx {::ctl-synth {:synth sy :params params}}}))))

(reg-event-fx
 ::start-signal-analyzer
 (fn [{:keys [db]} _]
   (let [sy (:synth/signal-analyzer db)
         sy* (if (o/node-active? sy)
               sy
               (do
                 (timbre/info "Starting signal analyzer")
                 (:get-signal-pitches-synth (start-signal-analyzer! (ge.route/fl-i1 :in)))))]
     {:db (assoc db :synth/signal-analyzer sy*)})))

;;;;;;;;;;;;;;;;;;
;; * FX
;;;;;;;;;;;;;;;;;;

(reg-fx ::init.fx
        (fn [_ _]
          (timbre/info "(Re)initializing")
          (o/stop)
          (ge.init/init!)
          (add-watch live-state ::post-live-state
                     (fn [_key _ref _old-value new-value]
                       (post-live-state* new-value)))
          (dispatch [[::start-main-synth]
                     [::start-signal-analyzer]])))

(reg-fx ::stop-synth
        (fn [_ {:keys [synth]}]
          (timbre/info "Stopping syth:" synth)
          (o/ctl synth :gate 0)))

(reg-fx ::ctl-synth
        (fn [_ {:keys [synth params]}]
          (when (o/node-active? synth)
            (doseq [[k v] params]
              (o/ctl synth k v)))))

(reg-fx ::stop-sample-arp (fn [_ _] (stop-sample-arp!)))

(reg-fx ::start-sample-arp (fn [_ _] (start-sample-arp!)))

(reg-fx ::stop-harmonizer (fn [_ _] (stop-harmonizer!)))

(reg-fx ::start-harmonizer (fn [_ _] (start-harmonizer!)))

(reg-fx ::post-live-state.subfx
        (fn [{:keys [db]} _]
          ;; sometimes the server seems to choke with two posts in quick sucession
          ;; so we delay the subs update by a little bit
          (a/go
            (a/<! (a/timeout 100))
            (post-live-state* db))))

(reg-fx
 ::init-midi.fx
 (fn [_ _]
   (timbre/info "Initializing MIDI/Pacer")
   (try
     ;; NOTE: using Pacer's TIEMI config
     (midi-in-event
      :midi-input (get-pacer!)
      :note-on (fn [{:keys [note]}]
                 (cond
                    ;; set section
                   (= 2 note) (dispatch {::change-section {:inc? false}})
                   (= 3 note) (dispatch {::change-section {:inc? true}})
                    ;; arp
                   (= 4 note) (dispatch {::toggle-sample-arp {}})

                    ;; arp config
                   (= 5 note) (dispatch {::inc-arp-cps-index {}})
                   (= 6 note) (dispatch {::inc-arp-pattern-index {}})

                    ;; harmonizer
                   (= 7 note) (dispatch {::toggle-harmonizer {}})
                   (= 8 note) (dispatch {::inc-harmonizer-harmony-index {}}))))
     (catch Exception e (timbre/error (.getMessage e))))))

;;;;;;;;;;;;;;;;;;
;; * Subs
;;;;;;;;;;;;;;;;;;

(def post-live-state-fx {::post-live-state (fn [_ _] {::post-live-state.subfx nil})})

(reg-sub ::arp-cps-data
         (fn [{:keys [arp/cps-index section]}]
           (get-arp-scale-data cps-index (wrap-at section sections*)))
         post-live-state-fx)

(reg-sub ::arp-pattern-data
         (fn [{:keys [arp/pattern-index section]}]
           (get-arp-pattern-data pattern-index (wrap-at section sections*)))
         post-live-state-fx)

(reg-sub ::harmonizer-data
         (fn [{:keys [harmonizer/harmony-index section] :as db}]
           (get-harmonizer-data harmony-index (wrap-at section sections*)))
         (merge post-live-state-fx
                {::restart-harmonizer (fn [{:keys [db]} _]
                                        (when (:harmonizer/on? db)
                                          {::start-harmonizer {}}))}))

(comment
  (ræ/get-state ::db)
  (dispatch {::init {:midi? true}})
  (dispatch {::change-section {:inc? false}})
  (dispatch {::change-section {:inc? true}})

  (get-subval ::arp-cps-data)
  (dispatch {::toggle-sample-arp {}})
  (dispatch {::inc-arp-cps-index {}})
  (dispatch {::inc-arp-pattern-index {}})

  (get-subval ::harmonizer-data)
  (dispatch {::inc-harmonizer-harmony-index {}})
  (dispatch {::toggle-harmonizer {}})

  (dispatch {::start-main-synth {}})
  (dispatch {::stop-main-synth {}})
  (dispatch {::ctl-synth {:synth-k :synth/main
                          :params main-synth-default-params}})

  (post-live-state* @live-state))

(comment
  (-> @live-state)
  (->> @live-state)
  (o/stop)

  (-> freq-history)
  (reset! freq-history nil)
  (-> @sc.rec.v1/bufs
      (get ["G#+42" :sample-arp 5])
      (->> (into {}))))
