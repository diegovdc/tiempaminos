(ns tieminos.compositions.garden-earth.moments.one
  "First piece or section from garden earth.
  Audio routing assumes the use of `garden-earth/one.rpp`"
  (:require
   [clojure.string :as str]
   [erv.scale.core :refer [+names]]
   [overtone.core :as o]
   [re-affect.alpha.core :as ræ]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.7d-percusion-ensamble.base :refer [bh]]
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

(defonce ^:private live-state (atom {}))
(ræ/reg-state ::db live-state)
(ræ/defapi ::db)
(comment
  (-> @ræ/states))
(def arp-subcps
  [;; S.0
   ["2)4 of 3)6 11-1.5.7.9"
    "2)4 of 3)6 9-1.5.7.11"]])

(declare live-state make-repeat-cell)

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

(def harmonizer-harmonies
  [;; S.0
   [[0 "3)4 of 3)6 1.3.5.9"]
    [0 "1)4 of 3)6 3.9-1.5.7.11"]
    [2 "1)4 of 3)6 5.9-1.3.7.11"]
    [1 "1)4 of 3)6 1.5-3.7.9.11"]]])

(declare make-harmony)

(defn update-harmonizer-harmony
  [{:keys [harmonizer/harmony-index section] :as state}]
  (let [index (inc (or harmony-index 0))
        [root-deg subcps-name] (->> harmonizer-harmonies
                                    (wrap-at section)
                                    (wrap-at index))
        {:keys [root harmony]} (make-harmony root-deg subcps-name)
        subcps-name* (str/replace subcps-name #"of 3\)6" "")
        pitch-class (-> root :pitch :class)
        set* (->> root :set sort (str/join ".") (#(str "{" % "}")))]
    (assoc state
           :harmonizer/harmony-index index
           :harmonizer/harmony harmony
           :harmonizer/harmony-str (str (into [] harmony) " - " subcps-name* " on " pitch-class " " set*))))

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

(defn update-arp-pattern
  [{:keys [arp/pattern-index section] :as state}]
  (let [index (inc (or pattern-index 0))
        pattern (->> arp-patterns
                     (wrap-at section)
                     (wrap-at index))]
    (assoc state
           :arp/pattern-index index
           :arp/pattern pattern)))

(defn get-arp-pattern-data
  [pattern-index section]
  (let [index pattern-index
        pattern (->> section :arp :patterns
                     (wrap-at index))]
    {:arp/pattern-index index
     :arp/pattern-name (:name pattern)
     :arp/pattern-fn (:fn pattern)}))

(defn update-arp-scale-data
  [{:keys [arp/cps-index section] :as state}]
  (let [index (inc (or cps-index 0))
        subcps-name (->> arp-subcps
                         (wrap-at section)
                         (wrap-at index))
        scale (subcps subcps-name)]
    (assoc state
           :arp/cps-index  index
           :arp/subcps-name subcps-name
           :arp/harmony-strs [(str/replace subcps-name #"of 3\)6" "")
                              (str/join " " (map (comp :class :pitch) scale))]
           :arp/scale scale)))

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

(def sections*
  [{:arp {:cps ["2)4 of 3)6 11-1.5.7.9"
                "2)4 of 3)6 9-1.5.7.11"]
          :patterns (nth arp-patterns 0)}
    :harmonizer {:harmonies (nth harmonizer-harmonies 0)}}])

(def post-live-state-fx {::post-live-state (fn [_ _] {::post-live-state nil})})

(reg-sub ::arp-cps-data
         (fn [{:keys [arp/cps-index section]}]
           (get-arp-scale-data cps-index (wrap-at section sections*)))
         post-live-state-fx)

(reg-sub ::arp-pattern-data
         (fn [{:keys [arp/pattern-index section]}]
           (get-arp-pattern-data pattern-index (wrap-at section sections*)))
         post-live-state-fx)

(reg-sub ::harmonizer-data
         (fn [{:keys [harmonizer/harmony-index section]}]
           (get-harmonizer-data harmony-index (wrap-at section sections*)))
         (merge post-live-state-fx
                {::restart-harmonizer (fn [{:keys [db]} _]
                                        (when (:harmonizer/on? db)
                                          {::start-harmonizer {}}))}))

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

;; Signal analyzer
;; Expects the webapp to be running
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

;;;;;;;;;;;;;;;;;
;;; SAMPLE & Hold
;;;;;;;;;;;;;;;;;

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
                                                           :out (bh 2)})}))))))
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

;;;;;;;;;;;;;;;
;;; Harmonizer
;;;;;;;;;;;;;;;

(defn stop-harmonizer! []
  (timbre/info :stopping-harmonizer)
  (ndef/stop ::harmonizer)
  (swap! live-state assoc :harmonizer/on? false))

(defn make-harmony
  [root-deg subcps-name]
  (let [scale (+names base-freq (subcps subcps-name))
        root (wrap-at root-deg scale)
        root-ratio (:bounded-ratio root)]
    {:root root
     :harmony (->> scale
                   (map (fn [d] (/ (:bounded-ratio d) root-ratio)))
                   (remove #(= 1 %)))}))

(comment
  (make-harmony 0 "3)4 of 3)6 1.3.5.9")
  (:harmonizer/harmony (get-subval ::harmonizer-data)))
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
       {:out (bh 2)}))
    (timbre/error "No :harmonizer/harmony found")))

(comment
  (ndef/ndef
   ::debug-signal-analyzer
   (-> (o/sound-in (ge.route/fl-i1 :in))
       #_(o/delay-l 1 1))
   {:out (bh)}))

(comment
  (-> freq-history)
  (reset! freq-history nil)
  (-> @sc.rec.v1/bufs
      (get ["G#+42" :sample-arp 5])
      (->> (into {}))))

(defn sections
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

(def ^:private initial-state (-> {:section 0}
                                 update-arp-scale-data
                                 update-arp-pattern))

(defn init! []
  (o/stop) (reset! live-state initial-state)
  (ge.init/init!))

(declare reg-event-db dispatch)

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

(reg-fx ::stop-sample-arp (fn [_ _] (stop-sample-arp!)))

(reg-fx ::start-sample-arp (fn [_ _] (start-sample-arp!)))

(reg-fx ::stop-harmonizer (fn [_ _] (stop-harmonizer!)))

(reg-fx ::start-harmonizer (fn [_ _] (start-harmonizer!)))

(defn post-live-state*
  [live-state-data]
  (post-live-state (-> live-state-data
                       (update :arp/pattern :name)
                       (merge (get-subval ::arp-cps-data)
                              {:arp/pattern (:arp/pattern-name (get-subval ::arp-pattern-data))}
                              (get-subval ::harmonizer-data)))))

(reg-fx ::post-live-state
        (fn [{:keys [db]} _] (post-live-state* db)))

(comment
  (ræ/get-state ::db)
  (dispatch {::change-section {:inc? false}})
  (dispatch {::change-section {:inc? true}})

  (get-subval ::arp-cps-data)
  (dispatch {::toggle-sample-arp {}})
  (dispatch {::inc-arp-cps-index {}})
  (dispatch {::inc-arp-pattern-index {}})

  (get-subval ::harmonizer-data)
  (dispatch {::inc-harmonizer-harmony-index {}})
  (dispatch {::toggle-harmonizer {}})

  (post-live-state* @live-state))

(comment
  (init!)
  (-> @live-state)
  (start-signal-analyzer! (ge.route/fl-i1 :in))

  ;; init live-state
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (post-live-state* new-value)))
  (->> @live-state)
  (pan-verb :in (ge.route/fl-i1 :in) :amp 2 :mix 1 :room 1
            :damp-min 0.6 :damp 0.7
            :pan-min -0.5 :pan 0.5)

  ;; Pacer's TIEMI config
  (midi-in-event
   :midi-input (get-pacer!)
   :note-on (fn [{:keys [note]}]
              (println note)
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
                (= 8 note) (dispatch {::inc-harmonizer-harmony-index {}})))))
