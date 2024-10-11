(ns tieminos.compositions.garden-earth.moments.one
  "First piece or section from garden earth.
  Audio routing assumes the use of `garden-earth/one.rpp`"
  (:require
   [clojure.string :as str]
   [erv.scale.core :refer [+names]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [bh]]
   [tieminos.compositions.garden-earth.analysis
    :refer [pitch-class->note-set]]
   [tieminos.compositions.garden-earth.base :refer [base-freq
                                                    interval-from-pitch-class2 subcps]]
   [tieminos.compositions.garden-earth.fl-grain-1.sample-arp :refer [arp
                                                                     arp-reponse-2
                                                                     default-interval-seq-fn]]
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

(def arp-subcps
  [;; S.0
   ["2)4 of 3)6 11-1.5.7.9"
    "2)4 of 3)6 9-1.5.7.11"]])

(declare live-state make-repeat-cell)


(defn simple-pattern
  [pattern pitch-class scale]
  ;; just for the UI's benefit
  (swap! live-state assoc :arp/pattern-str (str (into [] pattern)))
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
  [ ;; S.0
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
   (subcps "2)4 of 3)6 11-1.5.7.9")
   ))

(def harmonizer-harmonies
  [ ;; S.0
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
           :harmonizer/harmony-str (str (into [] harmony) " - " subcps-name* " on " pitch-class " " set*) )))
(defn update-arp-pattern
  [{:keys [arp/pattern-index section] :as state}]
  (let [index (inc (or pattern-index 0))
        pattern (->> arp-patterns
                     (wrap-at section)
                     (wrap-at index))]
    (assoc state
           :arp/pattern-index index
           :arp/pattern pattern)))

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

(def ^:private initial-state (-> {:section 0}
                                 update-arp-scale-data
                                 update-arp-pattern))

(defonce ^:private live-state (atom initial-state))

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

;; NOTE `ge-live-sig/start-signal-analyzer' should be running

(defn stop-sample-arp! []
  (timbre/info :stopping-arp)
  (gp/stop ::arp-rain)
  (swap! live-state assoc
         :arp.refrain/on? false))
(-> @live-state)
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

(defn start-sample-arp!
  [{:keys [subcps-name interval-seq-fn]
    :or {interval-seq-fn default-interval-seq-fn}}]
  (timbre/info :starting-arp)
  (ref-rain :id ::arp-rain
            :durs [5 3 8 2 1 5]
            :ratio 1/3
            :on-event (on-event
                        (let [{:keys [arp/scale arp/pattern]} @live-state]
                          (arp {:bufs-atom sc.rec.v1/bufs
                                :dur 0.5
                                :index index
                                :in (ge.route/fl-i1 :bus)
                                :play-fn #_(partial #'arp-reponse-1 {:scale scale
                                                                     :out (bh 0)})
                                (partial #'arp-reponse-2 {:scale scale
                                                          :interval-seq-fn (:fn pattern)
                                                          :out (bh 2)})}))))
  (swap! live-state assoc :arp.refrain/on? true))

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
  )

(defn start-harmonizer! []
  (timbre/info :starting-harmonizer)
  (if-let [ratios (:harmonizer/harmony @live-state)]
    (do (ndef/ndef
          ::harmonizer
          (-> (o/sound-in (ge.route/fl-i1 :in))
              #_(o/delay-l 1 1)
              (o/pitch-shift 0.1 ratios)
              ((fn [sig] (if (> (count ratios) 1) (o/mix sig) sig)))
              (o/free-verb 0.5 3)
              (o/pan2)
              (* 8))
          {:out (bh 2)})
        (swap! live-state assoc :harmonizer/on? true))
    (timbre/error "No :harmonizer/harmony found")))

(comment
  (ndef/ndef
      ::debug-signal-analyzer
      (-> (o/sound-in (ge.route/fl-i1 :in))
          #_(o/delay-l 1 1)
          )
    {:out (bh )})
  )

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
                                                   (wrap-at (:arp/pattern-fn-index live-state-data 0)
                                                            [[0 2]
                                                             [0 -2]
                                                             [0 3 1 -2]]))})}}]

    ((get-in sections* [(:section live-state-data 0) config-key]))))

(comment
  (do (o/stop) (reset! live-state initial-state))
  (ge.init/init!)
  #_(-> @live-state)
  (start-signal-analyzer! (ge.route/fl-i1 :in))

  ;; init live-state
  (add-watch live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (post-live-state (-> new-value
                                    (update :arp/pattern :name)))))
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
                 (= 2 note) (swap! live-state update :section dec)
                 (= 3 note) (swap! live-state update :section inc)
                 ;; arp
                 (and (:arp.refrain/on? @live-state)
                      (= 4 note))
                 (stop-sample-arp!)

                 (= 4 note) (start-sample-arp! (sections :arp @live-state))

                 ;; arp config
                 (= 5 note) (swap! live-state update-arp-scale-data)
                 (= 6 note) (swap! live-state update-arp-pattern)

                 ;; harmonizer
                 (and (:harmonizer/on? @live-state)
                      (= 7 note))
                 (stop-harmonizer!)

                 (= 7 note) (start-harmonizer!)
                 (= 8 note) (do
                              (swap! live-state update-harmonizer-harmony)
                              (when (:harmonizer/on? @live-state)
                                (start-harmonizer!)))))))
