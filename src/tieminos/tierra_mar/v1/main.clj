(ns tieminos.tierra-mar.v1.main
  "Based on tieminos.compositions.garden-earth.moments.one"
  (:require
   [clojure.string :as str]
   [erv.scale.core :refer [+names]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.blackhole :as bh]
   [tieminos.compositions.7d-percusion-ensamble.base :refer [bh]]
   [tieminos.compositions.garden-earth.analysis
    :refer [pitch-class->note-set]]
   [tieminos.compositions.garden-earth.base
    :refer [base-freq interval-from-pitch-class2 subcps]]
   [tieminos.compositions.garden-earth.init :as ge.init]
   [tieminos.compositions.garden-earth.routing :as ge.route]
   [tieminos.compositions.garden-earth.synths.live-signal
    :refer [freq-history pan-verb start-signal-analyzer]]
   [tieminos.compositions.garden-earth.web.ajax
    :refer [post-live-state post-note-tuning]]
   [tieminos.midi.core :refer [get-pacer! midi-in-event]]
   [tieminos.sc-utils.groups.v1 :as sc.groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.recording.v1 :as sc.rec.v1]
   [tieminos.tierra-mar.v1.arp :as tm.arp]
   [tieminos.tierra-mar.v1.configs :as tm.configs]
   [tieminos.tierra-mar.v1.nubosidad-lorentziana :as tm.nblz]
   [tieminos.tierra-mar.v1.state :as tm.state :refer [state]]
   [tieminos.utils :refer [wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp]))

;;;;;;;;;;;;;;;;;;
;; Main Controls
;;;;;;;;;;;;;;;;;;
(declare init! stop!)

(comment
  (-> @state)
  (init!)
  (stop!)

  (tm.nblz/init!)
  (tm.nblz/start-arp!)
  (tm.nblz/stop!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementations and WIP
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def arp-subcps
  [;; S.0
   ["2)4 of 3)6 11-1.5.7.9"
    "2)4 of 3)6 9-1.5.7.11"
    "1)4 of 3)6 5.9-1.3.7.11"]])

(declare make-repeat-cell)
#_(ns-unmap *ns* 'state)
(defn simple-pattern
  [pattern pitch-class scale]
  ;; just for the UI's benefit
  (swap! state assoc :arp/pattern-str (str (into [] pattern)))
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

(comment
  (-> @state :section))
(def ^:private initial-state (-> {:section 0}
                                 #_(update-arp-scale-data)
                                 #_update-arp-pattern))

(defn init-state! []
  (reset! tm.state/state {})
  (tm.state/set-arp-pattern! tm.state/state (tm.arp/get-pattern ":default"))
  (tm.state/set-arp-scale! tm.state/state (tm.arp/get-scale 0)))

#_(defonce ^:private state (atom initial-state))

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
  (let [analyzer-map
        (start-signal-analyzer {:in in
                                :freq 10
                                :analyzer-amp 3
                                :pitch-path "/receive-pitch-5"
                                #_#_:scale-freqs-ranges (make-scale-freqs-ranges
                                                         scale-freqs-map
                                                         (set (map (comp :class :pitch)
                                                                   scale-1)))
                                :on-receive-pitch #'on-receive-pitch})]
    (swap! state assoc :analyzer analyzer-map)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grain Sample Arp
;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: `ge-live-sig/start-signal-analyzer' should be running

#_(defn stop-sample-arp! []
    (timbre/info :stopping-arp)
    (gp/stop ::arp-rain)
    (swap! state assoc
           :arp.refrain/on? false))
(-> @state)
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
    (swap! state assoc :arp/pattern-str (str (into [] pattern)))

    (map #(interval-from-pitch-class2 scale pitch-class %)
         pattern)))

#_(defn start-sample-arp!
    [{:keys [subcps-name interval-seq-fn]
      :or {interval-seq-fn default-interval-seq-fn}}]
    (timbre/info :starting-arp)
    (ref-rain :id ::arp-rain
              :durs [5 3 8 2 1 5]
              :ratio 1/3
              :on-event (on-event
                         (let [{:keys [arp/scale arp/pattern]} @state]
                           (arp {:bufs-atom sc.rec.v1/bufs
                                 :dur 0.5
                                 :index index
                                 :in (ge.route/fl-i1 :bus)
                                 :play-fn #_(partial #'arp-reponse-1 {:scale scale
                                                                      :out (bh 0)})
                                 (partial #'arp-reponse-2 {:scale scale
                                                           :amp-min 1
                                                           :amp-max 1.5
                                                           :interval-seq-fn (:fn pattern)
                                                           :out (rainseq (++ 25 (map #(mod % 44) (range 0 88 5))))})}))))
    (swap! state assoc :arp.refrain/on? true))

;;;;;;;;;;;;;;;
;;; Harmonizer
;;;;;;;;;;;;;;;

(defn stop-harmonizer! []
  (timbre/info :stopping-harmonizer)
  (ndef/stop ::harmonizer)
  (swap! state assoc :harmonizer/on? false))

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
  (make-harmony 0 "3)4 of 3)6 1.3.5.9"))

(defn start-harmonizer! []
  (timbre/info :starting-harmonizer)
  (if-let [ratios (:harmonizer/harmony @state)]
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
        (swap! state assoc :harmonizer/on? true))
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
  [config-key state-data]
  (let [sections*
        {0 {:arp (fn []
                   {:subcps-name (wrap-at (:arp/cps-index state-data 0) arp-subcps)
                    :interval-seq-fn (partial make-repeat-cell
                                              (wrap-at (:arp/pattern-fn-index state-data 0)
                                                       [[0 2]
                                                        [0 -2]
                                                        [0 3 1 -2]]))
                    :group (sc.groups/early)
                    :out-fn (fn [_i]
                              (tm.configs/get-audio-bus
                               (rand-nth [:arp->nubosidad
                                          :arp->nubosidad2])))})}}]

    #_((get-in sections* [(:section state-data 0) config-key]))
    ((get-in sections* [0 :arp]))))
(comment

  (gp/stop))
(defn toggle-sample-arp!
  []
  (if (:arp.refrain/on? @state)
    (tm.arp/stop-sample-arp!)
    (tm.arp/start-sample-arp!
     (assoc (sections :arp @state)
            :state-atom state)))
  nil)

(comment
  (reset! tm.state/state {})
  (-> @tm.state/state)
  (-> @state)
  (-> @live-state))

(defn midi-ctl
  [{:keys [note]}]
  (cond
    ;; set section
    (= 2 note) (swap! state update :section dec)
    (= 3 note) (swap! state update :section inc)
    ;; arp
    (= 4 note) (toggle-sample-arp!)

    ;; arp config
    (= 5 note) (swap! state tm.state/update-arp-scale-data tm.arp/patterns)
    (= 6 note) (swap! state tm.state/update-arp-pattern arp-subcps)

    ;; harmonizer
    (and (:harmonizer/on? @state)
         (= 7 note))
    (stop-harmonizer!)

    (= 7 note) (start-harmonizer!)
    (= 8 note) (do
                 (swap! state update-harmonizer-harmony)
                 (when (:harmonizer/on? @state)
                   (start-harmonizer!)))))

(comment
  (toggle-sample-arp!)
  (-> @state)
  (midi-ctl {:note 2}) ;; section down
  (midi-ctl {:note 3}) ;; section up
  (midi-ctl {:note 4}) ;; toggle sample-arp
  (midi-ctl {:note 5}) ;; update arp scale data
  (midi-ctl {:note 6}) ;; update arp scale pattern                
  )

(defn stop! []
  (tm.nblz/stop!)
  (o/stop)
  (gp/stop)
  (reset! tm.configs/audio-buses {}))

(defn init! []
  (bh/set-interface! :scarlett)
  (sc.groups/init-groups!)
  (init-state!)
  (ge.init/init!)
  (tm.configs/init-buses!)
  (tm.configs/init-osc-clients!)
  (start-signal-analyzer! (ge.route/fl-i1 :in))
  (add-watch state ::post-state
             (fn [_key _ref _old-value new-value]
               (post-live-state (-> new-value
                                    (update :arp/pattern :name)
                                    (dissoc :analyzer tm.state/synths-key))))))

(comment

  (-> @state)
  (ge.init/init!)
  #_(-> @state)
  ;; TODO: maybe use input bus from (tm.configs/get-bus :fl-main)
  (start-signal-analyzer! (ge.route/fl-i1 :in))

  ;; init state
  (add-watch state ::post-state
             (fn [_key _ref _old-value new-value]
               (post-live-state (-> new-value
                                    (update :arp/pattern :name)
                                    (dissoc :analyzer tm.state/synths-key)))))
  (remove-watch state ::post-state)
  (->> @state)
  (o/kill synth)
  ;; TODO: maybe use input bus from (tm.configs/get-bus :fl-main)
  (def synth (pan-verb :in (ge.route/fl-i1 :in)
                       :amp 1
                       :mix 0.5 :room 4
                       :damp-min 0.3 :damp 0.2
                       :pan-min -0.5 :pan 0.5
                       :out 0))

  ;; Pacer's TIEMI config
  (midi-in-event
   :midi-input (get-pacer!)
   :note-on #'midi-ctl))



