(ns tieminos.compositions.garden-earth.moments.one
  "First piece or section from garden earth.
  Audio routing assumes the use of `garden-earth/one.rpp`"
  (:require
   [clojure.string :as str]
   [erv.utils.conversions :as conv]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [bh]]
   [tieminos.compositions.garden-earth.analysis
    :refer [pitch-class->note-set]]
   [tieminos.compositions.garden-earth.base :refer [subcps]]
   [tieminos.compositions.garden-earth.fl-grain-1.sample-arp :refer [arp
                                                                     arp-reponse-2
                                                                     pattern-interval-seq-fn]]
   [tieminos.compositions.garden-earth.routing :as ge.route]
   [tieminos.compositions.garden-earth.synths.live-signal
    :refer [pan-verb start-signal-analyzer]]
   [tieminos.compositions.garden-earth.synths.recording :as rec]
   [tieminos.compositions.garden-earth.web.ajax
    :refer [post-live-state post-note-tuning]]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(defonce live-state (atom {}))

(defonce last-sets (atom '()))

(defn on-receive-pitch
  [{:keys [pitch-class original-freq transp eik-freq-ref]
    :as freq-analysis-data}]
  (let [diff-cents (conv/ratio->cents (/ (/ original-freq transp)
                                         eik-freq-ref))
        set* (pitch-class->note-set pitch-class)]

    (when (not= (first @last-sets) set*)
      (swap! last-sets #(take 6 (conj % set*))))
    (when-not pitch-class
      (timbre/error (ex-info "Pitch class not found"  freq-analysis-data)))
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

(defn start-sample-arp!
  [subcps-name]
  (timbre/info :starting-arp)
  (let [scale (subcps subcps-name)]
    (ref-rain :id ::arp-rain
              :durs [5 3 8 2 1 5]
              :ratio 1/3
              :on-event (on-event
                          (arp {:bufs-atom rec/bufs
                                :dur 0.5
                                :index index
                                :in ge.route/fl-i1
                                :play-fn #_(partial #'arp-reponse-1 {:scale scale
                                                                   :out (bh 0)})
                                (partial #'arp-reponse-2 {:scale scale
                                                          :interval-seq-fn (partial
                                                                             pattern-interval-seq-fn
                                                                             [0 2 0 2 0 2 0 2])
                                                          :out (bh 0)})})))
    (swap! live-state assoc
           :arp.refrain/on? true
           :arp.refrain/harmony-strs [(str/replace subcps-name #"of 3\)6" "")
                                      (str/join " " (map (comp :class :pitch) scale))])))

;;;;;;;;;;;;;;;
;;; Harmonizer
;;;;;;;;;;;;;;;

(defn stop-harmonizer! []
  (timbre/info :stopping-harmonizer)
  (ndef/stop ::harmonizer)
  (swap! live-state assoc :harmonizer/on? false))

(defn start-harmonizer! []
  (timbre/info :starting-harmonizer)
  (let [ratios [7/6 4/3 11/12]]
    (ndef/ndef
     ::harmonizer
     (-> (o/sound-in ge.route/fl-i1)
         #_(o/delay-l 1 1)
         (o/pitch-shift 0.1 ratios)
         ((fn [sig] (if (> (count ratios) 1) (o/mix sig) sig)))
         (o/free-verb 0.5 3)
         (o/pan2)
         (* 8)))
    (swap! live-state assoc
           :harmonizer/on? true
           :harmonizer/harmony-str (str (sort ratios)))))

(comment
  (-> @live-state)
  (do (o/stop) (reset! live-state {}))
  (start-signal-analyzer! ge.route/fl-i1)

  ;; init live-state
  (add-watch live-state ::post-live-state (fn [_key _ref _old-value new-value] (post-live-state new-value)))

  (pan-verb :in ge.route/fl-i1 :amp 2 :mix 1 :room 1
            :damp-min 0.6 :damp 0.7
            :pan-min -0.5 :pan 0.5)

;; expecting default (green lights) oxygen layout
  (when-let [oxygen (get-oxygen!)]
    (midi-in-event
     :midi-input oxygen
     :note-on (fn [{:keys [note]}]
                (println note)
                 ;; drum pads
                (cond
                   ;; arp
                  (and (:arp.refrain/on? @live-state)
                       (= 36 note))
                  (stop-sample-arp!)

                  (= 36 note) (start-sample-arp! "2)4 of 3)6 11-1.5.7.9")

                   ;; harmonizer
                  (and (:harmonizer/on? @live-state)
                       (= 37 note))
                  (stop-harmonizer!)

                  (= 37 note) (start-harmonizer!))))))
