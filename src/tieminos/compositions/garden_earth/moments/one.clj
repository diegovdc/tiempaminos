(ns tieminos.compositions.garden-earth.moments.one
  (:require
   [clojure.string :as str]
   [erv.utils.conversions :as conv]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.analysis :refer [pitch-class->note-set]]
   [tieminos.compositions.garden-earth.fl-grain-1.sample&hold :refer [s&h]]
   [tieminos.compositions.garden-earth.habitat-flores-polinizadores :refer [s&h-reponse-1]]
   [tieminos.compositions.garden-earth.synths.live-signal :refer [pan-verb
                                                                  start-signal-analyzer]]
   [tieminos.compositions.garden-earth.synths.recording :as rec]
   [tieminos.compositions.garden-earth.web.ajax :refer [post-live-state
                                                        post-note-tuning]]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]
   [time-time.dynacan.players.gen-poly :as gp]))

(defonce live-state (atom {}))

(let [last-sets (atom '())]
  (defn on-receive-pitch
    [{:keys [pitch-class original-freq transp eik-freq-ref]
      :as freq-analysis-data}]
    (let [diff-cents (conv/ratio->cents (/ (/ original-freq transp)
                                           eik-freq-ref))
          set* (pitch-class->note-set pitch-class)]

      (when (not= (first @last-sets) set*)
        (swap! last-sets #(take 6 (conj % set*))))
      #_(println pitch-class diff-cents)
      (post-note-tuning (assoc freq-analysis-data
                               :label (format "%s {%s}" pitch-class (str/join "." set*))
                               :last-sets @last-sets
                               :diff-cents diff-cents)))))

;; Signal analyzer
;; Expects the webapp to be running
(defn start-signal-analyzer!
  []
  (start-signal-analyzer {:in 5
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

(defn stop-s&h! []
  (timbre/info :stopping-s&h)
  (gp/stop ::s&h-rain)
  (swap! live-state assoc :s&h.refrain/on? false))
(defn start-s&h!
  []
  (timbre/info :starting-s&h)
  (ref-rain :id ::s&h-rain
            :durs [5 3 8 2 1 5]
            :ratio 1/3
            :on-event (on-event
                       (s&h rec/bufs 0.5 index {:in 5 :play-fn s&h-reponse-1})))
  (swap! live-state assoc :s&h.refrain/on? true))

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
        (-> (o/sound-in 5)
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

  (o/stop)
  (start-signal-analyzer!)

  ;; init live-state
  (add-watch live-state ::post-live-state (fn [_key _ref _old-value new-value] (post-live-state new-value)))

  (pan-verb :in 5 :amp 2 :mix 1 :room 1
            :damp-min 0.6 :damp 0.7
            :pan-min -0.5 :pan 0.5)



  ;; experting default (green lights) oxygen layout
  (when-let [oxygen (get-oxygen!)]
    (midi-in-event
      :midi-input oxygen
      :note-on (fn [{:keys [note]}]
                 (println note)
                 ;; drum pads
                 (cond
                   ;; s&h
                   (and (:s&h.refrain/on? @live-state)
                        (= 36 note))
                   (stop-s&h!)

                   (= 36 note) (start-s&h!)

                   ;; harmonizer
                   (and (:harmonizer/on? @live-state)
                        (= 37 note))
                   (stop-harmonizer!)

                   (= 37 note) (start-harmonizer!))))))
