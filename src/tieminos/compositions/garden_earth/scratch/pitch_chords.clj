(ns tieminos.compositions.garden-earth.scratch.pitch-chords
  (:require
   [clojure.string :as str]
   [erv.scale.core :refer [+names]]
   [erv.utils.conversions :as conv]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [bh]]
   [tieminos.compositions.garden-earth.analysis :refer [pitch-class->bounded-ratio
                                                        pitch-class->note-set]]
   [tieminos.compositions.garden-earth.base :refer [base-freq subcps]]
   [tieminos.compositions.garden-earth.synths.live-signal :refer [freq-history
                                                                  start-signal-analyzer]]
   [tieminos.compositions.garden-earth.web.ajax :refer [post-note-tuning]]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [time-time.dynacan.players.gen-poly :as gp]))

(def freq-analysis {:diff-hz -4.962188720703125,
                    :eik-freq 232.03125,
                    :eik-freq-ref 464.0625,
                    :amp 0.008156421,
                    :original-freq 234.51234,
                    :transp 1/2,
                    :timestamp 1724963770271,
                    :input-bus 5})
(let [{:keys [original-freq transp eik-freq-ref]} freq-analysis]
  (conv/ratio->cents (/ (/ original-freq transp)
                        eik-freq-ref)))

(comment

  (def scale-1 (+names base-freq (subcps "1)4 of 3)6 5.9-1.3.7.11")))
  (def scale-2 (+names base-freq (subcps "3)4 of 3)6 1.5.7.9")))



  (-> scale-1)
  (let [last-sets (atom '())]
    (defn on-receive-pitch
      [{:keys [pitch-class original-freq transp eik-freq-ref]
        :as freq-analysis-data}]
      (let [diff-cents (conv/ratio->cents (/ (/ original-freq transp)
                                             eik-freq-ref))
            set* (pitch-class->note-set pitch-class)]

        
        (when (not= (first @last-sets) set*)
          (swap! last-sets #(take 6 (conj % set*))))
        (println pitch-class diff-cents)
        (post-note-tuning (assoc freq-analysis-data
                                 :label (format "%s {%s}" pitch-class (str/join "." set*))
                                 :last-sets @last-sets
                                 :diff-cents diff-cents)))))
  (o/stop)
  (start-signal-analyzer {:in 5
                          :freq 10
                          :pitch-path "/receive-pitch-5"
                          #_ #_  :scale-freqs-ranges (make-scale-freqs-ranges
                                                       scale-freqs-map
                                                       (set (map (comp :class :pitch)
                                                                 scale-1)))
                          :on-receive-pitch #'on-receive-pitch})

  (* 440.0 135/128)
  (ndef/ndef
      ::pitch-chords
      (-> (o/sound-in 5)
          #_(o/delay-l 1 1)
          (o/pitch-shift 0.1 [7/6 4/3 11/12])
          (o/mix)
          (o/free-verb 0.5 3)
          (o/pan2)
          (* 8)))
  (gp/stop)

  (defn intervals-from-pitch-class
    "Generate a seq of intervals as related to the `bounded-ratio` of a `pitch-class`"
    [pitch-class scale]
    (map
      #(/ (:bounded-ratio %) (pitch-class->bounded-ratio pitch-class))
      scale))

  (defn rm-1
    "Remove 1/1"
    [ratios]
    (remove #(== 1 %) ratios))

  (def scales
    "A map of pitch-class to scales (see usage below)"
    {"A+92" scale-1
     "C+59" scale-2
     "D+90" scale-2
     "G#+42" scale-1})
  (rm-1 (intervals-from-pitch-class "C+59" scale-1))

  (ndef/stop ::pitch-chord-control)
  (gp/stop)
  (let [last-pc (atom nil)]
    (gp/ref-rain
      :id ::pitch-chord-control
      :durs [1/5]
      :on-event (gp/on-event
                  (let [new-pc (-> @freq-history first :pitch-class)]
                    (cond
                      (not new-pc) nil
                      (and (not new-pc) (not @last-pc)) nil #_(println "no last-pc")
                      (= new-pc @last-pc)  nil #_(println "same pc")
                      :else (let [scale (->> scale-1
                                             shuffle
                                             ) #_(scales new-pc)
                                  ratios (->> scale
                                              (intervals-from-pitch-class new-pc)
                                              rm-1
                                              (take (inc (rand-int 4)))
                                              (map * [1/2 1 2]))]
                              (println ratios)
                              (reset! last-pc new-pc)
                              (println "new ndef"  @last-pc)
                              (if-not ratios
                                (timbre/error "NO RATIOS=========================================")
                                (ndef/ndef
                                    ::pitch-chords
                                    (-> (o/sound-in 5)
                                        #_(o/delay-l 1 1)
                                        (o/pitch-shift 0.1 ratios)
                                        ((fn [sig] (if (> (count ratios) 1) (o/mix sig) sig)))
                                        (o/free-verb 0.5 0.5)
                                        (o/pan2)
                                        (* 8 (lfo-kr 1 0.5 1)))
                                    {:fade-time 5
                                     :out (bh 2)})))))))))
