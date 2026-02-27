(ns tieminos.scratch.heart-beat-memories
  (:require
   [erv.beating-analyzer.v1 :refer [get-beat-data]]
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [erv.utils.core :refer [round2]]
   [erv.utils.ratios :refer [ratios->scale]]
   [overtone.core :as o]
   [tieminos.math.utils :refer [linlin]]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.seq-utils.core :refer [rainseq]]
   [tieminos.seq-utils.utils :refer [bigraph seq->graph]]
   [tieminos.utils :refer [now rrange wrap-at]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2 :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

;; register midi events in reverse chronological order, with delta
;; take a certain amount of recent events
;; 1. be able to
;;    - replay them
;;    - get the intervals and replay rhythm using the beatings
;;
;; 2. get most recent "harmony" and get frequencieds and
;;    - harmonize with beating patterns

(defonce ^:private midi-events (atom ()))

(def ^:private meta-slendro
  "12 tone meta-slendro"
  (->> [49/48 25/24 7/6 19/16 4/3 65/48 3/2 37/24 151/96 7/4 43/24 2/1]
       (ratios->scale)))
(def ^:private scale meta-slendro)

(def ^:private root
  (rationalize (round2 2 (conv/midi->cps 60))))
(defn- +freq-data
  [{:keys [root-hz
           ratio-1 ratio-1-partial
           ratio-2 ratio-2-partial]
    :as pair-data}]
  (let [freq-1 (* root-hz ratio-1 ratio-1-partial)
        freq-2 (* root-hz ratio-2 ratio-2-partial)]
    (assoc pair-data
           ::avg-freq (/ (+ freq-1 freq-2) 2)
           ::freq-1 freq-1
           ::freq-2 freq-2)))

(def ^:private beat-data
  (->> scale
       (map :ratio)
       (get-beat-data 2 root [1 2 3 4 5 6 7 9 11 13])
       (map-indexed #(+freq-data (assoc %2 ::id %1)))
       (filter #(> (::avg-freq %) 60))))

(def ^:private bf-map (group-by :beat-freq.ratio beat-data))

(def ^:private deg-pair->bfs
  (group-by (comp set (juxt :degree-1 :degree-2)) beat-data))

(oe/defsynth sini
  [freq 200
   freq-mul 1
   amp 0.5
   pan 0
   a 1
   s 1
   s-level 1
   r 1
   out 0]
  (o/out out
         (-> (* freq freq-mul)
             o/sin-osc
             (* amp (o/amp-comp freq)
                (lfo-kr 1 0 1)
                (o/env-gen (o/envelope [0 1 s-level 0] [a s r])
                           :action o/FREE))
             (o/pan2 (* (lfo-kr 1 -0.5 0.5) pan)))))

(oe/defsynth glitch-delay
  [in 0
   delay-freq 1
   reps 4
   pan 0
   dur 1
   pitch-shift 1
   band-1 800
   initial-delay 0
   amp 1
   out 0]

  (o/out out
         (-> (o/sound-in [in #_(+ 1 in)])
             (o/delay-l initial-delay)
             (o/comb-l (+ 1 delay-freq) delay-freq  reps)

             #_(o/moog-ladder band-1 0.1)
             (o/bpf band-1 0.2)
             (#(+ %
                  (* 0.3 (o/pitch-shift % 1 pitch-shift))))
             (o/pan2 (+ (lfo-kr 10 -1 1) pan))
             #_(o/free-verb (o/rand 0 1)
                            (o/rand 0 2)
                            (o/rand 0 1))
             (* 1/2  amp (o/env-gen
                          (o/envelope [0 1 1 0] [0.01 (+ initial-delay dur) 0.5])
                          :action o/FREE)))))
(comment
  (def gd (glitch-delay {:in 30
                         :delay-freq 1/4
                         :reps 6
                         :pan 1
                         :amp 2})))

(defonce ^:private events-data (atom ()))

(defn play-pair
  [params
   {:as pair
    beat-hz :beat-freq.hz
    :keys [root-hz
           ::avg-freq
           ratio-1 ratio-1-partial
           ratio-2 ratio-2-partial]}]
  #_(swap! events-data conj pair)
  (when (and (not (zero? beat-hz))
             (> (rand) 0.2)
             #_(< beat-hz 16))
    (println beat-hz)
    (let [delay-freq (/ 1 (/ beat-hz 1) #_(period-reduce 8  beat-hz))
          reps (int (*  (rrange 2 10) beat-hz))
          dur (int (* reps delay-freq))]
      (glitch-delay {:in 30
                     :initial-delay (* (rrand 0 8))
                     :delay-freq  delay-freq
                     ;; NOTE `1` also seems to work well
                     :reps reps
                     :dur dur
                     :pitch-shift (rand-nth [1 2 4 1/2 1/4])
                     :pan (rrange -1 1)
                     :band-1 (rrange 100 2000)
                     :amp (rrange 0.3 8)
                     :out 22})))
  (sini  (merge {:freq (* root-hz ratio-1 ratio-1-partial)
                 :pan -0.5
                 :out 20}
                params))
  (sini (merge {:freq (* root-hz ratio-2 ratio-2-partial)
                :pan 0.5
                :out 20}
               params)))

(comment
  (->> @events-data
       (mapv #(select-keys % [:degree-1 :ratio-1-partial
                              :degree-2 :ratio-2-partial]))
       set
       (into [])))

(defn events->durs
  [events]
  (->> events
       (drop 1)
       (mapv (comp #(/ % 1000.0) :delta))
       ;; NOTE last events has a duration of 1 second
       (#(conj % 1))))

(defn- events-bf-pairs-graph
  [events]
  (->> events
       (map :abs-degree)
       dedupe
       seq->graph
       bigraph))

(comment
  (events-bf-pairs-graph (reverse @midi-events)))

(defn play-beats
  ;; NOTE `events` should already be in chronological order, `midi-events` are un reverse
  [i-offset events]
  #_(println (events->durs events))
  (let [bf-graph (events-bf-pairs-graph events)]
    (ref-rain
     :id (keyword "beats" (str (random-uuid)))
     :durs (events->durs events)
     :loop? false
     :on-event (on-event
                (let [{:keys [abs-degree velocity note]} (nth events i)
                      other-degree (rand-nth (seq (bf-graph abs-degree)))
                      deg-pair (set [abs-degree other-degree])
                      bfs  (deg-pair->bfs deg-pair)
                      freq-mult (rainseq {1 10 2 3})
                      note-freq (float (scale/deg->freq scale (* root #_(pow 2 9)) (- note 60)))
                      bfs* (->> bfs
                                (filter #(>= (* freq-mult (::avg-freq %)) note-freq))
                                #_(map (juxt (comp #(* freq-mult %) float ::avg-freq) :beat-freq.hz)))
                      bf-pair (if (seq bfs*)
                                (wrap-at 2 bfs*)
                                (->> bfs
                                     (sort-by (comp #(* freq-mult %) ::avg-freq))
                                     last))]
                  #_(println note-freq
                             (->> bfs
                                  (filter #(>= (* freq-mult (::avg-freq %)) note-freq))
                                  (map (juxt (comp #(* freq-mult %) float ::avg-freq) :beat-freq.hz))))
                  (if bfs
                    (let [i (+ i i-offset)
                          dur-amp (*  1  dur-s (rainseq [1 2 3 10]))]
                      (doseq [pair [bf-pair (wrap-at (rainseq [6 6 6 5 4]) bfs)]]
                        (play-pair
                         {:a (* dur-amp (rrange  0 1))
                          :s (* dur-amp (rrange 0.1 0.5))
                          :s-level (rrange 0.1 0.5)
                          :r (rrange 0.2 0.4)
                          :freq-mul freq-mult
                          :amp (first (linlin 0 127 0 0.2 [velocity]))}
                         pair)))
                    (println "No bfs for pair:" deg-pair)))))))

(comment
  (play-beats 0 (reverse @midi-events)))

(defn register-midi-event
  [{:as _midi-data
    :keys [note channel velocity]}]
  (let [degree (- note 60)
        scale meta-slendro]
    (swap! midi-events
           (fn [evs]
             (let [prev-now (-> evs first :timestamp)
                   ts (now)]
               (conj evs {:timestamp ts
                          :delta (when prev-now (- ts prev-now))
                          :degree degree
                          :abs-degree (mod degree (count scale))
                          :note note
                          :velocity velocity
                          :channel channel}))))))

;; midi & main `ref-rain`
(comment
  (get-oxygen!)
  (reset! midi-events ())
  (-> midi-events)
  (rain.v2/stop)
  (o/stop)
  (midi-in-event
   :midi-input (get-oxygen!)
   :note-on (fn [{:as midi-data
                  :keys [note channel velocity]}]
              (let [degree (- note 60)
                    scale meta-slendro]
                (register-midi-event midi-data)
                #_(swap! midi-events
                         (fn [evs]
                           (let [prev-now (-> evs first :timestamp)
                                 ts (now)]
                             (conj evs {:timestamp ts
                                        :delta (when prev-now (- ts prev-now))
                                        :degree degree
                                        :abs-degree (mod degree (count scale))
                                        :note note
                                        :velocity velocity
                                        :channel channel}))))

                #_(sy/low2
                   :freq (scale/deg->freq scale (* root #_(pow 2 9)) degree)
                   :amp (first (linlin 0 127 0.3 0.7 [velocity]))
                   :atk (first (linlin 0 127 1.2 0.5 [velocity]))
                   :rel 2)
                []))
   :note-off (fn [{:keys []}]))

  (rain.v2/stop ::main)
  (ref-rain
   :id ::main
   :durs [3]
   :on-event
   (on-event
    (let [events (:events (reduce (fn [{:keys [total-delta events]} event]
                                    (if (> total-delta dur-ms)
                                      (reduced {:events events})
                                      {:total-delta (+ total-delta (:delta event 0))
                                       :events (conj events event)}))
                                  {:total-delta 0
                                   :events ()}
                                  @midi-events))]
      (when (seq events)
        (play-beats i events))))))
