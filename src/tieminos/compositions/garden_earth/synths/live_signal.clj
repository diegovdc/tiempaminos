(ns tieminos.compositions.garden-earth.synths.live-signal
  (:require
   [clojure.string :as str]
   [overtone.core :as o]
   [tieminos.compositions.garden-earth.base :as ge-base]))

(defn lfo [freq min* max*]
  (o/lin-lin (o/lf-noise1 freq) -1 1 min* max*))

(defn lfo-kr [freq min* max*]
  (o/lin-lin:kr (o/lf-noise1:kr freq) -1 1 min* max*))

(do
  (o/defsynth pan-verb [in 0 out 0
                        a 0.25 s 1 r 0.5
                        amp 1
                        mix-min 0.2 mix 0.5
                        room-min 0.5 room 0.5
                        damp-min 0 damp 0.5]
    (let [input (o/sound-in in)]
      (o/out out
             (->> (range 1)
                  (map (fn [_]
                         (-> input
                             (o/bpf (lfo 4 40 8000)
                                    (lfo 4 0.1 1))
                             (+ (* 0.5 input))
                             (o/free-verb (lfo (rand 0.5) mix-min mix)
                                          (lfo (rand 2) room-min room)
                                          (lfo (rand) damp-min damp))
                             (o/pan2 (lfo (rand) -1 1))
                             (* amp  (o/env-gen (o/env-asr a s r))))))
                  o/mix))))
  #_(o/stop)
  (comment
    (pan-verb :mix 1 :room 1 :amp 2)))

(defonce freq-history (atom nil))

(comment
  (o/demo 5 (o/send-reply (o/impulse 5) "/receive-pitch"
                          (o/pitch (o/sound-in 0))))
  (reset! freq-history nil)
  (-> @freq-history))

(defn run-get-signal-pitches
  [& {:keys [freq in pitch-path]
      :or {freq 5
           in 0
           pitch-path "/receive-pitch"}}]
  ((o/synth
    (let [input (o/sound-in in)]
      (o/send-reply (o/impulse freq) pitch-path
                    [(o/pitch:kr input) (o/amplitude:kr input)]
                    in)))))

(defn run-receive-pitch
  "Gets pitches from `sound-in` 0 in almost real time
  and `conj`es the data into`freq-history.
  NOTE: `run-get-signal-pitches` must be running`"
  [& {:keys [pitch-path]
      :or {pitch-path "/receive-pitch"}}]
  (o/on-event pitch-path
              (fn [data]
                (let [[_node-id input-bus freq freq?* amp] (-> data :args)
                      freq? (= freq?* 1.0)]
                  (when freq?
                    (println "freq: " freq "hz, amp: " amp " @ " pitch-path)
                    #_(println data)
                    (swap! freq-history
                           (comp (partial take 100) conj)
                           (assoc (ge-base/eiko-round-freq freq)
                                  :amp amp
                                  :timestamp (o/now)
                                  :pitch-path pitch-path
                                  :input-bus input-bus)))))
              (keyword (str/replace pitch-path #"/" ""))))

(defn start-signal-analyzer
  [& {:keys [in pitch-path]
      :or {in 0
           pitch-path "/receive-pitch"}}]
  (run-receive-pitch :pitch-path pitch-path)
  (run-get-signal-pitches :in in :pitch-path pitch-path))
(comment
  (o/stop)
  (start-signal-analyzer :in 0 :pitch-path "/receive-pitch-0")
  (start-signal-analyzer :in 1 :pitch-path "/receive-pitch-1")
  (def signal-pitches (run-get-signal-pitches))

  (run-receive-pitch))
