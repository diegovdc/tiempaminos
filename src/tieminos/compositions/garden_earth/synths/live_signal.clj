(ns tieminos.compositions.garden-earth.synths.live-signal
  (:require
   [clojure.string :as str]
   [overtone.core :as o]
   [tieminos.compositions.garden-earth.base :as ge-base :refer [scale-freqs-ranges]]))

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
                          (o/lag2 (o/pitch (o/sound-in 0))
                                  0.3)))
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
                    [(o/lag2 (o/pitch:kr input) 0.3)
                     (o/amplitude:kr input)]
                    in)))))

(defn run-receive-pitch
  "Gets pitches from `sound-in` 0 in almost real time
  and `conj`es the data into`freq-history.
  NOTE: `run-get-signal-pitches` must be running`"
  [& {:keys [pitch-path
             on-receive-pitch
             scale-freqs-ranges]
      :or {pitch-path "/receive-pitch"
           scale-freqs-ranges scale-freqs-ranges}}]
  (println "scale-freqs-ranges" scale-freqs-ranges)
  (o/on-event pitch-path
              (fn [data]
                (let [[_node-id input-bus freq freq?* amp] (-> data :args)
                      freq? (= freq?* 1.0)
                      data (when freq?
                             (assoc (ge-base/eiko-round-freq freq scale-freqs-ranges)
                                    :amp amp
                                    :timestamp (o/now)
                                    :pitch-path pitch-path
                                    :input-bus input-bus))]
                  (when freq?
                    (when on-receive-pitch (on-receive-pitch data))
                    (swap! freq-history
                           (comp (partial take 100) conj) data))))
              (keyword (str/replace pitch-path #"/" ""))))

(defn start-signal-analyzer
  [& {:keys [in freq pitch-path on-receive-pitch scale-freqs-ranges]
      :or {in 0
           freq 5
           scale-freqs-ranges scale-freqs-ranges
           pitch-path "/receive-pitch"}}]
  (run-receive-pitch :pitch-path pitch-path
                     :on-receive-pitch on-receive-pitch
                     :scale-freqs-ranges scale-freqs-ranges)
  (run-get-signal-pitches
    :in in
    :freq freq
    :pitch-path pitch-path))
(comment
  (o/stop)
  (start-signal-analyzer :in 0 :pitch-path "/receive-pitch-0")
  (start-signal-analyzer :in 1 :pitch-path "/receive-pitch-1")
  (def signal-pitches (run-get-signal-pitches))

  (run-receive-pitch))
