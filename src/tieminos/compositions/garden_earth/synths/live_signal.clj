(ns tieminos.compositions.garden-earth.synths.live-signal
  (:require
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [tieminos.compositions.garden-earth.base :as ge-base]))

(defn lfo [freq min* max*]
  (o/lin-lin (o/lf-noise1 freq) -1 1 min* max*))

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
                             (o/pan2 (lfo (rand ) -1 1))
                             (* amp  (o/env-gen (o/env-asr a s r))))))
                  o/mix))))
  #_(o/stop)
  (comment
    (pan-verb :mix 1 :room 1 :amp 2)))



(defonce freq-history (atom nil))

(comment
  (o/demo 5 (o/send-reply (o/impulse 5) "/receive-pitch"
                          (o/pitch (o/sound-in 0))) )
  (-> @freq-history))

(o/defsynth run-get-signal-pitches [freq 5]
  (o/send-reply (o/impulse freq) "/receive-pitch"
                (o/pitch (o/sound-in 0))))

(defn run-receive-pitch
  "Gets pitches from `sound-in` 0 in almost real time
  and `conj`es the data into`freq-history.
  NOTE: `run-get-signal-pitches` must be running`"
  []
  (o/on-event "/receive-pitch"
              (fn [data]
                (let [[_node-id _ freq freq?*] (-> data :args)
                      freq? (= freq?* 1.0)]
                  (when freq?
                    (println freq)
                    (swap! freq-history
                           (comp (partial take 100) conj)
                           (assoc (ge-base/eiko-round-freq freq)
                                  :timestamp (o/now)))))) ::foo))
(comment
  (def signal-pitches (run-get-signal-pitches))
  (run-receive-pitch))
