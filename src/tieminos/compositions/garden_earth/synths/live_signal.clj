(ns tieminos.compositions.garden-earth.synths.live-signal
  (:require
   [clojure.string :as str]
   [overtone.core :as o]
   [tieminos.compositions.garden-earth.base :as ge-base :refer [scale-freqs-ranges]]
   [tieminos.utils :refer [hz->ms]]))

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
                        damp-min 0 damp 0.5
                        pan-min -1 pan 1]
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
                             (o/pan2 (lfo (rand) pan-min pan))
                             (* amp  (o/env-gen (o/env-asr a s r))))))
                  o/mix))))
  #_(o/stop)
  (comment
    (pan-verb :mix 1 :room 1 :amp 2)))

(defonce freq-history (atom nil))
(-> @freq-history)
(comment
  (o/demo 5 (o/send-reply (o/impulse 5) "/receive-pitch"
                          (o/lag2 (o/pitch (o/sound-in 0))
                                  0.3)))
  (reset! freq-history nil)
  (-> @freq-history))

(defn run-get-signal-pitches
  [& {:keys [freq in pitch-path analyzer-amp]
      :or {freq 5
           in 0
           pitch-path "/receive-pitch"
           analyzer-amp 1}}]
  (println "SIG ANALIZER Bus:" in "amp:" analyzer-amp)
  ((o/synth
       (let [input  (* analyzer-amp (o/sound-in in))]
         (o/send-reply (o/impulse freq) pitch-path
                       [(o/lag2 (o/pitch:kr input) 0.1) ;; smooth out signal
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
  [& {:keys [in freq pitch-path on-receive-pitch scale-freqs-ranges
             analyzer-amp]
      :or {in 0
           freq 5
           scale-freqs-ranges scale-freqs-ranges
           pitch-path "/receive-pitch"
           analyzer-amp 1}}]
  (run-receive-pitch :pitch-path pitch-path
                     :on-receive-pitch on-receive-pitch
                     :scale-freqs-ranges scale-freqs-ranges)
  (run-get-signal-pitches
    :in in
    :analyzer-amp analyzer-amp
    :freq freq
    :pitch-path pitch-path))


#_(defn add-analysis
  [dur-s buf-key input-bus]
  (let [now (o/now)
        sample-start (- now (* 1000 dur-s)
                        ;; ensure samples window corresponds to dur-s
                        (hz->ms analyzer-freq))
        ;; TODO allow multiple inputs: (get @freq-history (keyword (:name input-bus)))
        analysis (->> @freq-history
                      (drop-while #(> (:timestamp %) now))
                      (take-while #(>= (:timestamp %) sample-start))
                      (reduce (fn [acc {:keys [amp freq freq?]}]
                                (-> acc
                                    (update :min-amp min amp)
                                    (update :max-amp max amp)
                                    (update :amps conj amp)
                                    (cond-> freq? (update :freqs conj freq))))

                              {:min-amp 0
                               :max-amp 0
                               :amps ()
                               :freqs ()}))
        avg-amp (avg (:amps analysis))
        avg-freq? (boolean (seq (:freqs analysis)))
        avg-freq (when avg-freq? (avg (:freqs analysis)))
        amp-norm-mult (normalize-amp (:max-amp analysis))]
    (swap! bufs update buf-key
           assoc
           :rec/time sample-start
           :analysis (-> analysis
                         (assoc :avg-amp avg-amp)
                         (assoc :avg-freq? avg-freq?)
                         (cond-> avg-freq? (assoc :avg-freq avg-freq))
                         (dissoc :amps :freqs))
           :amp-norm-mult amp-norm-mult)))

#_(defn add-amp-analysis
  "For use in thread in the `:on-end` key of `sc.rec.v1/start-recording`"
  [buf-key]
  (add-analysis dur-s buf-key input-bus))

(comment
  (o/stop)
  (start-signal-analyzer :in 0 :pitch-path "/receive-pitch-0")
  (start-signal-analyzer :in 1 :pitch-path "/receive-pitch-1")
  (def signal-pitches (run-get-signal-pitches))

  (run-receive-pitch))
