(ns tieminos.looper.v1
  (:require
   [overtone.core :as o]
   [tieminos.sc-utils.groups.v1 :as groups]))

;;;;;;;;;;;;;;;;;;
;; * Free Rec
;;;;;;;;;;;;;;;;;;

(def max-buf (o/buffer (* 44100 5)))

(o/defsynth recorder [buf 0 gate 1]
  (let [stop  (o/trig1 (- 1 gate) 0.002)
        pos   (o/integrator (o/k2a gate) 1) ; <-- coef 1, not 0
        phase (min pos (- (o/buf-frames buf) 1))
        _     (o/buf-wr (o/sound-in 5) buf :phase phase)
        final (o/latch (o/a2k pos) stop)
        _     (o/send-reply stop "/recPos" final)]
    ;; TODO add env with Free Action
    (o/out 0 0)))

(comment

  ;; Start recording
  (def rec-synth (recorder :buf max-buf :rec-time 10))
  (o/ctl rec-synth :gate 0)
  (o/defsynth pb [buf 0] (o/out 0 (o/play-buf 1 buf :action o/FREE)))
  (pb max-buf)
  (kill rec-synth)
  ;; Define the handler
  (on-event "/recPos"
            (fn [msg]
              (println msg)
              #_(let [frames-recorded (nth msg 3)]
                  (println "Recorded" frames-recorded "frames")
                  (def short-buf (buffer frames-recorded))
                  ;; Copy on the server
                  (server/snd "/b_copy"
                              (:id short-buf) 0
                              (:id max-buf) 0
                              frames-recorded)))
            ::rec-handler)

  ;; Clean up when done
  (on-synth-stop rec-synth (fn [_] (remove-event-handler rec-handler))))

;;;;;;;;;;;;;;;;;;
;; * Looper
;;;;;;;;;;;;;;;;;;

(o/defsynth ctl-phasor [buf 0 out-bus 0]
  (o/out out-bus
         (o/phasor:ar 1 (o/buf-rate-scale buf) 0 (o/buf-frames buf))))

(o/defsynth buf-writer [in 0 buf 0 phasor-bus 0 gate 1 fade-coef 0.85]
  (let [env   (o/env-gen (o/asr 0.01 1 0.01) :gate gate :action o/FREE)
        sig   (* (o/sound-in in) env)
        phs   (o/in phasor-bus 1)
        mixed (+ sig (* (o/buf-rd 1 buf phs) fade-coef))]
    (o/buf-wr mixed buf phs)
    (o/out 0 0)))

(o/defsynth buf-reader
  [buf 0
   phasor-bus 0
   pan 0
   gate 1
   out 0]
  (let [phs (o/in phasor-bus 1)
        env (o/env-gen (o/asr 0.001 1 0.001) gate 1.0 0.0 1.0 o/FREE)
        sig (* (o/buf-rd 1 buf phs) env)]
    (o/out out (o/pan2 sig pan))))

(comment
  (o/stop)
  (def buf (o/buffer (* 44100 2)))
  (def phasor-bus (o/audio-bus 1))
  (def phasor (ctl-phasor (groups/early) buf phasor-bus))
  (def bw (buf-writer (groups/late) 5  buf phasor-bus))
  (def br (buf-reader (groups/late) buf phasor-bus))
  (o/kill br))

(comment
  (def spl (o/load-sample "/Users/diego/Music/code/tieminos/resources/multitudFeminista.wav"))
  (def br (buf-reader spl phasor-bus))
  (o/demo 3 (o/sound-in 5))
  (o/demo 3 (o/play-buf 1 spl))
  (o/demo 3 (let [buf spl
                  phs (o/line 0 44100 1)
                  env (o/env-gen (o/asr 0.001 1 0.001) 1 1.0 0.0 1.0 o/FREE)
                  sig (o/buf-rd 1 buf phs)]

              (o/pan2 sig 0))))
;; ----
;; Looper factory
;; ----

(defn make-looper [{:keys [in out num-tracks loop-dur]
                    :or {out 0}}]
  (groups/init-groups!)
  (let [phasor-bus (o/audio-bus 1)
        bufs (mapv (fn [_] (o/buffer (* 44100 loop-dur)))
                   (range num-tracks))
        recording-bufs (atom {})
        tracks (atom {})

        rec-track (fn [track-num fade-coef]
                    (let [buf (bufs track-num)]
                      (cond
                        (nil? buf)
                        (println "Track does not exist:" track-num)

                        (@recording-bufs track-num)
                        (println "Already recording track:" track-num)

                        :else
                        (let [synth (buf-writer (groups/early :tail)
                                                :in in
                                                :buf buf
                                                :phasor-bus phasor-bus
                                                :fade-coef fade-coef)]
                          (swap! recording-bufs assoc track-num synth)))))

        stop-rec (fn [track-num]
                   (if-let [synth (@recording-bufs track-num)]
                     (do (o/ctl synth :gate 0)
                         (swap! recording-bufs dissoc track-num))
                     (println "Track not recording:" track-num)))

        play-track (fn [track-num]
                     (let [buf (bufs track-num)
                           track (@tracks track-num)]
                       (cond
                         (nil? buf)
                         (println "Buffer for track does not exist:" track-num)

                         track
                         (println "Track already playing:" track-num)

                         :else
                         (let [synth (buf-reader (groups/early :tail)
                                                 :buf buf
                                                 :phasor-bus phasor-bus
                                                 :out out)]
                           (println "playing track")
                           (swap! tracks assoc track-num synth)))))

        stop-track (fn [track-num]
                     (if-let [track (@tracks track-num)]
                       (do (o/ctl track :gate 0)
                           (swap! tracks dissoc track-num))
                       (println "Track not playing:" track-num)))]

    (ctl-phasor (groups/early :head) :buf (bufs 0) :out-bus phasor-bus)

    (println "Looper Created")

    {:num-tracks num-tracks
     :play-track play-track
     :stop-track stop-track
     :rec rec-track
     :stop-rec stop-rec}))

;; ----
;; Usage
;; ----

(comment
  (require '[tieminos.blackhole :as bh])
  (o/stop)
  (def l (make-looper {:in (bh/bus 3)
                       :out (bh/bus 20)
                       :num-tracks 8
                       :loop-dur 5}))
  ((:play-track l) 2)
  ((:stop-track l) 0)
  ((:rec l) 2 1)
  ((:stop-rec l) 2))
