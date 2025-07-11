(ns tieminos.lc.eexxpele3
  (:require
   [clojure.core.async :as async]
   [clojure.data.generators :refer [weighted]]
   [clojure.math :as math]
   [overtone.midi :as midi]
   [tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.utils :refer [subrain]]
   [tieminos.midi.plain-algo-note :refer [algo-note]]
   [tieminos.osc.reaper :as reaper :refer [make-toogle-tracks-fx
                                           unselect-all-tracks]]
   [tieminos.osc.surge :as surge]
   [tieminos.seq-utils.core :refer [** ++ choose lin mirror rainseq xo]]
   [tieminos.seq-utils.parsers.instruments-seqs-parser :refer [evseq]]
   [tieminos.seq-utils.qwerty-velocity :as qwerty]
   [tieminos.utils :refer [cb-interpolate rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]
   [time-time.standard :refer [rrand]]))

(defonce at-atoms (atom {}))

(defn seq-cycle
  [id coll]
  (let [ats @at-atoms
        i* (inc (ats id -1))]
    (swap! at-atoms assoc id i*)
    (wrap-at i* coll)))

(defonce never-arm-envelope?
  (atom false))

(def toggle-fx (do
                 (reaper/init)
                 (make-toogle-tracks-fx)))

(defn toggle-track-arm
  [arm? tracks]
  (async/go
    (reaper/init)
    #_(toggle-fx arm? tracks)
    (async/<! (async/timeout 500))
    (doseq [track tracks]
      (reaper/set-track-rec track arm?)
      (if (and arm? (not @never-arm-envelope?))
        (reaper/set-autowrite track)
        (reaper/set-autotrim track)))
    (unselect-all-tracks)))

(comment (toggle-track-arm true scene-1-tracks)
         (init-main-scene-track-volumes!))

(defn fade-track
  [{:keys [track dur-ms db on-end]}]
  (reaper/init)
  (cb-interpolate {:id (keyword "track" (str track))
                   :dur-ms dur-ms
                   :tick-ms 50
                   :init-val (reaper/from-db 0)
                   :target-val (reaper/from-db db)
                   :cb (fn [{:keys [val]}]
                         (reaper/set-vol track val))
                   :on-end (fn [_] (when (fn? on-end) (on-end)))}))

(def scene-1-tracks (range 1 5))

(def scene-2-tracks (range 5 12))
(def scene-3b-tracks (range 12 22))
(def scene-4-tracks (range 22 33))
(def interlude-1-tracks [34])
(def interlude-2-tracks [35])
(def all-tracks (concat scene-1-tracks
                        scene-2-tracks
                        scene-3b-tracks
                        scene-4-tracks
                        interlude-1-tracks
                        interlude-2-tracks))

(defn init-main-scene-track-volumes!
  []
  (let [scenes (reverse [scene-1-tracks
                         scene-2-tracks
                         scene-3b-tracks
                         scene-4-tracks
                         interlude-1-tracks
                         interlude-2-tracks])]
    (async/go
      ;; NOTE using a go block so that fade-tracks only affects a single track (so that no selected track lingers somewhere)
      (unselect-all-tracks)
      (doseq [tracks scenes]
        (async/<! (toggle-track-arm false tracks))
        (async/<! (async/timeout 100))
        (fade-track {:track (first tracks) :dur-ms 100 :db :-inf})
        (async/<! (async/timeout 300))))))

(defn init!
  []
  ;; TODO: initialize stuff from viejo vago brujo
  (midi/midi-sinks)
  (def sink (midi/midi-out "VirMIDI"))
  (def sink2 (midi/midi-out "VirMIDI Bus 2"))
  (def sink3 (midi/midi-out "VirMIDI Bus 3"))
  (surge/init)
  (init-main-scene-track-volumes!))

(comment
  ;; init ;; will create the midi-sinks as well
  (init!)
  (reaper/rec)

  ;; development
  (reset! never-arm-envelope? true)
  (doseq [t all-tracks] (reaper/select-track t true))
  (reaper/remove-all-envelopes)
  (doseq [t all-tracks] (reaper/select-track t false))

  (toggle-fx all-tracks true)
  (toggle-fx all-tracks false)

  (gp/stop)
  (rain.v2/stop))

(comment
  (reaper/rec)
  (reaper/stop)
  ;; Scene 1
  (toggle-track-arm true scene-1-tracks)
  (fade-track {:track (first scene-1-tracks) :dur-ms 5000 :db 0})

  (gp/ref-rain
   :id :s1/bd
   :tempo 40
   :durs [1/8]
   :on-event (gp/on-event
              (when (#{0 2 6 #_1} (mod i 8)) ;; #{0 2 #_6}
                  ;; bd
                (algo-note {:sink sink
                            :dur (rainseq {1 3 2 1 1/4 1 4 4})
                            :vel (min 127 (int (* 12 (at-i [3 4 5 3]))))
                            :chan 0
                            :offset 50
                            :tempo 120
                            :note (rainseq (++ [0 0 10 {0 3 12 1}
                                                (choose 0 3 2)
                                                0 -5 0 2]
                                               (lin :id/bd [1 {3 5 6 1} 2 4 6 {9 5 7 1}])))}))))
  (gp/ref-rain
   :id :s1/glitch-pluck
   :tempo 90
   :durs [4/3 1 1/2]
   :on-event (gp/on-event
              (when (> 0.5 (rand))
                (algo-note {:sink sink
                            :dur (at-i [1/7 3/2 4])
                            :vel (min 127 (int (* 12 (rand-nth [3 8 10 4 5 3]))))
                            :chan 1
                            :offset (weighted {80 4
                                               71 10
                                               75 3
                                               50 6})
                            :tempo 120
                            :note (weighted {(seq-cycle :s1/gp [1]) 5
                                             (- (rand-int 20) 20) 2})}))))
  (gp/ref-rain
   :id :s1/glitch-pluck2-random-ascent
   :tempo 90
   :durs (flatten [(concat (repeat 5 1/10) [(inc (rand-int 5))])
                   #_(concat (repeat 5 5/10) [(inc (rand-int 5))])
                   (concat (repeat 9 1/8) [(inc (rand-int 5))])
                   (concat (repeat 20 1/9) [(inc (rand-int 5))])
                   (concat (repeat 10 1/11) [(inc (rand-int 5))])
                   (concat (repeat 6 1/17) [(inc (rand-int 5))])])
   :on-event (gp/on-event
              (algo-note {:sink sink
                          :dur (weighted {1/8 18 1/2 1/5})
                          :vel (min 127 (int (* 16 (rand-nth [3 8 10 4 5 3]))))
                          :chan 1
                          :offset (rand-nth [0 3 8 10 12 40])
                          :tempo 120
                          :note (rainseq (mapcat (fn [x] (map #(+ x %)
                                                              (concat (range 50 (rrand 58 70))
                                                                      (reverse (range 50 (rrand 58 70))))))
                                                 [0 10 -10 -4 8]))})))

  (gp/stop :s1/bd)
  (gp/stop :s1/glitch-pluck)
  (gp/stop :s1/glitch-pluck2-random-ascent)
  (fade-track {:track (first scene-1-tracks) :dur-ms 60000 :db :-inf
               :on-end (fn [] (toggle-track-arm false scene-1-tracks))})
  (gp/stop))

(comment
  ;; Scene 2

  (toggle-track-arm true scene-2-tracks)
  (fade-track {:track (first scene-2-tracks) :dur-ms 1000 :db 0})

  (rain.v2/ref-rain
   :id :s2/bd
   :tempo 90
   :durs [1]
   :on-event (rain.v2/on-event
              (algo-note {:sink sink2
                          :dur (at-i [1])
                          :vel (min 127 (+ 1 (int (* 14 (at-i [3 4 5 3])))))
                          :chan 0
                          :offset (rainseq [60 30])
                          :tempo 90
                          :note [0 12 18]})))
  (rain.v2/ref-rain
   :id :s2/bd-child
   :ref :s2/bd
   :tempo 90
   :durs [1/4]
   :on-event (rain.v2/on-event
              (when (#{0 3 5 7 8} (mod i 10))
                (algo-note {:sink sink2
                            :dur (at-i [1])
                            :vel (min 127 (int (* 8 (seq-cycle :bd/vel [10 14 15 3]))))
                            :chan 2
                            :offset (seq-cycle :bd/offset [70 70 70 70 70
                                                           72 72 72])
                            :tempo 90
                            :note (seq-cycle :bd [1 2 3])}))))

  (gp/stop)
  (rain.v2/stop)
  (rain.v2/ref-rain
   :id :s2/bass
   :ref :s2/bd
   :tempo 90
   :durs [1 1/4 1 1 1/2 2 1]
   :on-event (rain.v2/on-event
              (when (xo "xooxooxo" i)
                (let [note (+  (rainseq (concat [[0 2 0 4 7]]
                                                [0 2 1 4 7]
                                                [0 2 1 4 7]
                                                [0 2 0 4 10])))]
                  (subrain
                   {:ref :s2/bass
                    :ratio 1/4
                    :tempo 90
                    :durs (repeat (int (rainseq (** 16 (choose 1 2 3)))) 1)
                    :on-event (rain.v2/on-event
                               (algo-note {:sink sink2
                                           :dur (* 0.05 dur-s)
                                           :vel (math/round (* 127 (rainseq (lin (keyword "id" (str (random-uuid)))
                                                                                 1 1/2 1/4 3/5
                                                                                 0 1/2 0 1
                                                                                 1/8 1/2 0 2/3
                                                                                 1/3 3/4 1/9 0))))
                                           :chan 1
                                           :offset 60
                                           :tempo 90
                                           :note note}))})))))
  (rain.v2/ref-rain
   :id :s2/bass
   :ref :s2/bd
   :tempo 90
   :durs [1 1/4 1 1 1/2 2 1]
   :on-event (rain.v2/on-event
              (when (> (rand) 0.4)
                (algo-note {:sink sink2
                            :dur (rainseq {1 6 3 6 4 1 10 3})
                            :vel (min 127 (int (* 8 (at-i [3 4 5 3]))))
                            :chan 1
                            :offset 50
                            :tempo 90
                            :note (rainseq (concat [0 2 0 4 7]
                                                   [0 2 1 4 7]
                                                   [0 2 1 4 7]
                                                   [0 2 0 4 7]))}))))

  ;; Controlar env-amp, wavshaper, y hpf, feedback, fm
  (rain.v2/stop)
  (rain.v2/ref-rain
   :id :s2/cianningies
   :ref :s2/bd
    ;; will break if not initialized with a :durs vector
   :durs [1/4]
   :on-event (rain.v2/on-event
              #_(when (#{0 2 4 6 8 10 12 13} (mod i 14)))
              (algo-note {:sink sink2
                          :dur 1/8
                          :vel (rainseq (++ {0 16 7 1 10 1/2} (qwerty/midi 80 110 "ajroldp")))
                          :chan 3
                          :offset 62
                          :tempo 90
                          :note (rainseq (++ {0 16, 2 3, 1 1, -2 1, -1 1/2}
                                             {0 8, 7 4, 12 2, 14 1, 13 1, 16 1/2}))})))
  (rain.v2/stop)
  (rain.v2/ref-rain
   :id :s2/cianningies
   :ref :s2/bd
   :durs [1/4]
   :on-event (rain.v2/on-event
              (subrain {:ref :s2/cianningies
                        :durs (repeat (rainseq {1 0 2 4 3 2 4 1
                                                  ;; 10 1/2
                                                })
                                      (rainseq {1/4 3 1/3 1 2/3 1
                                                  ;; 1/5 1/5
                                                }))
                        :delay (rainseq {0 15 1/3 1 1/4 2})
                        :on-event (rain.v2/on-event
                                   (algo-note {:sink sink2
                                               :dur (rainseq (++ 1/16 {0 10 1/5 1 1/7 2 (rrange -0.5 0.5) 3}))
                                               :vel (rainseq (++ [18 0 0]
                                                                 (** {1 8 1/2 3}
                                                                     (qwerty/midi 10 90 "p4b16p"))))
                                               :chan 3
                                               :offset 75
                                               :tempo 90
                                               :note (rainseq (++ {0 5 -2 1 2 1 10 1 -10 1/2}
                                                                  (lin (choose 0 3 -1)
                                                                       (choose 5 8 13))))}))})))

  (rain.v2/stop :s2/bd)
  (rain.v2/stop :s2/bd-child)
  (rain.v2/stop :s2/bass)
  (rain.v2/stop :s2/cianningies)
  (fade-track {:track (first scene-2-tracks) :dur-ms 5000 :db :-inf
               :on-end (fn [] (toggle-track-arm false scene-2-tracks))}))

;;;;;;;;;;;;;;;;;;
;; interludio 1
;;;;;;;;;;;;;;;;;;

(comment
  (toggle-track-arm true interlude-1-tracks)
  (fade-track {:track (first interlude-1-tracks) :dur-ms 2000 :db 0})

  (fade-track {:track (first interlude-1-tracks) :dur-ms 5000 :db :-inf
               :on-end (fn [] (toggle-track-arm false interlude-1-tracks))}))

(comment
  (gp/stop)
  ;; s4 viejo vago brujo

  (toggle-track-arm true scene-4-tracks)
  (fade-track {:track (first scene-4-tracks) :dur-ms 5000 :db 0})
  (fade-track {:track (first scene-4-tracks) :dur-ms 5000 :db :-inf
               :on-end (fn [] (toggle-track-arm false scene-4-tracks))}))

;;;;;;;;;;;;;;;;;;;
;; interludio 2
;;;;;;;;;;;;;;;;;;

(comment
  (toggle-track-arm true interlude-2-tracks)
  (fade-track {:track (first interlude-2-tracks) :dur-ms 2000 :db 0})

  (fade-track {:track (first interlude-2-tracks) :dur-ms 5000 :db :-inf
               :on-end (fn [] (toggle-track-arm false interlude-2-tracks))}))

(comment

  (fade-track {:track (first scene-3b-tracks) :dur-ms 5000 :db 0})
  (toggle-track-arm true scene-3b-tracks)
  (toggle-track-arm false scene-3b-tracks)
  ;; Scene 3b
  (gp/ref-rain
   :id :s3/main
   :tempo 120
   :durs [1/4]
   :on-event (gp/on-event
              (let [config {:sink sink3
                            :dur (at-i [1 1/3 1/3])
                            :vel (min 127 (int (* 12 (at-i [10 4]))))
                            :chan 0
                            :offset (at-i [50 52])
                            :tempo 120
                            :note 0}
                    bd1 (fn [] (algo-note (assoc config :chan 0 :note 0)))
                    bd2 (fn [] (algo-note (assoc config :chan 1 :note 0)))
                    sn (fn [] (algo-note (assoc config :chan 2 :note 0)))
                    hh (fn [] (algo-note (assoc config :chan 3 :note 0)))]
                (evseq "cc" #_(str "aca aba aba cae"
                                   "aca aca abe cae"
                                   "aca aea aba eae"
                                   "aca aba aba cae"
                                   "aca aca abe cbb"
                                   "aca aea aba eae"
                                   "bcb abb aca bb"
                            ;; "bca aba acab ac"
                                   )
                       "a" (bd1)
                       "b" (bd2)
                       "e" (hh))

                (evseq "c" #_(str "cedd cedc ddee cd"
                                  "cccc cdcc dccc dd"
                                  "eddc cedc edde dc"
                                  "cccc ccd ddcc cc"
                                  "cccc dddd ccdd dd"
                                  "cccc cccc "
                                  "dddd dddd")
                       "d" (sn)
                       "e" (hh))

                #_(evseq (str "mcccccccmccccc"
                              "ccccccmccccccc"
                              "cccnccccmccccc"
                              "cmcncccccccccc")
                         "m" (algo-note (assoc config :chan 4 :offset 60
                                               :vel (min 127 (int (* 12 (at-i [10 4 10]))))
                                               :note (at-i [0 4 5 7 -10 11 17])))
                         "n" (algo-note (assoc config :chan 4 :offset 70
                                               :dur (at-i [1 1/3 1/2])
                                               :vel (min 127 (int (* 12 (at-i [10 4]))))
                                               :note (seq-cycle :s3/n [10 16 15 10 5]))))
                #_(evseq "pcppmpcpmmmpcpcpmmmpmm"
                         "m" (algo-note (assoc config :chan 4 :offset 65
                                               :vel (min 127 (int (* 12 (at-i [10 4]))))
                                               :note (at-i [0 4 5 7 13 15])))
                         "p" (algo-note (assoc config :chan 5
                                               :offset (+ 70)
                                               :dur (at-i [1 3 1/2])
                                               :vel (min 127 (int (* 12 (at-i [10 4]))))
                                               :note (seq-cycle :s3/p [0 2 5 2 -5 2
                                                                       (at-i [0 4 0 5 6 0])
                                                                       2 0 2 0 2 8
                                                                       (seq-cycle :s3/p.b [10 12 17 19])]))))

                #_(evseq "bcb" #_"pccbbpbcb"
                         "p" (algo-note (assoc config :chan 6
                                               :offset (+ 70)
                                               :dur (* 10 (at-i [1 3 1/2 8]))
                                               :vel (min 127 (int (* 8 (at-i [10 4]))))
                                               :note (seq-cycle :s3/q [0 7 14 3 27 6 21 3])))
                         "b" (algo-note (assoc config :chan 7
                                               :offset (+ 70 -7 -7)
                                               :dur (* 10 (at-i [1 3 1/2 8]))
                                               :vel (min 127 (int (* 8 (at-i [10 4]))))
                                               :note (seq-cycle :s3/q [0 7 3 4 9 10 -3])))))))

  (fade-track {:track (first scene-3b-tracks) :dur-ms 5000 :db :-inf
               :on-end (fn []
                         (toggle-track-arm false (range 12 22))
                         (gp/stop :s3/main))}))
