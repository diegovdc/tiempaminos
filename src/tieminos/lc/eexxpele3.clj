(ns tieminos.lc.eexxpele3
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.math :as math]
   [clojure.string :as str]
   [overtone.midi :as midi]
   [tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.utils :refer [subrain]]
   [tieminos.midi.plain-algo-note :refer [algo-note]]
   [tieminos.osc.reaper :as reaper]
   [tieminos.osc.surge :as surge]
   [tieminos.seq-utils.core :refer [** ++ choose lin rainseq xo]]
   [tieminos.seq-utils.parsers.instruments-seqs-parser :refer [evseq]]
   [tieminos.seq-utils.qwerty-velocity :as qwerty]
   [tieminos.utils :refer [cb-interpolate rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]
   [time-time.standard :refer [rrand]]))

(defn smap [f x]
  (if (sequential? x)
    (map f x)
    (f x)))

(defn s+ [n xs]
  (smap #(+ n %) xs))

(defonce at-atoms (atom {}))

(defn seq-cycle
  [id coll]
  (let [ats @at-atoms
        i* (inc (ats id -1))]
    (swap! at-atoms assoc id i*)
    (wrap-at i* coll)))

(def never-arm-envelope?
  (atom false))

(defn toggle-track-arm
  [arm? & tracks]
  (reaper/init)
  (doseq [track tracks]
    (reaper/set-track-rec track arm?)
    (if (and arm? (not @never-arm-envelope?))
      (reaper/set-autowrite track)
      (reaper/set-autotrim track))))

(defn fade-track
  [{:keys [track dur-ms db]}]
  (reaper/init)
  (cb-interpolate {:id (keyword "track" (str track))
                   :dur-ms dur-ms
                   :tick-ms 50
                   :init-val (reaper/from-db 0)
                   :target-val (reaper/from-db db)
                   :cb (fn [{:keys [val]}]
                         (reaper/set-vol track val))}))

(defn instpat* [str]
  (-> str
      (str/replace #" " "")
      (str/split #"")))

(def instpat (memoize instpat*))

(comment
  ;; init
  (do
    (midi/midi-sinks)
    (def sink (midi/midi-out "VirMIDI"))
    (def sink2 (midi/midi-out "VirMIDI Bus 2"))
    (def sink3 (midi/midi-out "VirMIDI Bus 3"))
    (surge/init))

  ;; development
  (reset! never-arm-envelope?
          true)

  (gp/stop))

(comment

  (reaper/rec)
  ;; Scene 1
  (fade-track {:track 1 :dur-ms 5000 :db 0})
  (toggle-track-arm true 1 2 3 4)
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
  (fade-track {:track 1 :dur-ms 60000 :db :-inf})
  (toggle-track-arm false 1 2 3 4)
  (gp/stop))

(comment
  ;; Scene 2
  (fade-track {:track 5 :dur-ms 1000 :db 0})
  (toggle-track-arm true 5 6 7 8 9 10 11)

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

  (fade-track {:track 5 :dur-ms 5000 :db :-inf})
  (rain.v2/stop :s2/bd)
  (rain.v2/stop :s2/bd-child)
  (rain.v2/stop :s2/bass)
  (rain.v2/stop :s2/cianningies)

  (toggle-track-arm false 5 6 7 8 9 10))

(comment
  ;; interludios
  (toggle-track-arm true 31)
  (toggle-track-arm false 31)
  (toggle-track-arm true 32)
  (toggle-track-arm false 32))

(comment
  (gp/stop)
  ;; s4 viejo vago brujo
  (toggle-track-arm true 22 23 24 25 26 27 28 29 30 31 32)
  (toggle-track-arm false 22 23 24 25 26 27 28 29 30 31 32)
  (fade-track {:track 20 :dur-ms 5000 :db 0})
  (fade-track {:track 20 :dur-ms 5000 :db :-inf}))

(comment
  #_(fade-track {:track 11 :dur-ms 500 :db :-inf})
  (fade-track {:track 12 :dur-ms 5000 :db 0})
  (apply toggle-track-arm true (range 12 22))
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

  (fade-track {:track 12 :dur-ms 5000 :db :-inf})
  (gp/stop :s3/main)
  (apply toggle-track-arm false (range 12 22)))

#_(comment
    ;;  NOT too good
    ;; Scene 3
    (gp/ref-rain
     :id :s3/bd
     :tempo 90
     :durs [1/3]
     :on-event (gp/on-event
                (if (#{0 1 2 3 4 6 7 8 9 11} (mod i 12))
                  (algo-note {:sink sink3
                              :dur (at-i [1 1/3 1/3])
                              :vel (min 127 (int (* 12 (at-i [10 4 4]))))
                              :chan 0
                              :offset (+ 5 (at-i [50]))
                              :tempo 120
                              :note (at-i [0 0 0 0 0 1 2 0 0 0 4 5])})

                  (algo-note {:sink sink3
                              :dur (at-i [1 1/3 1/3 2 2])
                              :vel (min 127 (int (* 12 (at-i [10 4 4]))))
                              :chan 0
                              :offset (+ 10 (at-i [50 51 50 50]))
                              :tempo 120
                              :note (at-i [1 1 2 1 1])}))))

    (gp/ref-rain
     :id :hh
     :ref :bd
     :durs [1/3]
     :on-event (gp/on-event
                (when (> (rand) 0.3)
                  (algo-note {:sink sink3
                              :dur (* 2 dur-s)
                              :vel (min 127 (int (*  1
                                                     (at-i [5 5 10 6 2 3])
                                                     (seq-cycle :hh/vel [1 2 3 4 5 6 7 8 9 10 11 12]))))
                              :chan 3
                              :offset (at-i [55])
                              :tempo 60
                              :note (at-i [0 [2 3] 4 5 4])}))))
    (gp/stop :leady)
    (gp/ref-rain
     :id :leady
     :ref :bd
     :durs [2/3]
     :on-event (gp/on-event
                (algo-note {:sink sink
                            :dur (* (at-i [1 1 1 2]) dur-s)
                            :vel (min 127 (int (* 10 (seq-cycle :pad/vel [10 10 5]))))
                            :chan 4                           :offset 60
                            :tempo 60
                            :note (at-i [0
                                         (seq-cycle :bass/n2 [0 1 2 0 0 -6])
                                         0
                                         (seq-cycle :bass/n4 [6 6 5 3 6])
                                         (seq-cycle :bass/n1 [0 0 0 2])
                                         (seq-cycle :bass/n2 [0 1 2 0 0 -6])
                                         0
                                         (seq-cycle :bass/n4b [6 12 12 5 3 6])])})))

    (gp/stop))
