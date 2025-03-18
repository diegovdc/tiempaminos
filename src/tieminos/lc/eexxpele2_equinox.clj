(ns tieminos.lc.eexxpele2-equinox
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.string :as str]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [algo-note]]
   [tieminos.osc.surge :as surge]
   [tieminos.utils :refer [wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp]
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

(comment
  ;; init
  (midi/midi-sinks)
  (def sink (midi/midi-out "VirMIDI"))
  (def sink2 (midi/midi-out "VirMIDI Bus 3"))
  (def sink3 (midi/midi-out "VirMIDI Bus 4"))
  (surge/init)
  (gp/stop))

(comment
  ;; Scene 1
  (gp/ref-rain
   :id :bd
   :tempo 40
   :durs [1/8]
   :on-event (gp/on-event
              (when (#{0 2 6 1} (mod i 8))
                  ;; bd
                (algo-note {:sink sink
                            :dur 1/1
                            :vel (min 127 (int (* 12 (at-i [3 4 5 3]))))
                            :chan 0
                            :offset 50
                            :tempo 120
                            :note (seq-cycle :bd [1 1 2 4 6 9])}))))
  (gp/ref-rain
   :id :glitch-pluck
   :tempo 90
   :durs [4/3 1 1/2]
   :on-event (gp/on-event
              (algo-note {:sink sink
                          :dur (at-i [1/7 3/2 4])
                          :vel (min 127 (int (* 12 (rand-nth [3 8 10 4 5 3]))))
                          :chan 1
                          :offset (weighted {80 4
                                             70 2
                                             71 1
                                             75 3
                                             50 6})
                          :tempo 120
                          :note (weighted {(seq-cycle :gp [1]) 5
                                           (- (rand-int 10) 10) 2})})))
  (gp/ref-rain
   :id :glitch-pluck2-random-ascent
   :tempo 90
   :durs (flatten [(concat (repeat 5 1/10) [(inc (rand-int 5))])
                   (concat (repeat 9 1/8) [(inc (rand-int 5))])
                   (concat (repeat 20 1/9) [(inc (rand-int 5))])
                   (concat (repeat 10 1/11) [(inc (rand-int 5))])
                   (concat (repeat 6 1/17) [(inc (rand-int 5))])])
   :on-event (gp/on-event
              (algo-note {:sink sink
                          :dur (weighted {1/8 10 1/2 1})
                          :vel (min 127 (int (* 16 (rand-nth [3 8 10 4 5 3]))))
                          :chan 1
                          :offset (rand-nth [0 3 8 10 12 40])
                          :tempo 120
                          :note (seq-cycle :gp2 (range 50 (rrand 58 70)))})))
  (gp/stop :glitch-pluck2-random-ascent)

  (gp/stop))

(comment
  ;; Scene 2
  (gp/ref-rain
   :id :bd
   :tempo 90
   :durs [1]
   :on-event (gp/on-event
              (algo-note {:sink sink
                          :dur (at-i [1])
                          :vel (min 127 (int (* 12 (at-i [3 4 5 3]))))
                          :chan 0
                          :offset 60
                          :tempo 90
                          :note [0 12]})))
  (gp/ref-rain
   :id :bd-child
   :ref :bd
   :tempo 90
   :durs [1/2]
   :on-event (gp/on-event
              (when (#{0 3 5 7 8} (mod i 10))
                (algo-note {:sink sink
                            :dur (at-i [1])
                            :vel (min 127 (int (* 8 (seq-cycle :bd/vel [10 14 15 3]))))
                            :chan 2
                            :offset (seq-cycle :bd/offset [70 70 70 70 70
                                                           72 72 72])
                            :tempo 90
                            :note (seq-cycle :bd [1 2 3])}))))
  (gp/ref-rain
   :id :bass
   :ref :bd
   :tempo 90
   :durs [1 1/4 1 1 1/2 2 1]
   :on-event (gp/on-event
              (when (> (rand) 0.5)
                (algo-note {:sink sink
                            :dur (at-i [30 40])
                            :vel (min 127 (int (* 12 (at-i [3 4 5 3]))))
                            :chan 1
                            :offset 50
                            :tempo 90
                            :note (seq-cycle :bd (concat [0 2 0 4 7]
                                                         [0 2 10 4 7]
                                                         [0 2 1 4 7]
                                                         [0 2 0 4 10]))}))))
  (gp/ref-rain
   :id :pad
   :ref :bd
   :tempo 90
   :durs [1/8]
   :on-event (gp/on-event
              #_(when (#{0 2 4 6 8 10 12 13} (mod i 14))
                  (algo-note {:sink sink
                              :dur (weighted {1/16 8
                                              1/8 5
                                                ;; 1 1/2
                                                ;; 3/2 4
                                                ;; 1/3 2
                                                ;; 1/2 3
                                              })
                              :vel (min 127 (int (* 7 (seq-cycle :pad/vel #_[20 4 11 5 3 8]
                                                                 [6 7 8 9 10 11 12]))))
                              :chan 3
                              :offset (weighted {70 8
                                                   ;; 65 1
                                                   ;; 50 4
                                                   ;; 30 1
                                                 })
                              :tempo 90
                              :note (seq-cycle :pad (weighted {[0 1 2 3 4 5 6 7 8 9 10] 6
                                                               #_#_[0 2 4 5 6 7 9] 3}))}))))

  (gp/stop))

#_(comment
  ;;  NOT too good
  ;; Scene 3
    (gp/ref-rain
     :id :bd
     :tempo 120
     :durs [1/3]
     :on-event (gp/on-event
                (if (#{0 1 2 3 4 6 7 8 9 11} (mod i 12))
                  (algo-note {:sink sink
                              :dur (at-i [1 1/3 1/3])
                              :vel (min 127 (int (* 12 (at-i [10 4 4]))))
                              :chan 0
                              :offset (+ 5 (at-i [50]))
                              :tempo 120
                              :note (at-i [0 0 0 0 0 1 2 0 0 0 4 5])})

                  (algo-note {:sink sink
                              :dur (at-i [1 1/3 1/3 2 2])
                              :vel (min 127 (int (* 12 (at-i [10 4 4]))))
                              :chan 0
                              :offset (+ 10 (at-i [50 51 50 50]))
                              :tempo 120
                              :note (at-i [1 1 2 1 1])}))))

    (gp/ref-rain
     :id :hh
     :ref :bd
     :tempo 120
     :durs [1/6]
     :on-event (gp/on-event
                (when (> (rand) 0.05)
                  (algo-note {:sink sink
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
     :tempo 120
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
(comment

  ;; Scene 3b
  (gp/ref-rain
   :id :bd
   :tempo 120
   :durs [1/4]
   :on-event (gp/on-event
              (let [config {:sink sink
                            :dur (at-i [1 1/3 1/3])
                            :vel (min 127 (int (* 12 (at-i [10 4 10]))))
                            :chan 0
                            :offset (at-i [50 52])
                            :tempo 120
                            :note 0}]
                #_(case (at-i (str/split "baab" #""))
                    "a" (algo-note (assoc config :chan 0 :note 0))
                    "b" (algo-note (assoc config :chan 1 :note 0))
                    "e" (algo-note (assoc config :chan 3 :note 0))
                    "c" nil)
                #_(case (at-i (str/split "ededddeddedeeeeeeeedddd" #""))
                    "d" (algo-note (assoc config :chan 2 :note 0))
                    "e" (algo-note (assoc config :chan 3 :note 0))
                    "c" nil)
                #_(case (at-i (str/split "mnmmnc" #""))
                    "m" (algo-note (assoc config :chan 4 :offset 60
                                          :vel (min 127 (int (* 12 (at-i [10 4 10]))))
                                          :note (at-i [0 4 5 7 -10 11 17])))
                    "n" (algo-note (assoc config :chan 4 :offset 70
                                          :dur (at-i [1 1/3 1/2])
                                          :vel (min 127 (int (* 12 (at-i [10 4]))))
                                          :note (seq-cycle :s3/n [10 16 15 10 5])))
                    "c" nil)

                #_(case (at-i (str/split "pcppmpcpmmmpcpcpmmmpmm" #""))
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
                                                                  (seq-cycle :s3/p.b [10 12 17 19])])))
                    "c" nil)
                #_(case (at-i (str/split "bcb" #_"pccbbpbcb" #""))
                    "p" (algo-note (assoc config :chan 6
                                          :offset (+ 70)
                                          :dur (* 10 (at-i [1 3 1/2 8]))
                                          :vel (min 127 (int (* 8 (at-i [10 4]))))
                                          :note (seq-cycle :s3/q [0 7 14 3 27 6 21 3])))
                    "b" (algo-note (assoc config :chan 7
                                          :offset (+ 70 -7 -7)
                                          :dur (* 10 (at-i [1 3 1/2 8]))
                                          :vel (min 127 (int (* 8 (at-i [10 4]))))
                                          :note (seq-cycle :s3/q [0 7 3 4 9 10 -3])))
                    "c" nil))))

  (gp/ref-rain
   :id :hh
   :ref :bd
   :tempo 120
   :durs [1/6]
   :on-event (gp/on-event
              (when (> (rand) 0.05)
                (algo-note {:sink sink
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
   :tempo 120
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
