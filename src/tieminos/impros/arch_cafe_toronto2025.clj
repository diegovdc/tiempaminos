(ns tieminos.impros.arch-cafe-toronto2025
  (:require
   [overtone.core :as o]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

(comment
  (def al-bireh (o/load-sample "~/Music/samples/freesound/patrickmcginleyalbirehdripping.wav"))
  (def stones (o/load-sample "~/Music/samples/freesound/stonesshabaabscreaming.wav"))
  (def ramallah1 (o/load-sample "~/Music/samples/freesound/20150521recording2.wav"))

  (def bus1 (o/audio-bus 2 "bus1"))
  (def trig-bus (o/control-bus 10 "trig-bus"))
  (groups/init-groups!)

  (ndef/ndef ::al-bireh
             (let [b ramallah1]
               (o/mix (mapv (fn [r]
                              (-> (o/play-buf 2 ramallah1
                                              :rate r
                                              :loop true
                                              :trigger (o/impulse 1/11)
                                              :start-pos (rand-int (:n-samples b)))
                                  (* 3)
                                  (o/hpf 480)
                                  #_(o/mix)))
                            [1])))
             {:out bus1 :group (groups/mid)})

  (ndef/ndef ::stones
             (let [start-pos (rand-int (:n-samples stones))]
               (println start-pos)
               (o/mix (mapv (fn [r]
                              (-> (o/play-buf 2 stones
                                              :rate 1
                                              :loop true
                                              :start-pos start-pos
                                              :trigger (o/impulse 110))
                                  (o/mix)
                                  (o/pan2 (lfo 2 -1 1))
                                  (o/moog-ladder (* r 300) (lfo 10 0.9 1.5))
                                  (* 1 (lfo (rand 2) 0.7 2))
                                  #_(o/mix)))
                            [1 1/2  2/3 4/11])))
             {:out bus1 :group (groups/mid)})

  (ndef/ndef ::palestine-main
             (o/mix (mapv (fn [_]
                            (-> (o/in bus1 2)
                                (o/free-verb 1 (lfo (rand) 0 0.7))
                                #_(o/bpf (lfo (rand) 300 3500) (lfo (rand 0.5) 0.2 1))
                                #_(o/pitch-shift :pitch-ratio (rand-nth [1 2/3 2/7]))
                                (o/pan2 (lfo (rand 16) -1 1))
                                (o/mix)
                                (* 0 4 8 2 2)))
                          (range 5)))
             {:out 0
              :fade-time 10
              :group (groups/post-fx)})

  (o/stop)

  (ndef/stop))
