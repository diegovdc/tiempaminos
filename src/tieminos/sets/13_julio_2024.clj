(ns tieminos.sets.13-julio-2024
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [bh]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]
   [time-time.dynacan.players.gen-poly :as gp]
   [time-time.standard :refer [rrand]]))

(defn load-sample [filename] (o/load-sample (str "/Users/diego/Music/diego/rossana+videco/concierto_13_julio_2024/Media/" filename)))

(comment ;; setup
  (user/connect)
  (o/stop)
  (groups/init-groups!)

  (do
    (def liminal1 (load-sample "sample-1-un-sonido-que-exprese-un-estado-liminal-violin-y-voz.wav"))
    (def liminal2 (load-sample "sample-2-un-sonido-que-exprese-un-estado-liminal-violin-y-voz.wav"))
    (def liminal3 (load-sample "sample-3-un-sonido-que-exprese-un-estado-liminal-violin-y-voz.wav"))
    (def liminal4 (load-sample "sample-4-un-sonido-que-exprese-un-estado-liminal-violin-y-voz.wav"))
    (def sulpont (load-sample "12-Rossana-240703_2234.wav"))
    (def vln-quinta (load-sample "sample-quinta-oscilando-cuerdas-3-y-4.wav"))
    (def vln-intervalos (load-sample "sample-intervalos.wav"))
    (def vln-reso2 (load-sample "violin-reso-2.wav"))
    (def vln-armonicos (load-sample "sample-violin-armonicos-sobre-puente.wav"))
    (def vibra-7 (load-sample "sample-septimas-vibraphone.wav"))
    (def vibra-blues (load-sample "sample-blues-1-vibraphone.wav"))
    (def rio (load-sample "rill-under-the-bridge.wav"))
    (def orilla-rio (load-sample "agua-orilla-sutil.WAV")))

  (do
    (def bus1 (o/audio-bus 20 "bus1"))
    (def bus-rio (o/audio-bus 20 "bus1"))))

(comment ;; liminial
  (ndef/stop ::vibra)
  (ndef/ndef ::vibra
             (mapcat (fn [_] (let [buf (rand-nth [vibra-7 #_vln-armonicos #_vln-reso2 #_#_#_#_liminal1 liminal2 liminal3 liminal4])
                                   rate (rand-nth [1/2 6/7 7/9 14/9 12/7 2] #_[1 1/2 2/3 3/5 2])
                                   start-pos (rand-int (:n-samples buf))]
                               (-> (o/play-buf 2 buf rate
                                               :start-pos start-pos
                                               :loop true)
                                   (* (lfo-kr 0.2 0 0)))))
                     (range 4))
             {:out bus1
              :fade-time 10
              :group (groups/early)})

  (ndef/ndef ::liminal-b
             (mapcat (fn [_] (let [buf (rand-nth [liminal1 liminal2 liminal3 liminal4])
                                   rate (rand-nth [1 1/2 2/3 3/5 3 2])
                                   start-pos (rand-int (:n-samples buf))]
                               (-> (o/play-buf 2 buf rate
                                               :trigger (o/impulse 0.1)
                                               :start-pos start-pos
                                               :loop true)
                                   (* 0))))
                     (range 0))
             {:out bus1
              :fade-time 10
              :group (groups/early)}))

(comment ;; sobre el puente
  (ndef/stop ::sulpont)
  (ndef/ndef ::sulpont
      (mapcat (fn [_] (let [buf sulpont
                            rate (rand-nth [1 1/2 3/5 4/7 2])
                            start-pos (rand-int (:n-samples buf))]
                        (-> (o/play-buf 1 buf rate
                                        :trigger (rand-nth [1 (o/impulse 0.125)])
                                        :start-pos start-pos
                                        :loop true)
                            (o/pan2 (lfo-kr (rand 0.2) -1 1)))))
              (range 10))
      {:out bus1
       :fade-time 10
       :group (groups/early)})

  (ndef/stop ::sulpont2)
  (ndef/ndef ::sulpont2
      (mapcat (fn [_] (let [buf (rand-nth [liminal1 sulpont])
                            rate (rand-nth [5/4 9/2 22/12])
                            start-pos (rand-int (:n-samples buf))]
                        (-> (o/play-buf 1 buf rate
                                        :trigger (rand-nth [1
                                                            (o/impulse 4)
                                                            (o/impulse 7)
                                                            (o/impulse 3)])
                                        :start-pos start-pos
                                        :loop true)
                            (* 1)
                            (o/pan2 (lfo-kr (rand 0.2) -1 1)))))
              (range 8))
      {:out bus1
       :fade-time 10
       :group (groups/early)})

  (ndef/stop ::liminal-mid)
  (ndef/ndef ::liminal-mid
      (->> (o/in bus1 20)
           (map (fn [sig]
                  (-> sig
                      ((fn [sig] (+ (* 1 sig)
                                    (* 4 (o/moog-ladder sig (lfo-kr (rand 0.2) 1000 5000) 0.5))
                                    (* 2 (o/lpf sig 800)))))
                      (o/free-verb (lfo-kr 4 0.2 1) 4)
                      (* 8 1 (lfo-kr 0.2 2 4))
                      (o/pan2 (lfo-kr (rand 0.2) -1 1)))))
           (partition 2 2)
           (o/mix)
           (* 0))
      {:out (bh 0)
       :fade-time 30
       :group (groups/mid)})

  (ndef/stop ::sulpot ::sulpot2 ::liminal ::liminal-b ::liminal-mid))

(comment ;; río
  (o/stop)
  (ndef/stop ::rio)
  (ndef/ndef ::rio
      (->> 3
           range
           (mapcat (fn [i] (let [buf (rand-nth [rio orilla-rio])
                                 bpfreq (rand-nth [7 9 11 13 15 17 19])
                                 sig* (o/play-buf 2
                                                  buf
                                                  1
                                                  :start-pos (rand-int (:n-samples buf))
                                                  :loop true)]
                             (-> sig*
                                 (o/bpf (* 40 bpfreq)
                                        (lfo-kr (lfo-kr 0.32 0.1 1) 0.1 1))
                                 #_(o/hpf 400)
                                 (o/free-verb (lfo-kr (o/rand 0.2 2) 0.1 0.7) 2)
                                 #_((fn [sig] (+ (* 0.2 sig*)
                                                 (* 0.7 sig))))
                                 (o/pan2 (lfo-kr 2 -1 1))
                                 (o/mix)
                                 (* 0.1 4 8 2 (lfo-kr 0.1 0.1 1))))))
           (partition 2 2)
           (o/mix))
      {:out (bh 2)
       :fade-time 30
       :group (groups/early)})

  (o/demo (o/sin-osc))
  (oe/defsynth sperc
    [buf 0
     out 0
     rate 1
     start-pos 0
     amp 1
     bpf-freq 400
     bpf-rq1 0.5
     bpf-rq2 0.5
     a 0.01
     dur 0.2
     lpf 20000
     low-boost 0
     r 1]
    (o/out out
           (-> (*  (o/play-buf 2
                               buf
                               rate
                               :start-pos start-pos)
                   (o/env-gen (o/env-perc a r)))
               (o/bpf bpf-freq (o/line:kr bpf-rq1 bpf-rq2 dur))
               (o/lpf lpf)
               (o/free-verb 0.5 2)
               #_((fn [sig] (+ sig (* 32 low-boost (o/env-gen (o/env-perc (* dur 1/7) (* 6/7 dur)))
                                      (o/bpf sig 40 0.3)))))
               (* amp (o/env-gen (o/envelope [0 1 1 0]
                                             [0.001 (* 3 dur) 0.3])
                                 :action o/FREE)))))

  (gp/stop)

  (gp/stop ::rio-seq)
  (ref-rain
    :id ::rio-seq
    :durs [10]
    :tempo 160
    :ratio 1/8
    :on-event (on-event
                (let [buf (rand-nth [#_liminal4 rio])]
                  (sperc {:buf buf
                          :amp (if (= buf liminal4) 1 8)
                          :start-pos (rand-int (:n-samples buf))
                          :bpf-freq (* 400 (rand-nth [4 5 7 9 11]))
                          :bpf-rq1 0.1
                          :bpf-rq2 0.5
                          :a 2
                          :dur (* 2 dur-s)
                          :out (bh 2)}))))



  (gp/stop ::rio-punkte)
  (ref-rain
    :id ::rio-punkte
    :ref ::liminal-seq2
    :durs [2 3 2 3 3]
    :tempo 160
    :ratio 1/11
    :on-event (on-event
                (let [buf (rand-nth [rio])]
                  (sperc {:buf buf
                          :amp (* 0.3 4 (rrand 5 8)) ;; cresc
                          :rate (at-i [1 2 1/4 3/2])
                          :start-pos (weighted {(at-i [150000 20000 100000 400000 70000]) 1
                                                (rand-int (:n-samples buf)) 10})
                          :bpf-freq (max 50 (* 1600 (at-i [1/16 1 (at-i [12/7 3/2 3/2]) 1 (at-i [7/4 4/7 2/7]) 1])))
                          :bpf-rq1 0.01
                          :bpf-rq2 0.8
                          :a  0.01
                          :r (weighted {(at-i (concat (repeat 6 0.01)
                                                      [0.4])) 1
                                        1 0})
                          :lpf 4000
                          :dur 2
                          :out (bh 4)}))))

  (gp/stop ::liminal-seq2)
  (ref-rain
    :id ::liminal-seq2
    :durs [2 3 2 3 3]
    :tempo 160
    :ratio 6/11
    :on-event (on-event
                (let [buf (rand-nth [liminal3 liminal2 liminal1])]
                  (sperc {:buf buf
                          :amp (weighted {(* 1 2 7 (rrand 5 8)) 1}) ;; cresc
                          :rate (rand-nth [17/6 17/9 14/12 9/4 2 ])
                          :start-pos #_(at-i [150000 20000 400000 70000]) (rand-int (:n-samples buf))
                          :bpf-freq (* 400 (rand-nth [1/2 1 1/4 5/2 7/4 14/6 #_32/7]))
                          :bpf-rq1 0.1
                          :bpf-rq2 0.5
                          :a 0.01
                          :r (weighted {0.2 4
                                        3 1})
                          :lpf 700
                          :dur 10
                          :out (bh 2)}))))

  (o/stop)
  (ndef/stop ::scelsi)
  (ndef/ndef ::scelsi
      (o/mix (partition
               2 2
               (mapcat (fn [_] (let [buf (rand-nth [vln-reso2])
                                     rate (rand-nth [1/2 1 4/3 3/2 22/9 13/10 7/4 7/4 2 4])
                                     start-pos (rand-int (:n-samples buf))]
                                 (-> (o/play-buf 2 buf rate
                                                 #_#_:trigger (rand-nth [1 (o/impulse 16)])
                                                 :start-pos start-pos
                                                 :loop true)
                                     (* 4  16 (lfo-kr 0.2 0.7 1)))))
                       (range 10))))
      {:out (bh 4)
       :fade-time 10
       :group (groups/early)})


  (gp/stop ::vibra-seq)
  (ref-rain
    :id ::vibra-seq
    :durs [2 3 2 3 3]
    :tempo 160
    :ratio 1/11
    :on-event (on-event
                (let [buf (rand-nth [vibra-7 vibra-blues])]
                  (sperc {:buf buf
                          :amp (weighted {(* 4  8 (rrand 5 8)) 9
                                          0 40
                                          12 1})
                          :rate (weighted {1 3
                                           3/2 1
                                           2 1
                                           4/3 1/2
                                           (rand-nth [7/1 7/2]) 1/2})
                          :start-pos (at-i [(weighted {(rand-int (:n-samples buf)) 10
                                                       50000 1})
                                            (weighted {(rand-int (:n-samples buf)) 4
                                                       700000 1})
                                            (weighted {(rand-int (:n-samples buf)) 5
                                                       700000 1})
                                            ])
                          :bpf-freq (min 50 (* 400 (rand-nth [1/2 4 9/7 7/4 11 1/16])))
                          :bpf-rq1 (weighted {0.8 5
                                              0.01 1})
                          :bpf-rq2 (weighted {1 5
                                              0.01 1})
                          :a (weighted {0.1 20
                                        1/5 1})
                          :r (weighted {0.1 10
                                        1 1})
                          :lpf 4000
                          :dur 2
                          :out (bh 4)})))))
