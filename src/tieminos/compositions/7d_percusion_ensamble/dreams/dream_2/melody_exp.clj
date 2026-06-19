(ns tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.melody-exp
  (:require
   [clojure.set :as set]
   [erv.cps.utils :refer [+degree]]
   [overtone.core :as o]
   [tieminos.compositions.7d-percusion-ensamble.base
    :as *7d-base
    :refer [diat->polydori-degree root]]
   [tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.nil-space
    :as space]
   [tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.utils
    :refer [subrain]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.polydori.analysis.dorian-hexanies
    :refer [dorian-hexanies-in-polydori-2]]
   [tieminos.polydori.scale :refer [polydori-v2]]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.seq-utils.core
    :refer [** ++ choose graph lin mancha mirror mseq rainseq ret rev xo]]
   [tieminos.synths :as s]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]
   [time-time.standard :refer [rrand]]))

(comment
  ;; cps graph traversal
  (-> polydori-v2
      :subcps
      (get "4)5 of 4)7 3.7.9.15.19")
      :graphs
      :full
      (->> (map (fn [[k v]]
                  [(:bounded-ratio k)
                   (set (map :bounded-ratio v))]))
           (into {}))
      ;; keys
      )

  (-> polydori-v2
      :graphs
      :full)
  ;; WIP
  (do
    ;; NOTE: not used
    (defn make-polydori-deg-graph
      [scale-number]
      (let [hex (-> dorian-hexanies-in-polydori-2
                    (nth scale-number)

                    :hexany
                    (->> (map-indexed (fn [i data]
                                        (nth (:scale polydori-v2) (diat->polydori-degree scale-number i :original))))))]

        (->> (for [d1 hex
                   d2 hex]
               (when (some true? (for [s1 (:sets d1)
                                       s2 (:sets d2)]
                                   (= 3 (count (set/intersection s1 s2)))))
                 [d1 d2]))
             (remove nil?)
             (reduce (fn [graph [n1 n2]]
                       (-> graph
                           (update (:degree n1) (fnil conj #{}) (:degree n2))
                           (update (:degree n2) (fnil conj #{}) (:degree n1))))
                     {}))
        #_hex))

    (make-polydori-deg-graph 12))
  (do
    ;; NOTE: the good one
    (defn make-diat-polydori-deg-graph
      [scale-number]
      (let [hex (-> dorian-hexanies-in-polydori-2
                    (nth scale-number)
                    :hexany
                    +degree)]

        (->> (for [d1 hex
                   d2 hex]
               (when (= 1 (count (set/intersection (:set d1) (:set d2))))
                 [d1 d2]))
             (remove nil?)
             (reduce (fn [graph [n1 n2]]
                       (-> graph
                           (update (:degree n1) (fnil conj #{}) (:degree n2))
                           (update (:degree n2) (fnil conj #{}) (:degree n1))))
                     {}))
        #_hex))

    (def diat4v1 (make-diat-polydori-deg-graph 12))
    diat4v1)

  (mapv
   #(mseq % [1 2 3 (graph diat4v1)])
   (range 10))

  (oe/defsynth perky
    [freq 200
     amp 0.5
     atk 0.01
     dcy 1
     ladder-ratio 1
     pan 0
     out 0]
    (let [attack (-> (o/white-noise)
                     (o/moog-ff (o/rand 3000))
                     (* amp (o/env-gen (o/env-perc 0 (min 0.2 (+ atk dcy))))))
          body (+ (-> (o/saw freq)
                      (o/moog-ladder (*  freq) 0.3)
                      (* amp 8
                         (lfo-kr 3 0.8 1)
                         (o/env-gen (o/env-perc 0.01 (+ atk dcy -0.01)))))
                  (-> (+ (o/sin-osc freq)
                         (* (o/sin-osc-fb (/ freq 2)
                                          (* 2 (o/env-gen (o/env-perc 0 (min (* 2/3 dcy) (+ atk dcy))))))
                            (o/env-gen (o/env-perc 0 (min 0.7 (+ atk dcy))))))
                      (o/moog-ladder (* ladder-ratio  freq) 0.3)
                      (* amp 8
                         (lfo-kr 3 0.8 1)
                         (o/env-gen (o/env-perc atk dcy)
                                    :action o/FREE))))]
      (o/out out (-> (+ attack body)
                     #_(o/pan2 pan)
                     (* 0.5 (o/amp-comp-a freq))))))

  (oe/defsynth perky2
    [freq 200
     amp 0.5
     atk 0.01
     dcy 1
     ladder-ratio 1
     ladder-reso 0.8
     pan 0
     out 0]
    (let [attack (-> (o/sin-osc (o/line:kr freq (/ freq 4) 0.01))
                     (o/moog-ladder (o/rand 3000))
                     (* amp (o/env-gen (o/env-perc 0 (min 0.2 (+ atk dcy))))))
          body (-> (o/formant (+ (* 0.1 (o/sin-osc 2)) freq)
                              (+ (* 1/2 freq (o/sin-osc (o/line 2 0 (+ atk dcy)) (o/rand 0 3.14)))
                                 (* freq 2)))
                   (o/moog-ladder (* 2 ladder-ratio freq) ladder-reso)
                   (* amp 1.5
                      (lfo-kr (o/line:kr (o/rand 6 10) 0 dcy) 0.8 1)
                      (o/env-gen (o/env-perc atk (* 2 dcy) 1.5)
                                 :action o/FREE)))
          sig (+ attack body)]
      (o/out out (-> sig
                     (o/compander sig)
                     #_(o/pan2 pan)
                     (* 0.5 (o/amp-comp-a freq))))))

  (oe/defsynth low
    [freq 85
     amp 0.5
     mod-freq 8300
     pan 0
     atk 0.01
     dcy 1
     out 0]
    (o/out out (-> (o/range-lin (o/pulse mod-freq) (- freq 15) (+ freq 15))
                   o/sin-osc
                   #_(o/pan2 pan)
                   (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                   (* amp (o/amp-comp-a freq)))))

  (oe/defsynth sharp-plate
    [freq 350
     amp 1
     mod-freq 300
     lpf 2000
     pan 0
     atk 0.01
     dcy 0.5
     out 0]
    (o/out out (-> (o/range-lin (o/saw mod-freq) (- freq 350) (+ freq 350))
                   o/sin-osc
                   #_(o/pan2 pan)
                   (o/lpf lpf)
                   (* (o/env-gen (o/env-perc atk dcy) :action o/FREE))
                   (* amp (o/amp-comp-a freq)))))

  (rain.v2/stop)
  (let [scale-l [6 8 8 6 8]
        scale-r [8 8 6]
        s (fn [] (rand-nth [perky2 perky low s/sharp-plate]))]
    (rain.v2/ref-rain
     :id ::1
     :durs [3 2 2]
     :ratio 1/9
     :on-event (rain.v2/on-event
                (let [scale-l (mseq i scale-l)
                      scale-r (mseq i scale-r)
                      sq-r (++ [-6 -6 -6 0 0] [(++ (lin 0 0 -6 0 -12) 0) 3 [12 11] 15
                                               (graph (make-diat-polydori-deg-graph scale-r))])
                      sq-l (++  [-6 -6 -6 0 0 0] [0 [11 12] 15
                                                  (++ (lin 0 6 12 0 -12) (graph (make-diat-polydori-deg-graph scale-l)))])]
                  (when (xo "xx" i)
                    ((s) (:freq (*7d-base/deg->data
                                 :base-freq (* 2 root)
                                 :scale scale-l
                                 :degree (* (mseq i sq-r))))
                         :amp (rrange 0.5 0.9)
                         :pan (rrange 0 1)
                         :dcy (rrange 0.5 1.5)
                         :atk (rrange 0.01 0.03)
                         :mod-freq (rrand 3000 5000)
                         :out (mseq i (mancha space/main-graph))))
                  (when (xo "xxxxx" i)
                    ((s) (:freq (*7d-base/deg->data
                                 :base-freq (* root)
                                 :scale scale-r
                                 :degree (* (mseq i sq-l))))
                         :amp (rrange 0.5 0.9)
                         :pan (rrange -1 0)
                         :dcy (rrange 0.5 2)
                         :atk (rrange 0.01 0.03)
                         :mod-freq (rrand 3000 5000)
                         :out (mseq i (mancha space/main-graph-2d)))))
                #_(when (xo "xxo" i)
                    ((s) (:freq (*7d-base/deg->data
                                 :base-freq (* root)
                                 :scale 3
                                 :degree (* (mseq (inc i) sq)
                                            (mseq i [1 1/2 2 1/2 2])
                                            1/2)))
                         :amp (rrange 0.5 1.5)
                         :pan -1
                         :dcy (rrange 1.5 6)
                         :atk (rrange 0.01 0.1)
                         :mod-freq (rrand 1600 10000))))))

  #{1 15 3 9}
  (-> polydori-v2
      :scale)

  (let [g (graph
           (-> polydori-v2
               :subcps
               (get "4)5 of 4)7 3.7.9.15.19")
               :graphs
               :full
               (->> (map (fn [[k v]]
                           [(:bounded-ratio k)
                            (set (map :bounded-ratio v))]))
                    (into {}))))]
    (mapv
     #(mseq % g)
     (range 10))))

(comment
  ;; jam 1
  (defn s [] (rand-nth [perky2 perky low sharp-plate]))
  (rain.v2/stop)

  (let [scale 12]
    (rain.v2/ref-rain
     :id :jam
     :durs [1]
     :ratio 1/9
     :on-event (rain.v2/on-event

                #_((s) {:freq (:freq (*7d-base/deg->data
                                      :base-freq (* 1/2 root
                                                    (rainseq (concat (repeat 10 1)
                                                                     (mirror [1/2  4 {8 1  2 4}])
                                                                     (mirror [1/2  4 {8 1  2 4}])
                                                                     (mirror [1/2  4 {8 1  2 4}])
                                                                     (mirror [1/2  4 {8 1  2 4}])
                                                                     (mirror [1/2  4 {8 1  2 4}])
                                                                     (mirror [1/2  4 {8 1  2 4}]))))
                                      :scale scale
                                      :degree (rainseq (concat (repeat 20 [0 4])
                                                               (repeat 18 [0 7])
                                                               (repeat 18 [0 4 7])
                                                               (repeat 5 [10 4 7])))))
                        :amp (rainseq (concat (repeatedly 10 #(rrange 0.5 0.9))
                                              [1]
                                              (repeatedly 8 #(rrange 0.5 0.9))
                                              [1]
                                              (repeatedly 6 #(rrange 0.5 0.7))
                                              [1]
                                              (repeatedly 4 #(rrange 0.5 0.3))
                                              [1]
                                              (repeatedly 2 #(rrange 0.5 0.1))
                                              [1 1 1]))
                        :dcy (rrange 0.5 2)
                        :atk (rrange 0.01 0.03)
                        :mod-freq (rrand 3000 5000)
                        :out (mseq i (mancha :mancha-1 space/main-graph-2d))})

                (when (> 0.9 (rand))
                  ((rainseq [perky
                             (lin perky2 low)
                             perky
                             perky
                             perky2])
                   {:freq (:freq (*7d-base/deg->data
                                  :base-freq (* root
                                                (rainseq (concat (repeat 10 1/4)
                                                                 (repeat 20 1/2)
                                                                 (repeat 3 (lin 2 3 4 5 6 5 4 3 2)))))
                                  :scale (rainseq (concat (repeat 6 12)
                                                          (repeat 3 11)
                                                          (repeat 3 10)))
                                  :degree (rainseq (++ [0 4 0 1 0 2]
                                                       (concat (repeat 10 2)
                                                               (repeat 10 5)
                                                               (conj (repeat 10 7)
                                                                     (repeat 10 4))
                                                               (conj (repeat 10 17)
                                                                     (repeat 10 -4)))))))
                    :amp (rainseq {(rrange 1 1.2) 1
                                   (rrange 0.5 0.9) 13})
                    :dcy (rrange 0.2 1.8)
                    :atk (rainseq {(rrange 0.01 0.03) 16
                                   (rrange 0.1 0.3) 1})
                    :mod-freq (rrand 300 5000)
                    :out (user/spy (mseq i (graph :mancha-3 space/main-graph)))}))))))
(comment
  ;; jam 2
  (defn s [] (rand-nth [perky2 perky low sharp-plate]))

  (rain.v2/stop)

  (let [scale 12]
    (rain.v2/ref-rain
     :id :jam
     :durs (let [durs (mapv (fn [x] (/ x 5))
                            [6 5 4 5 5 6 4 5 6 5 4])]
             (fn [{:keys [index ratio]}]
               (println index)
               (user/spy "dur" (* ratio (wrap-at 0 durs)))))
     :ratio 1/9
     :on-event (rain.v2/on-event
                (when (xo "xooxoxooxoo" i)
                  (perky2 {:freq (:freq (*7d-base/deg->data
                                         :base-freq (* root
                                                       (rainseq (concat (repeat 1 2)
                                                                        #_(repeat 1 1)
                                                                        (repeat 2 2)
                                                                        (repeat 1 (lin 3 4))
                                                                        #_(mapv #(/ % 4) [16 15 14 13 12 11 10 9 8])
                                                                        #_[8 7 6 5 4 3 2 1])))
                                         :scale (rainseq (concat (repeat 40 14)
                                                                 (repeat 40 13)
                                                                 (repeat 40 12)
                                                                 #_(repeat 40 11)
                                                                 (repeat 40 10)))
                                         :degree (rainseq (++ [6 0 1 0 2 -6 3 -4 5]
                                                              [#_0 1 #_2 (choose 3 9) 0 #_(lin 1 4) #_2 #_5
                                                               (lin 15 14 13 14 12 13 11)
                                                               (lin 1 4)
                                                               (lin 15 14 13 14 12 13 11)
                                                               (lin 15 14 13 14 12 13 11)
                                                               (lin 15 14 13 14 12 13 11)]))))
                           :ladder-ratio (rainseq (range 0.4 0.1 -0.1))
                           :amp (rainseq {(rrange 0.5 3) 13})
                           :dcy (rainseq {(rrange 0.1 0.01) 10
                                          (rrange 2 4) 1/2})
                           :atk (rainseq {(rrange 0.01 0.03) 1
                                          (rrange 1 6) 15})
                           :out (rainseq (concat  (repeat 100 (mancha  space/left-wall))
                                                  (repeat 100 (mancha  space/right-wall))))}))

                (let [root-amp (rainseq (concat (repeat 16 1/2)
                                                (repeat 2 1/2)
                                                (repeat 1 1/4)
                                                (repeat 1 (lin 1 1 1 2))))]
                  ((rainseq [perky perky perky perky (lin low perky2)])
                   {:freq (:freq (*7d-base/deg->data
                                  :base-freq (* root root-amp)
                                  :scale (rainseq (concat (repeat 40 14)
                                                          (repeat 40 13)
                                                          (repeat 40 12)
                                                          #_(repeat 40 11)
                                                          (repeat 40 10)))
                                  :degree (rainseq (concat (repeat 20 [0 1])
                                                           (repeat 2 [0 5 (lin 8 14)])))))
                    :amp (if (= 1 root-amp) 1 (rainseq (++ 0.5 (** 0.1 [1 2 3 4]))))
                    :dcy (if (= 1 root-amp) 4 (rrange 0.2 1.8))
                    :ladder-ratio (rainseq (shuffle (range 1.2 0.2 -0.01)))
                    :atk (if (= 1 root-amp) 1 (rainseq {(rrange 0.01 0.03) 16
                                                        (rrange 0.1 0.3) 1}))
                    :mod-freq (rrand 300 5000)
                    :out (rainseq (concat  (repeat 100 (mancha  space/right-wall))
                                           (repeat 100 (mancha  space/left-wall))))}))))))

(comment
  (def i 0)
  ;; subrain test
  (rain.v2/stop)
  (let [play? (atom true)]
    (rain.v2/ref-rain
     :id :subrain-test
     :durs [2]
     :ratio 1/2
     :on-event (rain.v2/on-event
                (let [scale (rainseq (concat (repeat 60 18)
                                             (repeat 40 19)))
                      freq (:freq (*7d-base/deg->data
                                   :base-freq (* root 1/2)
                                   :scale scale
                                   :degree (rainseq [{0 10 13 1} {8 10 12 1} [10 11 16 17]])))]
                  #_(if (xo "xxoxxxo" i)
                      (sharp-plate {:freq (* freq 1/4) :amp (rand)
                                    :out (rainseq (graph space/cube-front))})
                      (low {:freq (* freq 1/4) :amp (rand)
                            :out (rainseq (graph space/cube-front))}))
                  (reset! play? (xo "ox" #_"ooooxxxooxooxxx" i))
                  (subrain
                   {:ref :subrain-test
                    :durs (rainseq (lin (apply ret (repeat 20 7/15))
                                        (apply ret (repeat 10 3/5))
                                        #_(apply ret (repeat 7 4/25))
                                        #_(apply ret (repeat 10 1/10))
                                        #_(apply ret (repeat 5 3/5))
                                        #_(apply ret (repeat 5 4/5))
                                        #_(apply ret (repeat 5 3/5))
                                        #_(apply ret (repeat 5 4/5))))
                    :ratio 1
                    :on-event (rain.v2/on-event

                               (when (and (= dur 3/5) @play?)
                                 ((rand-nth [low perky perky2 sharp-plate])
                                  (let [t ((if (= 3/5 dur)
                                             (partial min 1)
                                             (partial max 1/2))
                                           (rainseq (lin 1 1 1 1/4 1 2 2 2 1/4 1 1/4 1/8 1 2 2 2 1)))]
                                    {:freq (* freq
                                              (if (= dur 3/5) 1 1/2)
                                              (rainseq (lin 1/2 2 3)) t)
                                     :mod-freq (rrange 500 3000)
                                     :dcy (rainseq {(rrange 0.5 0.2) 1
                                                    (rrange 1 2) 1})
                                     :amp 0.3 #_(if (< t 1) 1 0.5)
                                     :out (if (= dur 3/5)
                                            (rainseq (graph space/cube-back))
                                            (rainseq (graph space/cube-front)))})))
                               (when-not (and (= dur 3/5) @play?)
                                 ((rand-nth [low perky perky2 sharp-plate])
                                  (let [t (rainseq (lin 1 1 1 1/4 1 2 2 2 1/4 1 1/4 1/8 1 2 2 2 1))]
                                    {:freq (* freq
                                              (if (= dur 3/5) 1 1/2)
                                              (rainseq (lin 1/2 2 3)) t)
                                     :mod-freq (rrange 500 3000)
                                     :dcy (rainseq {(rrange 0.5 0.2) 1
                                                    (rrange 1 2) 1})
                                     :amp 0.3 #_(if (< t 1) 1 0.5)
                                     :out (if (= dur 3/5)
                                            (rainseq (graph space/cube-back))
                                            (rainseq (graph space/cube-front)))}))))})

                  #_(when (> (rand) 0.5)
                      (subrain
                       {:ref :subrain-test
                        :durs (repeat (rrand 4 19) 1/16)
                        :delay 4/5
                        :on-event (rain.v2/on-event
                                   (perky2
                                    {:freq (:freq (*7d-base/deg->data
                                                   :base-freq (* root 2)
                                                   :scale scale
                                                   :degree (rainseq (range 0 10))))
                                     :ladder-ratio (rainseq (apply lin (range 0.9 0.7 -0.1)))
                                          ;; :ladder-reso 2
                                     :amp (rainseq {(rrange 0.5 1.5) 13})
                                     :dcy (rainseq {(rrange 0.8 1) 10})
                                     :atk (rainseq {(rrange 0.01 0.05) 1})
                                     :out (rainseq (graph space/left-wall))}))}))
                  #_(subrain
                     {:ref :subrain-test
                      :durs (rainseq [(ret 1/2 1/2)
                                      (ret 1/2 1/4 1/2)
                                      (ret 1/4 1/4 1/4 1/4)
                                      (apply ret (repeat 5 1/8))])
                      :ratio (rainseq (choose  3/5 2/5 1/5))
                      :delay (rainseq {1/3 4 1/2 1})
                      :on-event (rain.v2/on-event
                                 ((rainseq (choose low perky))
                                  {:freq (:freq (*7d-base/deg->data
                                                 :base-freq (* root (rainseq (lin 4 1 1 1 1 2 1 1 2 1 2 2 1 1/2 1 4 4 4 4)))
                                                 :scale scale
                                                 :degree (rand-nth [(rainseq (lin 0 7 8 3 #_[8 9 10] #_[8 9 10]))
                                                                    (rainseq (lin 0 7 8 3 [8 9 10] [8 9 10 -6]))
                                                                    (rainseq (lin 5 6))])))
                                   :ladder-ratio (rainseq (range 0.4 0.1 -0.1))
                                   :ladder-reso (rainseq (range 0.5 1.5 0.1))
                                   :amp (rainseq {(rrange 0.5 0.9) 13})
                                   :dcy (rainseq {(rrange 0.4 1) 10})
                                   :atk (rainseq {(rrange 0.01 0.05) 1})
                                   :out (rainseq (graph space/right-wall))}))}))))))

(comment
  (o/stop)
  (rain.v2/stop)
  (rain.v2/ref-rain
   :id :triangles
   :durs [1 1/2 1]
   :ratio 1/9
   :on-event
   (rain.v2/on-event
    (let [scale (rainseq (concat
                          (repeat 44 0)))]

      (when (xo "xxx" i)
        (perky
         {:freq (:freq
                 (*7d-base/deg->data
                  :base-freq (* root 2 1/2)
                  :scale scale
                  :degree (rainseq (++ [0 -3 0 {2 10 3 1 1 1}]
                                       (concat (repeat 10 0)
                                               (repeat 1 2))))))
          :amp (rrange 0.1 0.4)
          :ladder-ratio 2
          :ladder-reso 1
          :out (+  (rainseq [0 1 2 3 4]))}))))))

(comment
  (do
    (oe/defsynth bd
      [freq 80
       amp 0.5
       dur 1
       out 0]
      (o/out out
             (let [sig (o/sin-osc (o/line:kr 15000 freq 0.001))]
               (-> (+ sig (o/moog-ladder sig))
                   (* amp
                      (o/amp-comp-a freq)
                      (o/env-gen (o/env-perc 0 dur)
                                 :action o/FREE))))))

    (do
      (rain.v2/stop)
      (def scale (atom 13))
      (rain.v2/ref-rain
       :id :bd-thingy
       :durs [1]
       :ratio 1/2
       :on-event (rain.v2/on-event
                  (bd {:amp 1
                       :freq (:freq (*7d-base/deg->data
                                     :base-freq (* (/ root 2))
                                     :scale @scale
                                     :degree (rainseq [-2 -2 -2 -3 [-1 0]])))
                       :out (at-i [0 8 14 7])})))
      (rain.v2/ref-rain
       :id :bass
       :ref :bd-thingy
       :durs (concat [1 2 3/2 1/2]
                     [1 2 3/2 1/2]
                     [1 2 3/2 1/2]
                     (map #(* 1/2 %) [1 2 3/2 1/2])
                     #_(map #(* 1/2 %) [1 2 3/2 1/2])
                     (map #(* 1/4 %) [1 2])
                     [1 2 3/2 1/2]

                     #_(map #(* 1/4 %) [3/2 1/2])
                     (map #(* 1/4 %) [3/2 1/2])
                     #_(map #(* 1/4 %) (repeat 16 1/2))
                     #_(map #(* 1/4 %) (repeat 4 1/4)))
       :ratio 1/2
       :on-event (rain.v2/on-event
                  (when (> (rand) 0.7)
                    (bd {:amp 0.2
                         :dur (* 6 dur-s)
                         :freq (:freq (*7d-base/deg->data
                                       :base-freq (* (/ root 2))
                                       :scale @scale
                                       :degree (rainseq (++ [8 8 8 8 8 2 2 2 2 8 8]
                                                            (concat [0 3 2])))))
                         :out (rainseq (graph space/left-wall))}))))
      #_(rain.v2/stop :bd-thingy2)
      (rain.v2/ref-rain
       :id :bd-thingy2
       :ref :bd-thingy
       :durs [1]
       :ratio 1/4
       :on-event (rain.v2/on-event
                  (when (xo "oxoxoxoo" i)
                    (perky {:amp 0.1
                            :dur 0.7
                            :freq (:freq (*7d-base/deg->data
                                          :base-freq (* (/ root 2))
                                          :scale @scale
                                          :degree (mseq i (lin 11 [13 1]))))
                            :out 9}))
                  (when (xo "ooooxo" i)
                    (let [t (rainseq {1 8 2 1 3 1})]
                      (subrain {:ref :bd-thingy2
                                :durs (repeat 6 (rainseq [1/2 3/2 {1/4 2 1/3 5}]))
                                :delay (rand-nth [0 1/3])
                                :on-event
                                (rain.v2/on-event
                                 (when (> (rand) 0.5)
                                   (perky {:amp 0.2
                                           :dur (rainseq {0.7 5
                                                          1 5
                                                          1.5 3
                                                          2 5})
                                           :atk (rainseq {0.01 4 0.1 1 0.2 1})
                                           :freq (:freq (*7d-base/deg->data
                                                         :base-freq (* (/ root 2) (if (= dur 3/8) 2 1) t)
                                                         :scale @scale
                                                         :degree (mseq i (++ (concat (repeat 8 0)
                                                                                     (repeat 8 2)
                                                                                     (repeat 8 0)
                                                                                     (repeat 8 1))
                                                                             {(rev (lin 11 13)) 4
                                                                              (lin 11 13) 1}))))
                                           :out (rainseq (graph space/main-graph))})))})))))
      (rain.v2/ref-rain
       :id :t3
       :ref :bd-thingy
       :durs [1]
       :ratio 1/15
       :on-event (rain.v2/on-event
                  (when (xo "xooooo" i)
                    (low {:amp 0.1
                          :dur 5
                          :atk 0.01
                          :mod-freq 1800
                          :freq (:freq (*7d-base/deg->data
                                        :base-freq (* root (rainseq [1 1 1 1 1 2 2 2 2 2 1/2]))
                                        :scale @scale
                                        :degree (mseq i (lin 11 [16 18]))))
                          :out (rainseq (graph space/main-graph))}))))
      (rain.v2/ref-rain
       :id :scale
       :ref :bd-thingy
       :durs [1]
       :ratio 1
       :on-event (rain.v2/on-event
                  (reset! scale (rainseq (concat #_(repeat 12 16)
                                          #_(repeat 12 13)
                                          #_(repeat 12 11)
                                          #_(repeat 12 12)
                                          #_(repeat 12 8)
                                          #_(repeat 24 5)
                                          #_(repeat 24 4)
                                          (repeat 48 3)))))))))

(comment

  (rain.v2/ref-rain
   :id :space-atters
   :durs [1]
   :ratio 1/6
   :on-event (rain.v2/on-event
              (let [out (rainseq (mirror (++ -1
                                             (concat
                                              (repeat 6 space/tri-16d-seq)
                                              (repeat 6 space/tri-12d-seq)
                                              (repeat 6 space/tri-11d-seq)
                                              (repeat 6 space/tri-15d-seq)))))]
                (perky2 {:freq (* 100 (rainseq [1 3 2 [1 1 4]]))
                         :amp 0.5
                         :dcy (* 2 dur-s (rainseq [1 1 4 1]))
                         :out out})

                (when (xo "xooxoxoo" i)
                  (bd {:out (mod (+ 6 out) 24)}))
                (when (xo "ooxoxxox" i)
                  (perky2 {:freq (* 100 (* 2 (rainseq (lin 1 [3 13/8] 2 [1 1 4] [7/4]))))
                           :amp 0.5
                           :dcy (* 4 dur-s (rainseq [1 1 4 {1 8 8 1}]))
                           :out out})
                  (when (xo "ooxoxxox" i)
                    (subrain
                     {:ref :space-atters
                      :durs (repeat 8 1)
                      :ratio 1/2
                      :on-event (rain.v2/on-event
                                 (let [freq (* 400
                                               (rainseq {1 20 1/2 3 1/4 1})
                                               (* 2 (rainseq (lin 10/7 10/6 [26/10 13/10] 2 [1 20/13 13/2] [7/4 42/13]))))]
                                   (perky2 {:freq freq
                                            :amp (rainseq {0.1 10 0.2 4 0.5 1})
                                            :atk (rainseq {0 8 0.4 1})
                                            :ladder-ratio (rrange 0.5 8)
                                            :ladder-reso (rand 1.5)
                                            :dcy (*  dur-s (rainseq [1 1 4 {1 18 8 1}]))
                                            :out out})
                                   (when (xo "xoxoo" i)
                                     (low {:freq (* freq (rainseq (lin 1 1 1/2)))
                                           :amp (* 0.9 (rainseq (lin 1 1/4 1/2 1)))
                                           :mod-freq (rrange 300 800) :dcy 2}))))})))
                (when (rainseq (concat (repeat 15 false)
                                       (repeat 7 true)
                                       (repeat 15 [false true false])
                                       (repeat 7 true)
                                       (repeat 15 false)
                                       (repeat (+ 16 7) true)))
                  (perky {:freq (* (rainseq (concat
                                             (repeat 7 300)
                                             (repeat 15 200)
                                             (repeat 7 300)
                                             (repeat 15 200)
                                             (repeat (+ 16 7) 300)
                                             (repeat 15 200)))
                                   (* 2 (rainseq (lin 1 [3 13/8] 2 [1 1 4] [7/4]))))
                          :amp 0.8
                          :dcy (rainseq {0.1 4 0.3 1 2 1/2})
                          :out (rainseq (graph space/main-graph))})))))

  (rain.v2/stop :space-atters)
  (rain.v2/ref-rain
   :id :space-atters
   :durs [1]
   :ratio 1/6
   :on-event (rain.v2/on-event
              (let [out (rainseq (mirror (++ -1
                                             (concat
                                              (repeat 6 space/tri-16d-seq)
                                              (repeat 6 space/tri-12d-seq)
                                              (repeat 6 space/tri-11d-seq)
                                              (repeat 6 space/tri-15d-seq)))))]
                (perky2 {:freq (* 100 (rainseq [1 3 2 [1 1 4]]))
                         :amp 0.5
                         :dcy (* 2 dur-s (rainseq [1 1 4 1]))
                         :out out})

                (when (xo "xooxoxoo" i)
                  (bd {:out (mod (+ 6 out) 24)}))
                (when (xo "ooxoxxox" i)
                  (perky2 {:freq (* 100 (* 2 (rainseq (lin 1 [3 13/8] 2 [1 1 4] [7/4]))))
                           :amp 0.5
                           :dcy (* 4 dur-s (rainseq [1 1 4 {1 8 8 1}]))
                           :out out})
                  (when (xo "ooxooxox" i)
                    (subrain
                     {:ref :space-atters
                      :durs (repeat 2 1)
                      :ratio 1/2
                      :on-event (rain.v2/on-event
                                 (let [freq (* 200
                                               (rainseq {1 10 1/2 5 1/4 4})
                                               (* 2 (rainseq (lin 10/7  10/6 #_#_#_#_[26/10 13/10] 2 [1 20/13 13/2] [7/4 42/13]))))]
                                   (perky2 {:freq freq
                                            :amp (rainseq {0.1 10 0.2 4 0.5 1})
                                            :atk (rainseq {0 8 0.4 1})
                                            :ladder-ratio (rrange 0.5 8)
                                            :ladder-reso (rand 1.5)
                                            :dcy (*  dur-s (rainseq [1 1 4 {1 18 8 3}]))
                                            :out out})
                                   (when (xo "oxooo" i)
                                     (low {:freq (* freq (rainseq (lin 1 1 1/2)))
                                           :amp (* 0.9 (rainseq (lin 1 1/4 1/2 1)))
                                           :mod-freq (rrange 300 800) :dcy 4}))))})))
                (when (rainseq (concat (repeat 15 false)
                                       (repeat 7 true)
                                       (repeat 15 [false  false])
                                       #_(repeat 7 true)
                                       (repeat 15 false)
                                       #_(repeat (+ 16 7) true)))
                  (perky {:freq (* (rainseq (concat
                                             (repeat 7 300)
                                             (repeat 15 200)
                                             (repeat 7 300)
                                             (repeat 15 200)
                                             (repeat (+ 16 7) 300)
                                             (repeat 15 200)))
                                   (* 2 (rainseq (lin 1 [3 13/8] 2 [1 1 4] [7/4]))))
                          :amp 0.8
                          :dcy (rainseq {0.1 4 0.3 1 2 1/2 4 4})
                          :out (rainseq (graph space/main-graph))}))))))
