(ns tieminos.sets.viejo-vago-brujo
  "Set para Viejo Vago Brujo

  - La versión en `01f6ae4570114375fd63c138a3d3c9f3e1339840` está bastante bien."
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.math :refer [pow]]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [algo-note]]
   [tieminos.seq-utils.core :refer [** ++ choose lin rainseq ret xo]]
   [tieminos.seq-utils.qwerty-velocity :as qwerty]
   [time-time.dynacan.players.gen-poly :as gp]
   [time-time.dynacan.players.refrain.v2 :as rain.v2 :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(def sample-base-dir "~/Music/samples/")

(defn delacreme-stacks
  [sample-name]
  (format  "%sDelacreme-One-Shots/stacks/%s.wav"
           sample-base-dir
           sample-name))

(defn delacreme-single
  [sample-name]
  (format  "%sDelacreme-One-Shots/singles/%s.wav"
           sample-base-dir
           sample-name))

(def bh-default-out 20)
(def bd-out (+ 2 bh-default-out))
(def congas-out (+ 4 bh-default-out))
(def rim-out (+ 6 bh-default-out))
(def hh-out (+ 8 bh-default-out))
(def sd-out (+ 10 bh-default-out))

(o/defsynth st-smpl
  [buf 0
   amp 1
   rate 1
   pan 0
   len 1
   out bh-default-out]
  (o/out out (-> (o/play-buf 2 buf rate)
                 (o/mix)
                 (o/pan2 pan)
                 (* amp (o/env-gen (o/envelope [0 1 1 0]
                                               [0.0001
                                                (* len
                                                   (- (o/buf-dur buf)
                                                      0.0002))
                                                0.0001])
                                   :action o/FREE)))))
(o/defsynth mono-smpl
  [buf 0
   rate 1
   amp 1
   pan 0
   len 1
   out bh-default-out]

  (o/out out (-> (o/play-buf 1 buf rate)
                 (o/pan2 pan)
                 (* amp (o/env-gen (o/envelope [0 1 1 0]
                                               [0.0001
                                                (* len (- (o/buf-dur buf)
                                                          0.0002))
                                                0.0001])
                                   :action o/FREE)))))

(defn delay*
  [{:keys [ref ratio delay-time durs]
    :or {delay-time 0
         durs [1]
         ratio 1}}
   event-fn]
  (let [durs (concat (if (> delay-time 0) [delay-time] [])
                     durs)
        total-durs (count durs)]
    (ref-rain (cond-> {:id (random-uuid)
                       :loop? false
                       :durs durs
                       :on-event (on-event
                                  (when-not (and (> delay-time 0) (zero? i))
                                    (let [last-dur? (= (inc index) total-durs)]
                                      (event-fn (assoc data :last-dur? last-dur?)))))}
                ref (assoc :ref ref)
                ratio (assoc :ratio ratio)))))
(comment
  (do
    (def rim (o/sample (delacreme-stacks "Rim_multirim")))
    (def bd (o/sample (delacreme-single "bd29_01_Synthdrum Pack")))
    (def bd2 (o/sample (delacreme-single "BD_Kick boom1_Kick Pack Deluxe")))
    (def hh (o/sample (delacreme-single "HH_Zildjian Avedis_V-08_s_HiHat Essentials")))
    (def hho (o/sample (delacreme-single "HH_Zildjian quick_V-01_s_HiHat Essentials")))
    (def congao (o/sample (delacreme-single "conga open_World Sounds Vol3")))
    (def conga-low (o/sample (delacreme-single "conga low open_World Sounds Vol3")))
    (def conga-high (o/sample (delacreme-single "conga high hit_World Sounds Vol3")))
    (def sd (o/sample (delacreme-single "SD_dnb_Raw Muffled Snares"))))

  (st-smpl rim :out 20)
  (st-smpl bd)
  (-> sd :n-channels))
(pow 3 5)
(comment
  (ref-rain
   :id :bd
   :tempo 180
   :ratio 1/4
   :cycle-len 36
   :durs #_[4 4 4 4 4 4 4 2 1 1 2 2]
   [66/16 4 4 62/16 4 62/16 4 2 1 1 2 34/16]
   :on-event (on-event
              (when-not (and (= dur 1/4) (rainseq (lin false true false true true true)))
                (let [bd* (weighted {bd 3 bd2 1})
                      player (if (= bd* bd) st-smpl mono-smpl)]
                  (delay*
                   {:durs (rainseq [(ret 1)
                                    (ret 1)
                                    (ret 1)
                                    #_{(ret 1) 10
                                       (ret 1/3 1/3) 2
                                       (ret 1/32 1/3) 4}])}
                   (fn [_]
                     (player bd*
                             :out bd-out
                             :rate (* #_(rrand 0.999 1.001)
                                    (rainseq (let [n (++ 1 [0 0.002 0.002 -0.001 -0.002 0])
                                                   p {n 20 #_#_#_#_2/3 1 1/2 1}]
                                               [p n p n p n p n n])))
                             :len (cond
                                    (>= dur 1/12) 0.2
                                    (>= dur 1/2) 0.5
                                    :else (rainseq [1 0.9 0.9 0.7 0.8]))
                             :amp (* (rrand 1 0.9)
                                     (if (or (zero? (mod i 12))
                                             (xo "ooxoxo" i))
                                       1
                                       (rainseq (apply lin (qwerty/amp "peye"))))))
                     #_(player bd*
                               :out bd-out
                               :rate 2
                               :len 0.1
                               :amp 1)))))))
  (ref-rain
   :id :rim
   :ref :bd
   :tempo 180
   :ratio 1/4
   :durs [6 98/16 62/16 2 4]
   :on-event (on-event
              (st-smpl rim :pan (rrand -1.0 1) :out rim-out)))

  (rain.v2/stop)
  (rain.v2/stop :hh)
  (ref-rain
   :id :hh
   :ref :bd
   :tempo 180
   :ratio 1/4
   :durs [2/3 2 2 2 2 2 2 2 2 4/3]
   :on-event (on-event
              (let [amp (rainseq (** (concat (repeat 10 1) [[1.2 1.5]])
                                     (qwerty/db -18 0 "z16l816z")))]
                (if (xo "ooxox" i)
                  (mono-smpl hh
                             :rate (rainseq (++ [0.001 0 -0.001] (lin 1 1 12/11 1 1)))
                             :out hh-out
                             :amp amp
                             :len (rainseq (lin 1 0.9 0.7 1 1 0.8 0.5 0.3))
                             :pan (rrand -1.0 0))
                  (mono-smpl hho
                             :out hh-out
                             :rate (rainseq (++ 1 [0.001 0 -0.001]))
                             :amp amp
                             :len (rainseq (lin 1 0.9 0.7 1 1 0.8 0.5 0.3))
                             :pan (rrand 0.5 1))))))

  (rain.v2/stop)
  (def conga-pan (memoize (fn [_synth _rate]
                            (rrand -0.5 0.5))))
  (ref-rain
   :id :congas
   :ref :bd
   :tempo 180
   :ratio 1/2
   :durs [2 2 2 2 2 2 2 2 1 2 1]
   :on-event (on-event
              (when (or #_true (#{0  3 4} (mod i 5)))
                (let [ratchet* (rainseq [1 1 1 2 {1 8
                                                  2 2
                                                  3 3
                                                  5 1/2}])
                      ratchet (if (= dur 1/2) (min 2 ratchet*) ratchet*)
                      conga (rand-nth [conga-low
                                       conga-high congao])
                      amp (+ (at-i [0.5 0 1 0 0])
                             (rainseq {1 1
                                         ;; 0 2
                                       0.8 2
                                       0.7 3}))
                      amp-curve (rainseq (choose 0.94 0.92 0.85 0.7 1.2 1.3))
                      start-amp (if (and (> amp-curve 1)
                                         (> ratchet 1)) 1 0.7)
                      rate (rainseq (** [2 2 2 {1 10 [2 1 1] 3}]
                                        [1 13/11 1 12/11 [12/11
                                                          {1 8
                                                           12/11 2
                                                           13/11 8
                                                           7/4 5
                                                           2 8
                                                           3 10
                                                           3/2 2}]]))
                      curve (rainseq [1.2 1.1 1.3 0.9 0.8 0.7])
                      durs (map-indexed #(* %2 (pow curve %1))
                                        (repeat ratchet
                                                (/ dur-s ratchet)))]
                  (delay*
                   {:ratio (rainseq {1 8
                                     2 2
                                     3 5
                                     5 2})
                    :durs durs
                    :delay-time (rainseq {0 2 1/6 2 2/3 6})}
                   (fn [{:keys [index]}]
                     (st-smpl conga
                              :pan (conga-pan conga rate)
                              :out congas-out
                              :len (rainseq (lin 1 0.9 0.9 1 1 0.8 0.5
                                                 0.3
                                                 0.4
                                                 0.5
                                                 1))
                              :amp (min 1 (* start-amp
                                             (rainseq (lin 1 1 1.1 1 0.9 1.2))
                                             (pow amp-curve index)
                                             amp))
                              :rate (+
                                     (weighted {0 5 (rrand -0.001 0.01) 3})
                                     rate))))))))

  (rain.v2/stop)
  (ref-rain
   :id :sd
   :ref :bd
   :tempo 180
   :ratio 1
   :durs [5/4 3/4]
   :on-event (on-event (when-not (= dur 5/4) (mono-smpl sd :out sd-out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (def sink (midi/midi-out "VirMIDI"))
  (def sink2 (midi/midi-out "VirMIDI Bus 3"))
  (def sink3 (midi/midi-out "VirMIDI Bus 4"))

  (rain.v2/stop)

;;;;;;;;;;;
  ;; Bass
;;;;;;;;;;;

  ;; A really good alternative to the bass, from glitchy-forest
  #_(gp/ref-rain :id :s1/bd :tempo 40 :durs [1/8] :on-event (gp/on-event (when (#{0 2 6 1} (mod i 8)) (algo-note {:sink sink :dur (rainseq {1 3 2 1 1/4 1 4 4}) :vel (min 127 (int (* 12 (at-i [3 4 5 3])))) :chan 0 :offset 50 :tempo 120 :note (rainseq (++ [0 0 10 {0 3 12 1} (choose 0 3 2) 0 -5 0 2] (lin :id/bd [1 {3 5 6 1} 2 4 6 {9 5 7 1}])))}))))
  (ref-rain
   :id :bass
   :ref :bd
   :tempo 180
   :ratio 1
   :durs [4 2 4 2 #_2]                  ; TODO: check if thi this #_2 could still work?
   :on-event (on-event
              (let [amp 14]
                (algo-note {:sink           sink
                            :dur     (* 2 dur-s)
                            :vel            (min 127 (int (* amp (rainseq [8 5 7 6 5]))))
                            :scale-size     7
                            :chan           0
                            :offset         60
                            :note           (rainseq
                                             (++ (concat (repeat 25 0)
                                                         #_(repeat 25 [[0 2 -1] 0 [0 -7]]))
                                                 [0 3 5 7 8 (lin 8 {7 7 15 3})]))})
                (when (zero? (mod i 6))
                  (delay*
                   {:ratio (* 1/2 (rainseq {1/4 5 1/3 2 2/3 2 1/6 1}))
                    :durs (let [total (rainseq {1 6
                                                2 4
                                                3 3
                                                4 2
                                                5 1})]
                            (repeat total (/ 1 (inc total))))}
                   (fn [_]
                     (algo-note {:sink           sink
                                 :dur (* 0.7 dur-s)
                                 :vel            (min 127 (int (* amp (rainseq [8 3 4 5 3]))))
                                 :scale-size     7
                                 :chan           0
                                 :offset         60
                                 :note           (rainseq (choose 19 18 17 21 20 22))})))))))
  (rain.v2/stop)
  (rain.v2/stop :melody2)
  ;;  Good alternative to the melody
  #_(gp/ref-rain :id :s1/glitch-pluck :tempo 90 :durs [4/3 1 1/2] :on-event (gp/on-event (when (> 0.5 (rand)) (algo-note {:sink sink :dur (at-i [1/7 3/2 4]) :vel (min 127 (int (* 12 (rand-nth [3 8 10 4 5 3])))) :chan 1 :offset (weighted {80 4 71 10 75 3 50 6}) :tempo 120 :note (weighted {(seq-cycle :s1/gp [1]) 5 (- (rand-int 20) 20) 2})}))))
  #_(gp/ref-rain :id :s1/glitch-pluck2-random-ascent :tempo 90 :durs (flatten [(concat (repeat 5 1/10) [(inc (rand-int 5))]) #_(concat (repeat 5 5/10) [(inc (rand-int 5))]) (concat (repeat 9 1/8) [(inc (rand-int 5))]) (concat (repeat 20 1/9) [(inc (rand-int 5))]) (concat (repeat 10 1/11) [(inc (rand-int 5))]) (concat (repeat 6 1/17) [(inc (rand-int 5))])]) :on-event (gp/on-event (algo-note {:sink sink :dur (weighted {1/8 18 1/2 1/5}) :vel (min 127 (int (* 16 (rand-nth [3 8 10 4 5 3])))) :chan 1 :offset (rand-nth [0 3 8 10 12 40]) :tempo 120 :note (rainseq (mapcat (fn [x] (map #(+ x %) (concat (range 50 (rrand 58 70)) (reverse (range 50 (rrand 58 70)))))) [0 10 -10 -4 8]))})))
  (ref-rain
   :id :melody2
   :ref :bd
   :tempo 180
   :ratio 1/8
   :durs [4 2 4 3 1 4 2 4 3 1 8]
   :on-event (on-event
              (when-not (= dur 1)
                (algo-note {:sink sink
                            :dur (weighted {1 3
                                            1/2 1
                                            1/10 1
                                            1/5 2})
                            :vel (min 127 (int (* 1 (at-i [8 3 4 5 3]))))
                            :chan 1
                            :offset (+ -10 (at-i [60 60 62 60 60 60 60 63 65]))
                            :note (+ (at-i [0 3 5 7 8 8 12 13 15]))}))))

  (rain.v2/stop :hh)
  (gp/stop)
  (rain.v2/stop :bd)
  (rain.v2/stop :pad)
  (rain.v2/stop :bass)
  (rain.v2/stop :melody2)
  (rain.v2/stop :rim)
  (rain.v2/stop :congas)
  (rain.v2/stop :sd))
