(ns tieminos.compositions.birds-canon-v2
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :as scale]
   [erv.utils.ratios :as ratios]
   [overtone.core :as o]
   [taoensso.timbre :as log]
   [tieminos.math.bezier :as bz]
   [tieminos.math.utils :refer [linlin]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.v1 :as scu]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.std :refer [std!]]
   [time-time.player :as player]
   [time-time.standard :refer [rrand wrap-at]]))

(def durs (into [] (linlin 0.5 10 (bz/curve 80 [4 3 0.1 12 -1 -10 4 6]))))

(comment
  (bz/plot durs "test"))

(defonce index->sample-data-atom (atom {}))

(defn get-playback-data! [max-sample-pos index]
  (if-let [data (@index->sample-data-atom index)]
    data
    (let [pos (rand-int max-sample-pos)
          data {:start-pos pos
                :sin-freq (* 240 (weighted {8 2
                                            4 1}))
                :sin-amp (weighted {0 3
                                    #_#_(rrange 0.01 0.1) 4})
                :degs  (map (fn [_] (rrand -24 12))
                            (range (weighted {1 10
                                               ;; 2 1
                                               ;; 3 1
                                              ;; 5 2
                                              })))}]
      (swap! index->sample-data-atom assoc index data)
      data)))

(oe/defsynth play-sample
  [bufnum 0
   rate 1
   start-pos 0
   a 0.1
   s 1
   r 1
   amp 1
   panl-bus 0
   panr-bus 0
   rev-mix 0
   rev-room 0.8
   rev-damp 0.5
   sin-freq 100
   sin-amp 0.1
   out 0]
  (let [[left right] (o/play-buf 2 bufnum rate start-pos)]
    (o/out out (-> (o/mix [(-> left (o/pan2 (o/in:kr panl-bus)))
                           (-> right (o/pan2 (o/in:kr panr-bus)))
                           (-> (o/sin-osc sin-freq)
                               (* sin-amp #_(scu/lfo-kr 0.5 0.5 1)
                                  (o/env-gen (o/env-perc)))
                               (o/pan2 (o/in:kr panr-bus)))])
                   (o/free-verb rev-mix rev-room rev-damp)
                   (* amp (o/env-gen (o/envelope [0 1 1 0] [a s r])
                                     :action o/FREE))))))

(oe/defsynth lfo-ctl
  [freq 1
   out 0]
  (o/out:kr out (scu/lfo-kr freq -1 1)))

(comment
  (oe/defsynth sini
    [freq 200
     pan-bus 0]
    (o/out 0 (-> (o/sin-osc freq)
                 (o/pan2 (o/in:kr pan-bus))
                 (* 0.2))))

  (def pan1 (o/control-bus 1 "pan1"))

  (def lfo1 (lfo-ctl 10 pan1))
  (o/ctl lfo1 :freq 11)
  (sini 200 pan1)
  (o/stop))

(def scales (map ratios/ratios->scale [[1N 8/7 24/19 4/3 28/19 32/19]
                                       #_[1N 57/49 9/7 19/14 12/7 38/21]
                                       #_[1 32/27 4/3 32/21 14/9 16/9]
                                       #_[1N 21/19 4/3 28/19 32/19 7/4]
                                       #_[1N 7/6 4/3 3/2 14/9 7/4]]))

(defn on-event
  [max-sample-pos sample panners voice-offsets
   {data :data}]
  #_(log/info  data)
  (let [{:keys [voice index]} data
        playback-data (get-playback-data! max-sample-pos index)
        rev-mix (weighted {0 1
                           0.3 5
                           0.5 2
                           0.7 1
                           1 2})]
    #_(log/info :voice voice :index index :playback-data playback-data)
    (doseq [[i deg] (map-indexed vector (:degs playback-data))]
      (log/info :voice voice :index index :deg deg :start-pos (:start-pos playback-data))
      (let [rate (scale/deg->freq (wrap-at voice scales) 1 (+ (wrap-at voice voice-offsets) deg))]
        (play-sample {:out 20
                      :bufnum sample
                      :panl-bus (wrap-at voice panners)
                      :panr-bus (wrap-at (+ 1 voice) panners)
                      :start-pos (:start-pos playback-data)
                      :rate rate
                      :s (* 2 (:dur data))
                      :a (rrange 0.1 1)
                      :r (rrange 3 5)
                      :amp (* 1.7
                              (/ 1 (inc i))
                              (case rev-mix
                                0.3 0.9
                                0.5 0.7
                                0.7 0.5
                                1 0.5
                                1))
                      :sin-freq (* rate (:sin-freq playback-data))
                      :sin-amp (:sin-amp playback-data)
                      :rev-mix rev-mix
                      :rev-room (weighted {0.5 1
                                           0.8 2
                                           1 0.8})
                      :rev-damp (weighted {0.5 1
                                           0.8 1
                                           1 2})})))))

(comment
  (def birds-symphony
    "https://freesound.org/people/arnaud%20coutancier/sounds/467096/"
    (o/load-sample "samples/freesound/467096__arnaud-coutancier__birds-symphony.wav"))
  (def oropendolas-and-arredajos
    "https://freesound.org/people/arnaud%20coutancier/sounds/467096/"
    (o/load-sample "samples/freesound/413976__adrienpola__oropendolas-and-arrendajos-at-the-chagra-behind-nainekulodge-amazon.wav"))

  (def voice-offsets [0 1 -2 -4 -5 -7 -9 -10])

  (do
    (def panners (map (fn [i]
                        (let [panner (o/control-bus 1 (str "pan-lfo" i))]
                          (lfo-ctl (rrange 0.1 0.4) panner)
                          panner))
                      (range 5)))
    (reset! index->sample-data-atom {})
    (-> canon player/stop!)
    (def canon
      (let [sample         birds-symphony
            max-sample-pos (-> sample :n-samples (* 0.9) int)]
        (std! durs [1 9/8 3/2 10/4 7/4 11/4 14/11 #_#_#_17/11 17/10 17/8]
              (int (* 0.618 (count durs)))
              (fn [data]
                (on-event
                 max-sample-pos
                 sample
                 panners
                 voice-offsets
                 data))
              :loop? false)))))
