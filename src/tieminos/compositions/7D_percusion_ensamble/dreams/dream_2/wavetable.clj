(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.wavetable
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [overtone.core :as o]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [bh]]
   [tieminos.overtone-extensions :as oe]
   [time-time.dynacan.players.refrain.v2 :as rain2]
   [time-time.standard :refer [rrand]]
   [user]))

(do
  (defn get-sample-num-from-path
    [path]
    (-> path
        (str/split #"/")
        last
        (str/split #"\.")
        first
        (str/split #"_")
        last
        edn/read-string))
  (get-sample-num-from-path "/Users/diego/Music/code/tieminos/samples/AKWF-wavetable/AKWF_bw_sin/AKWF_sin_1.wav"))

(defn available-dirs
  []
  (let [path (format "%s/samples/AKWF-wavetable" user/tieminos-path)]
    (->> path
         io/file
         file-seq
         (filter #(.isDirectory %))
         (map #(.getPath %))
         (remove #(= % path))
         (map (fn [p]
                (-> p
                    (str/split #"/")
                    last
                    (str/split #"\.")
                    first
                    (str/replace #"AKWF_" "")
                    (str/replace #"AKWF_" "")))))))

(available-dirs)

(defn load-akwf-directory
  [dir-suffix]
  (->> (format "%s/samples/AKWF-wavetable/AKWF_%s/" user/tieminos-path dir-suffix)
       io/file
       file-seq
       (remove #(.isDirectory %))
       (map #(.getPath %))
       (sort-by get-sample-num-from-path)
       (mapv o/load-sample)))

(defn akwf-samples
  [& dir-suffixes]
  (->> dir-suffixes
       (mapv
        (fn [suffix]
          (println "Loading" suffix)
          (let [bufs (load-akwf-directory suffix)]
            [(-> suffix (str/replace #"_" "-") keyword)
             {:min-wave (first bufs)
              :max-wave (last bufs)}])))
       (into {})))
(comment
  (apply akwf-samples '("sinharm"
                        "blended"
                        "bw_sin"
                        "bw_sawbright"
                        "bw_sawrounded"
                        "general"
                        "violin"
                        "stereo"
                        "raw"
                        "c604"
                        "linear"
                        "symetric"
                        "bw_sq"
                        "eorgan"
                        "bw_sqrounded"
                        "hdrawn"
                        "epiano"
                        "oscchip"
                        "bw_tri"
                        "piano"
                        "vgamebasic"
                        "bw_saw"
                        "cello"
                        "stringbox"
                        "eguitar"
                        "theremin"
                        "pluckalgo"
                        "bw_sawgap"
                        "vgame"
                        "overtone"
                        "oboe"
                        "flute"
                        "distorted"
                        "ebass"
                        "aguitar"
                        "dbass"
                        "birds"
                        "hvoice"
                        "altosax"
                        "fmsynth"
                        "granular"
                        "bitreduced"
                        "snippets"
                        "clavinet"
                        "clarinett")))

(oe/defsynth mooglad
  [freq 220
   min-wave 0
   max-wave 1
   offset 0
   range 1
   a 0.01
   s 0.5
   s-amp 1
   d-amp 1
   r 0.49
   amp 0.5
   moog-freq 20000
   moog-res 0.5
   pan 0
   out 0]
  (let [env (o/env-gen (o/envelope [0 s-amp d-amp 0]
                                   [a s r]
                                   0.1)
                       :action o/FREE)
        dur (+ a s r)
        wave-range (- max-wave min-wave)
        min-wave* (+ min-wave (* offset wave-range))
        max-wave* (+ min-wave* (* range wave-range))
        sig (-> (o/v-osc (o/line:kr (o/clip min-wave* min-wave max-wave)
                                    (o/clip max-wave* min-wave max-wave)
                                    dur)
                         freq)
                (o/moog-ladder moog-freq moog-res))]
    (o/out out (-> sig
                   (* amp env)
                   (o/pan2 pan)))))
(oe/defsynth mono-mooglad
  [freq 220
   min-wave 0
   max-wave 1
   offset 0
   range 1
   a 0.01
   s 0.5
   s-amp 1
   d-amp 1
   r 0.49
   amp 0.5
   moog-freq 20000
   moog-res 0.5
   pan 0
   out 0]
  (let [env (o/env-gen (o/envelope [0 s-amp d-amp 0]
                                   [a s r]
                                   0.1)
                       :action o/FREE)
        dur (+ a s r)
        wave-range (- max-wave min-wave)
        min-wave* (+ min-wave (* offset wave-range))
        max-wave* (+ min-wave* (* range wave-range))
        sig (-> (o/v-osc (o/line:kr (o/clip min-wave* min-wave max-wave)
                                    (o/clip max-wave* min-wave max-wave)
                                    dur)
                         freq)
                (o/moog-ladder moog-freq moog-res))]
    (o/out out (-> sig
                   (* amp env)))))

(comment

  (rain2/ref-rain
   :id :wt1
   :durs [1/4 1/4 1/4 1/4 1/2 1/4]
   :tempo 77
   :on-event (rain2/on-event
              (when (xo "xoxoxoooxo" i)
                #_(mooglad (merge
                            (akwf (weighted {:dbass 0
                                             :birds 2}))
                            {:amp (+ (rand 0.))
                             :freq (* (weighted {800 2 1600 1})
                                      (at-i [1 17/14 3/2 2 3/2 17/14]))
                             :pan (rrand -1.0 1)
                             :offset (+ (rand 0.1) 0.3)
                             :range -0.0
                             :a 1
                             :s (weighted {0.3 5 0.7 1})
                             :moog-freq (* (rand-nth [1/2 1])
                                           (weighted {800 7}))
                             :moog-res (+ 10 (rand 1.9))
                             :s-amp (rand-nth [0.5 0.8 0.3])
                             :d-level (at-i [0.2 1])
                             :r 1
                             :out (bh 6)})))))
  (rain2/ref-rain
   :id :wt2
   :ref :wt1
   :durs [1/8]
   :on-event (rain2/on-event
              (if (xo "ooooooo" i)
                (mooglad (merge
                          (akwf (at-i [:ebass :bitreduced]))
                          {:amp (* 0.1 (at-i [1 1/2 2 1/2 1 2]))
                           :freq (* 800
                                    (weighted {1 8 3/2 1 2 1 1/2 1})
                                    (weighted {17/14 7 3/2 4 28/17 2}))
                           :offset (rand 0.8)
                           :range (rrand -0.4 0.2)
                           :a (weighted {0.0 3
                                         0.01 1})
                           :s (+ (rand 0.3) 0.1)
                           :moog-freq (rrand 1500 3000)
                           :moog-res (* (rand 0.5)
                                        (at-i [3 1 1 1]))
                           :d-level (weighted {0 3
                                               0.1 4
                                               0.3 1})
                           :r 0.5
                           :out (bh 4)}))
                (mooglad (merge
                          (akwf :bw-saw)
                          {:amp (* 0.  (at-i [1 1 2 1]))
                           :freq (* (at-i [800 1600])
                                    (at-i [1 2 3/2 17/7]))
                           :pan (rrand -1.0 1)
                           :offset (rand)
                           :range 0.4
                           :a (weighted {0 3
                                         0.1 1})
                           :s 0.8
                           :moog-freq (rrand 600 1000)
                           :moog-res (* (rand 0.2)
                                        (at-i [1 1]))
                           :d-level 0.7
                           :r (weighted {0 5
                                         1 5
                                         2 1
                                         3 1/2})
                           :out (bh 2)})))))
  (rain2/stop)
  (rain2/ref-rain
   :id :wt3
   :ref :wt1
   :durs [1/4]
   :on-event (rain2/on-event
              (when (xo "o" i)
                (mooglad (merge
                          (akwf (at-i [:ebass]))
                          {:amp (* 0.6 (at-i [1 1 1 3/2]))
                           :freq (* 80
                                    (at-i [1 1 1 1 2])
                                    (at-i [1 3/2 1 3/2 1 17/14]))
                           :offset (rand 0)
                           :range 0.01
                           :a (weighted {0.0 3
                                         0.02 1})
                           :s 0.2
                           :moog-freq (rrand 300 800)
                           :moog-res (* 0.4 (at-i [1 2]))
                           :d-level 0.3
                           :r 0.3
                           :out (bh 0)}))))))
