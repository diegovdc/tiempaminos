(ns tieminos.compositions.garden-earth.chord-clouds
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.string :as str]
   [erv.cps.core :as cps]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.base
    :refer
    [algo-note
     auto-scales
     const-log-fn
     eik
     get-cps-pitch-class
     log
     log-with-msg
     midi-in-event
     midi-out-1
     mpe-note-off
     mpe-note-on
     on-event
     ref-rain
     run-auto-scale
     stop
     sub-cps-scale
     wrap-at]]))

(comment
  ;; get dekanies
  (->> eik :subcps keys sort (filter #(str/includes? % "2)5")))
  (->> eik :subcps keys sort
       (filter #(str/includes? % "2)4"))
       (filter #(str/includes? % "9-"))
       (filter #(str/includes? % "5")))
  (->> eik :subcps keys sort
       (filter #(str/includes? % "3)4"))
       (filter #(str/includes? % "11"))
       (filter #(str/includes? % "7"))))

(def dekanies
  '("2)5 of 3)6 1-3.5.7.9.11"
    "2)5 of 3)6 3-1.5.7.9.11"
    "2)5 of 3)6 5-1.3.7.9.11"
    "2)5 of 3)6 7-1.3.5.9.11"
    "2)5 of 3)6 9-1.3.5.7.11"
    "2)5 of 3)6 11-1.3.5.7.9"))

(def hexanies-of-dek-9
  '("2)4 of 3)6 9-1.3.5.11"
    "2)4 of 3)6 9-1.3.5.7"
    "2)4 of 3)6 9-1.5.7.11"
    "2)4 of 3)6 9-3.5.7.11"))

(def tetranies-of-dek-3-with-5
  (->> eik :subcps keys
       (filter #(str/includes? % "3)4"))
       (filter #(str/includes? % "3"))
       (filter #(str/includes? % "5"))))

(def tetranies-of-dek-9-with-1
  (->> eik :subcps keys
       (filter #(str/includes? % "3)4"))
       (filter #(str/includes? % "9"))
       (filter #(str/includes? % "1"))))

(def tetranies-of-dek-9-with-11
  (->> eik :subcps keys
       (filter #(str/includes? % "3)4"))
       (filter #(str/includes? % "9"))
       (filter #(str/includes? % "11"))))

(def tetranies-of-dek-11-with-7
  (->> eik :subcps keys
       (filter #(str/includes? % "3)4"))
       (filter #(str/includes? % "11"))
       (filter #(str/includes? % "7"))))

(def tetranies-of-dek-11-with-9
  (->> eik :subcps keys
       (filter #(str/includes? % "3)4"))
       (filter #(str/includes? % "11"))
       (filter #(str/includes? % "9"))))

(def tetranies-of-dek-11-with-3
  (->> eik :subcps keys sort
       (filter #(str/includes? % "3)4"))
       (filter #(str/includes? % "11"))
       (filter #(str/includes? % "3"))))

(def chords [[0 3 4 6 8]
             [1 2 4 7 10]
             [-8 2 4 7 10 17]
             [-9  9 10 17]
             [-10 12 17 24]
             [-10 11 17 19 24]
             [-20 0 1 5 10 14]])

(defn rand-durs [dur-ms max-range chord]
  (mapv (fn [_]
          (let [deviation (rand max-range)]
            (if (> 0.3 (rand))
              (+ dur-ms deviation)
              (max 100 (- dur-ms (/ deviation 2))))))
        chord))

(defn rand-delays [max-delay chord]
  (reduce (fn [acc _] (conj acc (+ (last acc) (rand max-delay))))
          [0]
          chord))

(defn trim-chord [weights chord]
  (let [trim% (weighted weights)]
    (if (zero? trim%)
      chord
      (filter (fn [_] (> (rand) trim%))
              chord))))

(defn reverse-chord [chord]
  (if (> 0.7 (rand)) (reverse chord) chord))

(comment
  (run-auto-scale dekanies :dekany :id :auto-dekanies))

(comment
  "Explorar secuencias de 1 o 2 acordes, y/o de una serie de mas elementos pero menos densidad de notas.
El objetivos es que la flauta pueda tener su rol y no estar peleando por la atención con la síntesis"
  (stop))
(do
  (defn chord-clouds
    [chords
     & {:keys [id
               auto-scales-key
               tempo
               durs
               dur+delay-factor
               max-vel
               delay-weights
               offset-weights
               trim-weights]
        :or {id :chord-clouds
             auto-scales-key :dekany
             tempo 500
             durs [5 8 7 13]
             dur+delay-factor 0.3
             max-vel 45
             delay-weights {500 1, 1500 1, 3000 1, 7000 1, 8000 1}
             offset-weights {-20 5, -8 1, -5 2, 0 2, 5 2}
             trim-weights {0 70, 0.3 20, 0.5 6, 0.7 3, 0.8 1}}}]
    (ref-rain
     :id id
     :tempo tempo
     :durs durs
     :on-event (on-event
                (let [scale (@auto-scales auto-scales-key)
                      chord (vec (reverse-chord
                                  (trim-chord
                                   trim-weights
                                   (wrap-at index chords))))
                      delay (weighted delay-weights)]
                  (when-not scale
                    (timbre/error "Waiting for auto-scale to run with key"
                                  auto-scales-key))
                  (when (and scale dur-ms chord (seq chord))
                    (print ":")
                    (algo-note :sink midi-out-1
                               :dur (mapv #(+ (* dur+delay-factor delay) %)
                                          (rand-durs dur-ms 7000 chord))
                               :scale scale
                               :base-freq 440
                               :get-pitch-class get-cps-pitch-class
                               :deg-offset (weighted offset-weights)
                               :midi-note chord
                               :vel (mapv (fn [_] (+ 5 (rand-int max-vel))) chord)
                               :delay (rand-delays delay chord)))))))

  ;;   2 3 | 1 2 3 -- wt + bass tone
  ;;   2 3 | 1   3 -- otone
  ;;   2 3 | 1i i3 -- otone
  ;; 1 2 3 | 1i i3 -- otone
  ;; 1 2 3x| 1 2 3
  (defn light-clouds []
    (run-auto-scale tetranies-of-dek-3-with-5
                    :dekany
                    :id :auto-dekanies
                    :log-fn (const-log-fn
                             ["  2 3 | 1 2 3 -- wt + bass tone, multi"
                              "  2 3 | 1   3 -- otone"
                              "1 2 3 | 1x x3 -- otone, play with x"]))

    (chord-clouds [[0] [2] [-1 1] [2 0] [-1 1] [-2 3]]
                  :offset-weights {4 90, 5 10, 0 3, -4 1}))

  (defn very-light-clouds []
    (run-auto-scale ["3)4 of 3)6 1.3.5.7"
                     "3)4 of 3)6 1.3.5.9"]
                    :dekany
                    :id :auto-dekanies
                    :log-fn (const-log-fn
                             ["1 2 3 | 1 2 3 -- play with rh tones"
                              "  2 3 | 1 2 3 -- wt + bass tone, multi"
                              "  2 3 |   2 3"]))

    (chord-clouds [[0] [-1 1] [2] [2 0] [-1 1] [-2 3]]
                  :offset-weights {4 60, 5 10, 0 3, -4 60, -5 40, -10 5}
                  :delay-weights {200 80, 300 20}))
  (defn consonant-clouds []
    (let [log-fn (const-log-fn
                  ["otone tremoloing (keys in parenthesis)"
                   " 1 2 3 |(1 2) "
                   "   2 3 |(1 2) "
                   " 1 2 3 | (x x) "
                   "   2 3 | (x x) "
                   "   2(3)| (x x) "])]
      (log-with-msg
       "Consonantish clouds"
       (run-auto-scale ["1)4 of 3)6 7.11-1.3.5.9"
                        "1)4 of 3)6 9.11-1.3.5.7"]
                       :dekany
                       :id :auto-dekanies
                       :log-fn log-fn
                       :durs [300 500 800])))

    (log
     (chord-clouds [[0] [-1 1] [2 -11] [2 0] [0 5] [-1 1 -10] [-2 3]]
                   :max-vel 55
                   :dur+delay-factor 1.5
                   :offset-weights {4 60, 5 10, 0 3, -4 60, -5 40, -10 5}
                   ;; :delay-weights {0 50, 200 30, 300 20}
                   )))
  (defn dark-light-clouds []
    (let [log-fn (const-log-fn
                  ["otone tremoloing (keys in parenthesis)"
                   " 1 2 3 |(1 2) "
                   "   2 3 |(1 2) "
                   " 1 2 3 | (x x) "
                   "   2 3 | (x x) "
                   "   2(3)| (x x) "])]
      (log-with-msg
       "dark light clouds"
       (run-auto-scale ["2)4 of 3)6 11-1.3.5.9"
                        "2)4 of 3)6 9-1.3.5.11"
                        "2)4 of 3)6 7-1.3.5.11"
                        "2)4 of 3)6 11-1.3.5.7"]
                       :dekany
                       :id :auto-dekanies
                       :log-fn log-fn)))

    (log
     (chord-clouds [[0] [-1 1] [2 -11] [2 0] [0 5] [-1 1 -10] [-2 3]]
                   :max-vel 55
                   :dur+delay-factor 1.5
                   :offset-weights {4 60, 5 10, 0 3, -4 60, -5 40, -10 5}
                   ;; :delay-weights {0 50, 200 30, 300 20}
                   )))

  (defn will-darken []
    (let [log-fn (const-log-fn
                  ["otone tremoloing (keys in parenthesis)"
                   " 1 2 3 |(1 2) "
                   "   2 3 |(1 2) "
                   " 1 2 3 | (x x) "
                   "   2 3 | (x x) "
                   "   2(3)| (x x) "])]
      (log-with-msg
       "Will darken"
       (run-auto-scale (mapcat #(repeat 3 %)
                               ["3)4 of 3)6 1.3.5.9"
                                "3)4 of 3)6 1.3.5.11"
                                ;; "1)4 of 3)6 7.11-1.3.5.9"
                                ;; "1)4 of 3)6 7.9-1.3.5.11"
                                ;; "1)4 of 3)6 7.9-1.3.5.11"
                                ;; "1)4 of 3)6 9.11-1.3.5.7"
                                ])
                       :dekany
                       :id :auto-dekanies
                       :log-fn log-fn
                       :durs [300 500 800])))

    (log
     (chord-clouds [[0] [-1 1] [2 -11] [2 0] [0 5] [-1 1 -10] [-2 3]]
                   :max-vel 55
                   :dur+delay-factor 1.5
                   :offset-weights {4 60, 5 10, 0 3, -4 60, -5 40, -10 5})))
  (defn tranquilo
    "Bajo radulesquiano, para interludio solista en flauta"
    []
    (let [log-fn (const-log-fn
                  ["otone tremoloing (keys in parenthesis)"
                   " 1 2 3 |(1 2) "
                   "   2 3 |(1 2) "
                   " 1 2 3 | (x x) "
                   "   2 3 | (x x) "
                   "   2(3)| (x x) "])]
      (log-with-msg
       "Tranquilo"
       (run-auto-scale (mapcat #(repeat 4 %)
                               ["1)4 of 3)6 1.3-5.7.9.11"
                                "1)4 of 3)6 7.9-1.3.5.11"])
                       :dekany
                       :id :auto-dekanies
                       :log-fn log-fn)))

    (log
     (chord-clouds [[0]
                    [0]
                    [0 2 7]
                    [0]
                    [0 4]]
                   :id :tranquilo
                   :max-vel 25
                   :dur+delay-factor 20
                   :durs [5 3 13]
                   :offset-weights {-10 60}
                   ;; :offset-weights {4 60, 5 10, 0 3, -4 60, -5 40, -10 5}
                   :delay-weights {0 50, 200 30, 300 20, 400 40})))
  (defn tranquilo-agudo []
    (let [log-fn (const-log-fn
                  ["   2 3 | 1 2 "])]
      (log-with-msg
       "Tranquilo pt2"
       (run-auto-scale (mapcat #(repeat 4 %)
                               ["1)4 of 3)6 1.3-5.7.9.11"
                                "1)4 of 3)6 7.9-1.3.5.11"])
                       :dekany
                       :id :auto-dekanies
                       :log-fn log-fn)))

    (log
     (chord-clouds [[0] [-1 1] [2 0] [0 5] [-2 3]]
                   :max-vel 10
                   :dur+delay-factor -0.5
                   :durs [5 3 2] #_[3 2 2]
                   :offset-weights {4 60, 5 10}
                   :delay-weights {0 500, 200 30, 300 20})))
  #_(tranquilo-agudo))

#_(@auto-scales :dekany)
(comment
  (stop)

  ;; for playing with a keyboard
  (ref-rain
   :id :auto-dekanies
   :tempo 90
   :durs [4] #_[13 13 21]
   :on-event (on-event
              (let [cps-name (wrap-at index @hexanies-of-dek-9-)
                    scale    (sub-cps-scale cps-name)]

                (timbre/info "SETTING new CPS\n" cps-name "\n\n\n\n\n\n")

                (midi-in-event
                 :note-on (fn [msg]
                            (timbre/info "vel" (msg :velocity))
                            (mpe-note-on :sink midi-out-1
                                         :scale scale
                                         :base-freq 30
                                         :get-pitch-class get-cps-pitch-class
                                         :deg-offset -50
                                         :midi-note (msg :note)
                                         :vel (msg :velocity)))
                 :note-off #(mpe-note-off midi-out-1 (% :note)))))))

(comment
  (midi-in-event
   :note-on (fn [msg]
              #_(mpe-note-on :sink midi-out-1
                             :scale (get-in
                                     eik
                                     [:subcps "3)4 of 3)6 1.3.7.11" :scale])
                             :base-freq 30
                             :get-pitch-class get-cps-pitch-class
                             :deg-offset -50
                             :midi-note (msg :note)
                             :vel (msg :velocity)))
   :note-off #(mpe-note-off midi-out-1 (% :note))))
