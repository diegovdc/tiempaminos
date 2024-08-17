(ns tieminos.compositions.garden-earth.trainer
  (:require
   [clojure.set :as set]
   [erv.scale.core :refer [+names]]
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [erv.utils.core :refer [interval]]
   [overtone.core :as o]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [bh]]
   [tieminos.compositions.garden-earth.base
    :refer
    [base-freq on-event pitch-class->pr-fingering ref-rain subcps]]
   [tieminos.compositions.garden-earth.synths.general :refer [tuning-monitor]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp]))

(def *1oo4
  ["1)4 of 3)6 1.11-3.5.7.9"
   "1)4 of 3)6 3.11-1.5.7.9"
   "1)4 of 3)6 5.11-1.3.7.9"
   "1)4 of 3)6 7.11-1.3.5.9"
   "1)4 of 3)6 9.11-1.3.5.7"
   "1)4 of 3)6 1.9-3.5.7.11" ;; difcil
   "1)4 of 3)6 3.9-1.5.7.11" ;; clara
   "1)4 of 3)6 5.9-1.3.7.11" ;; disonante (tonos relativamente cercanos) pero muy hermosa, mística, intensa, sobre todo cuando satura
   "1)4 of 3)6 7.9-1.3.5.11"
   "1)4 of 3)6 1.7-3.5.9.11"
   "1)4 of 3)6 3.7-1.5.9.11"
   "1)4 of 3)6 5.7-1.3.9.11"
   "1)4 of 3)6 1.5-3.7.9.11"
   "1)4 of 3)6 3.5-1.7.9.11"
   "1)4 of 3)6 1.3-5.7.9.11"])

(def *3oo4
  ["3)4 of 3)6 1.3.5.11"
   "3)4 of 3)6 1.3.5.7"
   "3)4 of 3)6 1.3.5.9" ;; muy chida, recuerda a algo folcórico
   "3)4 of 3)6 1.3.7.11"
   "3)4 of 3)6 1.3.7.9"
   "3)4 of 3)6 1.3.9.11"
   "3)4 of 3)6 1.5.7.11"
   "3)4 of 3)6 1.5.7.9"
   "3)4 of 3)6 1.5.9.11"
   "3)4 of 3)6 1.7.9.11" ;; muy bello acorde, relajante (si se agrega A+53 se puede tocar una frase de In C)
   "3)4 of 3)6 3.5.7.11"
   "3)4 of 3)6 3.5.7.9"
   "3)4 of 3)6 3.5.9.11" ;; desierto - explorada conjuntamente con la de abajo - fue difícil percibir su caracter el día que las toqué, pero escuchando la grabación sugiere un espacio vacío y desierto
   "3)4 of 3)6 3.7.9.11" ;; desierto
   "3)4 of 3)6 5.7.9.11"])


(def known-pitches
    #{"A+92" ;; TODO comprar con el otro A
      "B+56"
      "C+20"
      "C#+40"
      "D+24"
      "D+90"
      "D#+75"
      "E+55"
      "F+56"
      "F#+6"
      "G+22"
      "G#+73"
      "G+88"})

(defn unknown-pitches-chords
  [known-pitches-set
   subcps-names]
  (let [chords (map
                 (fn [subcps-name]
                   [subcps-name
                    (+names base-freq (subcps subcps-name))])
                 subcps-names)]
    (->> chords
         (map (fn [[name* chord]]
                (let [unknown (set/difference (set (map (comp :class :pitch) chord))
                                              known-pitches-set)]
                  {:subcps name*
                   :unknown-pitches-count (count unknown)
                   :unknown-pitches unknown
                   })
                ))
         (sort-by :unknown-pitches-count >))))

(comment
  (count known-pitches)
  (unknown-pitches-chords
    known-pitches
    (concat *1oo4 *3oo4))

  (do
    (def scale-index 7)
    (+names base-freq (subcps (*3oo4 scale-index))))
  (scale/print-scale-intervals! (subcps (*1oo4 scale-index))
                                :unit :ratios)
  (scale/print-scale-intervals! (subcps (*1oo4 scale-index))
                                :unit :cents)
  (o/stop)
  (gp/stop)
  (let [scale (+names base-freq (subcps "3)4 of 3)6 1.3.5.9"
                                        #_(*3oo4 scale-index)))
        last-interval (atom '(1 1))]
    (ref-rain
      :id ::trainer
      :durs [3]
      :on-event
      (on-event
        (let [note (or (nth scale (rand-nth [0 1 2 3]))
                       #_(rand-nth scale))
              _ (swap! last-interval
                       #(->> (conj % (:bounded-ratio note))
                             (take 2)))
              interval* (apply interval (sort < @last-interval))]
          (println :interval
                   interval*
                   (int (conv/ratio->cents interval*))
                   "\n")
          (tuning-monitor               ; synth
            :freq (* (rand-nth [220 880 440]) (:bounded-ratio note))
            :a 5
            :r 5
            :pan (rrange -0.5 0.5)
            :amp (rrange 0.125 0.25)
            :out (bh 0))
          (println (pitch-class->pr-fingering
                     (-> note :pitch :class))
                   "\n\n"))))
    (ref-rain
      :id ::trainer2
      :durs (fn [_] (rand-nth [1 3]))
      :on-event
      (on-event
        (let [note (or (nth scale (rand-nth [0 1 2 3]))
                       #_(rand-nth scale))
              _ (swap! last-interval
                       #(->> (conj % (:bounded-ratio note))
                             (take 2)))
              interval* (apply interval (sort < @last-interval))]
          (println :interval
                   interval*
                   (int (conv/ratio->cents interval*))
                   "\n")
          (tuning-monitor               ; synth
            :freq (* 4 (rand-nth [220 880 440]) (:bounded-ratio note))
            :a (rand 0.2)
            :r (rrange 1.0 7)
            :pan (rrange -1 1)
            :amp (rand 0.2)
            :lpf-freq 5000
            :out (bh 0))
          (println (pitch-class->pr-fingering
                     (-> note :pitch :class))
                   "\n\n"))))))
