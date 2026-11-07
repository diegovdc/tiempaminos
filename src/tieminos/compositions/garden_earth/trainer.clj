(ns tieminos.compositions.garden-earth.trainer
  (:require
   [clojure.set :as set]
   [erv.scale.core :as scale :refer [+names]]
   [erv.utils.conversions :as conv]
   [erv.utils.core :refer [interval]]
   [overtone.core :as o]
   [tieminos.compositions.7d-percusion-ensamble.base :refer [bh]]
   [tieminos.compositions.garden-earth.base
    :refer [base-freq eik eik-ratios on-event pitch-class->pr-fingering
            ref-rain subcps]]
   [tieminos.compositions.garden-earth.synths.general
    :refer [tuning-monitor]]
   [tieminos.compositions.garden-earth.web.ajax :refer [post-fingering2]]
   [tieminos.harmonic-experience.lattice :as hexp.lattice]
   [tieminos.harmonic-experience.trainer :as hexp.trainer]
   [tieminos.harmonic-experience.utils :as hexp.utils]
   [tieminos.midi.core :refer [get-exquis!]]
   [tieminos.seq-utils.core :refer [choose]]
   [tieminos.seq-utils.utils :refer [repcat]]
   [tieminos.utils :refer [map-subscale-degs rrange]]
   [time-time.dynacan.players.gen-poly :as gp]))

(def *1oo4
  ["1)4 of 3)6 1.11-3.5.7.9" ;; Bluesera
   "1)4 of 3)6 3.11-1.5.7.9"
   "1)4 of 3)6 5.11-1.3.7.9"
   "1)4 of 3)6 7.11-1.3.5.9"
   "1)4 of 3)6 9.11-1.3.5.7"
   "1)4 of 3)6 1.9-3.5.7.11" ;; difícil
   "1)4 of 3)6 3.9-1.5.7.11" ;; clara
   "1)4 of 3)6 5.9-1.3.7.11" ;; disonante (tonos relativamente cercanos) pero muy hermosa, mística, intensa, sobre todo cuando satura ;; en una segunda exploración también me gustó, pero me pareció más sencilla, menos disonante, pero porque la experimenté desde el especto 7:8:11:12
   "1)4 of 3)6 7.9-1.3.5.11"
   "1)4 of 3)6 1.7-3.5.9.11"
   "1)4 of 3)6 3.7-1.5.9.11"
   "1)4 of 3)6 5.7-1.3.9.11" ;; difícil
   "1)4 of 3)6 1.5-3.7.9.11" ;; le gustó a Dinoh
   "1)4 of 3)6 3.5-1.7.9.11"
   "1)4 of 3)6 1.3-5.7.9.11"])

(def *3oo4
  ["3)4 of 3)6 1.3.5.11"
   "3)4 of 3)6 1.3.5.7"
   "3)4 of 3)6 1.3.5.9" ;; muy chida, recuerda a algo folcórico
   "3)4 of 3)6 1.3.7.11"
   "3)4 of 3)6 1.3.7.9"
   "3)4 of 3)6 1.3.9.11" ;; difícil, quizá muy neutro o aún no le encuentro mucho
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
  #{"A+53"
    "A+92" ;; TODO: comprar con el otro A
    "A#+55"
    "B+56"
    "C+20"
    "C+59"
    "C#+40"
    "C#+71"
    "D+24"
    "D+90"
    "D#+75"
    "E+6"
    "E+55"
    "F+56"
    "F#+6" ;; TODO: comparar con el otro F
    "F#+38"
    "G+22"
    "G+88"
    "G#+73"})

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
                   :unknown-pitches unknown})))

         (sort-by :unknown-pitches-count >))))

(defn make-subcps
  [subcps-str]
  (let [subcps* (+names base-freq (subcps subcps-str))]
    (map
     (fn [{:keys [bounded-ratio] :as cps}]
       (assoc cps
              :degree (-> bounded-ratio
                          eik-ratios
                          :degree)))
     subcps*)))

(defn- filter-by-pitch-class
  [pitch-class-names eik-scale]
  (let [pc-set* (set pitch-class-names)]
    (filter #(-> % :pitch :class pc-set*)
            eik-scale)))

(comment
  (require '[tieminos.compositions.garden-earth.base :refer [eik]]
           '[erv.cps.core :as cps])

  ;; TODO: move to erv lib
  (defn subset-from-degs
    "Make a subset of a scale from vector of degrees"
    [scale degs]
    (mapv (fn [deg] (nth scale deg))
          degs))

  (-> eik)
  (map-subscale-degs 20 [0] 1)

  (subset-from-degs (:scale eik)
                    (map :degree (make-subcps "2)4 of 3)6 7-1.3.9.11")))

  (map (comp :class :pitch) (subcps "3)4 of 3)6 1.7.9.11"))
  (-> eik :subcps
      (get "3)5 of 3)6 1.3.5.7.9")
      cps/+all-subcps
      :subcps
      keys
      sort)

  (->> ["2)4 of 3)6 1-3.7.9.11" ;; probar
        "2)4 of 3)6 11-1.3.7.9" ;; se ve fácil y sencilla de escuchar, pero casi tradicional
        "2)4 of 3)6 3-1.7.9.11"
        "2)4 of 3)6 7-1.3.9.11" ;; se ve difícil pero interesante
        "2)4 of 3)6 9-1.3.7.11"]
       (map subcps)
       (map #(map (juxt (comp sort :set) (comp :class :pitch)) %))))

(comment
  (def root 440)
  (def subcps* (make-subcps
                #_"1)4 of 3)6 5.9-1.3.7.11"
                #_"1)4 of 3)6 1.5-3.7.9.11"
                #_"3)4 of 3)6 1.7.9.11" ;; usar
                #_"2)4 of 3)6 7-1.3.9.11" ;; usar, brillante-reflejante, buenas pentatónicas, pero difícil
                "2)4 of 3)6 9-1.5.7.11"))

  (->> subcps* #_(map (juxt :set (comp :name :pitch))))
  (repcat [10 (choose 0 1)]
          [10 (choose 0 1 5)]
          [5 (choose  1)]
          [5 (choose  1)]
          [10 (choose  1 3)]
          [10 (choose  1 3 4)]
          [20 (choose  2 3 4)])

  (->> (repcat [10 [0 1]]
               [10 [0 1 5]]
               [5 [1]]
               [10 [1 3]]
               [10 [1 3 4]]
               [20 [2 3 4]])
       (map (partial map #(:degree (nth subcps* %))))
       (mapv #(apply choose %)))

  (hexp.utils/set-output-mode! :reaper)
  (hexp.lattice/setup-kb
   {:root root
    :scale (:scale eik)
     ;; :midi-kb (get-exquis!)
    :kb-degs (map :degree subcps*)
    :synth-config {:amp 0.7
                   :a 0.1
                   :out (hexp.utils/out 26)}
    :lattice-config {:width 1440
                     :height 900
                     :ratio->node-name (->> eik
                                            :scale
                                            (reduce
                                             (fn [m {:keys [bounded-ratio pitch]}]
                                               (assoc m bounded-ratio (:class pitch)))
                                             {}))}})
  (hexp.trainer/trainer
   {:root root
    :scale (:scale eik)
    :degrees (->> (repcat [10 [0 1]]
                          [10 [0 1 5]]
                          [5 [1]]
                          [10 [1 3]]
                          [7 [1 3 4]]
                          [7 [2 3 4]]
                          [5 [2]]
                          [10 [2 1]])
                  (map (partial map #(:degree (nth subcps* %))))
                  (mapv #(apply choose %)))
    #_(->> subcps*
           #_(filter-by-pitch-class #{"G#+73"})
           (map :degree)
           (apply choose))
    :tempo 60
    :print-info? false
    :a {9 3, 12 1}
    :r {9 3, 12 1}
    :periods {1 5, 2 1, 1/2 3 1/4 2}
    :on-note-play (let [pcs (atom ())]
                    (fn [{:keys [_last-interval note interval _freq]}]
                      #_(println :interval
                                 interval
                                 (int (conv/ratio->cents interval)))
                      (let [pc (str
                                (pitch-class->pr-fingering
                                 (-> note :pitch :class))
                                "\n"
                                (str "  interval " interval " " (int (conv/ratio->cents interval))
                                     "c\n\n"))]
                        (println pc)
                        (swap! pcs conj pc))
                      (try (post-fingering2 (take 3 @pcs))
                           (catch Exception _ nil))))
    :synth/params-fn (fn [{:keys [freq]}]
                       {:pan (rrange -1 1)
                        :amp (rrange 0.3 0.6)
                        :lpf-freq freq})
    :out (hexp.utils/out 26)})

  (hexp.trainer/stop))

(comment
  (count known-pitches)
  (unknown-pitches-chords
   known-pitches
   (concat *1oo4 *3oo4))

  (def scale-index 7)

  (def cps
    #_"1)4 of 3)6 5.9-1.3.7.11"
    "1)4 of 3)6 1.5-3.7.9.11")

  (scale/print-scale-intervals! (subcps cps)
                                :unit :ratios)
  (scale/print-scale-intervals! (subcps cps)
                                :unit :cents)
  (o/stop)
  (gp/stop ::trainer)
  (let [scale (+names base-freq (subcps cps
                                        #_(*3oo4 scale-index)))
        last-interval (atom '(1 1))]
    (ref-rain
     :id ::trainer
     :durs (fn [_] (rand-nth [5 8 10]))
     :on-event
     (on-event
      (let [degrees [0 1 2 3] #_(cond
                                  (< index 10) [2 3]
                                  (< index 20) [0 2 3]
                                  (< index 40) [1 2 3]
                                  (< index 50) [1 2]
                                  (< index 65) [0 1 2]
                                  (< index 80) [0 2]
                                  (< index 90) [0 2 3]
                                  :else [0 1 2 3])
            note (nth scale (rand-nth degrees))
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
         :a 6
         :r 6
         :pan (rrange -0.5 0.5)
         :amp (rrange 0.125 0.25)
         :out (bh (+ 6 (rand-int 28))))
        (println (pitch-class->pr-fingering
                  (-> note :pitch :class))
                 "\n\n"))))))
