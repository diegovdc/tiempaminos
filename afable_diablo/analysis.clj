(ns analysis
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.data.generators :refer [weighted]]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv-fib-synth.overtone-extensions :refer [defsynth]]
   [erv.cps.core :as cps]
   [erv.cps.similarity :as cpss]
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [overtone.core :as o]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain stop]]))

(do
  (defn all-rotations
    "Zero based rotations. `scale` is in cents"
    [scale]
    (map (fn [note] (sort (map #(mod (- % note) 1200) scale))) scale))

  (def dorico-six-notes
    (->> (combo/combinations [0 200 300 500 700 900 1000] 6)
         (mapcat all-rotations)
         #_(filter #(and ((set %) 300)))
         set)))

(comment
  ;; cps-sorted-by-euclidean-distance
  (-> (range 1 31 2)
      (combo/combinations 4)
      (->> (pmap #(->> (cps/make 2 %)
                       (cpss/+gens %)
                       cpss/+cents
                       cpss/+euclidean-distance))
           (sort-by :euclidean-distance)
           (filter #(dorico-six-notes
                     (:closest-12-edo %)))
           (sort-by :euclidean-distance)
           (group-by #(->> (:factors %)
                           set
                           (set/intersection #{1 3 9 19})
                           count))
           (#(dissoc % 0))
           (map (fn [[k v]]
                  [k (map (juxt :euclidean-distance :factors :cents) v)]))))

  (-> (combo/combinations [1 3 9 19 15 21 7] 4)
      (->> (pmap #(->> (cps/make 2 %)
                       (cpss/+gens %)
                       cpss/+cents
                       cpss/+euclidean-distance))
           (sort-by :euclidean-distance)
           ;; can use filter to look for scales that fit dorian
           (filter #(dorico-six-notes
                     (:closest-12-edo %)))
           (sort-by :euclidean-distance)
           (map (juxt :euclidean-distance :factors :cents)))
      #_  count))


(defn +cents [scale]
  (map #(assoc % :cents (conv/ratio->cents (:bounded-ratio %)))
       scale))
(defn +degree [scale]
  (map-indexed (fn [i n] (assoc n :degree i)) scale))

(def polydori
  (->> (cps/make 4 [1 3 9 19 15 21 7] :norm-fac (* 7 1 15 19))
       :scale
       +cents
       +degree))

(->> (cps/make 4 [1 3 9 19 15 21 7])
     cps/+all-subcps
     :subcps
     keys
     (filter #(str/includes? % "2)4"))
     (filter #(str/includes? % "3.7.19.21"))

     )

(-> (cps/make 4 [1 3 9 19 15 21 7] :norm-fac 315 #_(* 49/3 513))
    cps/+all-subcps
    :subcps
    ;; keys
    (select-keys '("2)4 of 4)7 1.15-3.7.19.21"
                   "2)4 of 4)7 9.15-3.7.19.21"
                   "2)4 of 4)7 1.9-3.7.19.21"))
    seq
    (nth 0)
    second
    :scale
    +cents)



;; dorico 1 "2)4 of 4)7 7.15-1.3.9.19" with added 3th
(def dorico-1
  '({:set #{7 15 19 9},
     :ratio 1,
     :bounded-ratio 1,
     :bounding-period 2,
     :cents 0.0}
    {:set #{7 1 15 3},
     :ratio 1/57,
     :bounded-ratio 64/57,
     :bounding-period 2,
     :cents 200.5319830023106}
    ;; adding 3th -opt 1
    #_{:set #{21 3 19 9}, :cents 315.64128700055255}
    ;; adding 3th - opt 2
    #_{:set #{7 1 21 9}, :cents 284.99917647198805}
    ;; adding 3th - opt 3
    {:set #{7 1 21 19} :bounded-ratio 7/5 :cents 378.6021908735147}

    {:set #{7 15 3 9},
     :ratio 3/19,
     :bounded-ratio 24/19,
     :bounding-period 2,
     :cents 404.4419847330848}
    {:set #{7 15 3 19},
     :ratio 1/3,
     :bounded-ratio 4/3,
     :bounding-period 2,
     :cents 498.04499913461217}
    {:set #{7 1 15 9},
     :ratio 1/19,
     :bounded-ratio 32/19,
     :bounding-period 2,
     :cents 902.4869838676968}
    {:set #{7 1 15 19},
     :ratio 1/9,
     :bounded-ratio 16/9,
     :bounding-period 2,
     :cents 996.0899982692252}))

(all-rotations '(0 201 404 498 903 996))


;; dorico 2 "2)4 of 4)7 7.21-1.3.9.19" -- needs 4th
(def dorico-2
  '({:set #{7 1 15 19},
     :ratio 1,
     :bounded-ratio 1,
     :bounding-period 2,
     :cents 0.0}
    {:set #{7 15 19 9},
     :ratio 9,
     :bounded-ratio 9/8,
     :bounding-period 2,
     :cents 203.91000173077484}
    ;; add 4th opt 1
    {:set #{7 15 21 19} :cents 470.7809073345124}
    ;; add 4th opt 2
    {:set #{7 1 21 9} :cents 488.909178202762}
    ;; add 4th opt 3
    {:set #{21 3 19 9} :cents 519.5512887313275}

    {:set #{7 1 19 9},
     :ratio 3/5,
     :bounded-ratio 6/5,
     :bounding-period 2,
     :cents 315.64128700055255}
    {:set #{7 15 3 19},
     :ratio 3,
     :bounded-ratio 3/2,
     :bounding-period 2,
     :cents 701.9550008653874}
    {:set #{7 1 3 19},
     :ratio 1/5,
     :bounded-ratio 8/5,
     :bounding-period 2,
     :cents 813.6862861351653}
    {:set #{7 3 19 9},
     :ratio 9/5,
     :bounded-ratio 9/5,
     :bounding-period 2,
     :cents 1017.5962878659401}))

(def dorico-3
  "dorico 3 \"2)4 of 4)7 1.15-3.7.19.21\" needs 2nd"
  '({:set #{7 1 15 3}, :ratio 1, :bounded-ratio 1, :bounding-period 2, :cents 0.0}
    ;; add 2nd opt 1
    {:set #{1 15 21 9} :cents 203.91000173077484}
    ;; add 2nd opt 2
    {:set #{7 1 21 19} :cents 178.0702078712047}

    {:set #{1 15 21 19},
     :ratio 19,
     :bounded-ratio 19/16,
     :bounding-period 2,
     :cents 297.5130161323026}
    {:set #{1 15 3 19},
     :ratio 19/7,
     :bounded-ratio 19/14,
     :bounding-period 2,
     :cents 528.6871096631775}
    {:set #{1 15 21 3},
     :ratio 3,
     :bounded-ratio 3/2,
     :bounding-period 2,
     :cents 701.9550008653874}
    {:set #{7 1 15 19},
     :ratio 19/3,
     :bounded-ratio 19/12,
     :bounding-period 2,
     :cents 795.5580152669148}
    {:set #{7 1 15 21},
     :ratio 7,
     :bounded-ratio 7/4,
     :bounding-period 2,
     :cents 968.8259064691249}))


(defsynth sini [freq 440 amp 1 r 0.5 out 0]
  (o/out out (-> [(o/sin-osc freq)
                  (o/sin-osc (* 2 freq))
                  (o/sin-osc (* 3 freq))
                  #_(o/sin-osc (* 5 freq))]
                 o/mix
                 (o/lpf 2500)
                 o/pan2
                 (* amp (o/env-gen (o/env-perc 0.05 (* 0.5 r)) :action o/FREE)))))



(def polydori-by-sets (->> polydori (group-by :set)))

(->> dorico-1 (mapcat (comp polydori-by-sets :set)))

(do
  (let [scale (->> dorico-1 (mapcat (comp polydori-by-sets :set)))]
    (ref-rain :id :test
              :durs [3 2 2 3 2
                     3 2 2 3 2
                     3 2 2 3 2
                     3 2 2 3 3
                     ;; 2 1 2 2 2 1 2
                     ]
              :ratio 1/9
              :on-event
              (on-event
               (let [deg (weighted {0 10
                                    1 4
                                    2 5
                                    3 3
                                    4 7
                                    5 8
                                    6 4})
                     base-freq (* 440 (weighted {1 5
                                                 2 3
                                                 1/2 4
                                                 1/4 2}))
                     dur (weighted {0.5 1 1 5 1.5 8 2 8 3 2 5 1})]
                 #_(sini :freq (scale/deg->freq scale base-freq deg)
                       :r dur))))

    (ref-rain :id :test2
              :durs [3 2 2 3 2
                     3 2 2 3 2
                     2 1 2 1 1 2 1 2
                     2 1 2 2 2 1 2
                     ]
              :ratio 1
              :on-event
              (on-event
               (let [deg (weighted {0 10
                                    1 4
                                    2 5
                                    3 3
                                    4 7
                                    5 8
                                    6 4})
                     base-freq (* 440 (weighted {1/8 2}))
                     dur (weighted {0.5 1 1 5 1.5 8 2 8 3 2 5 1})]
                 (sini :freq (scale/deg->freq scale base-freq deg)
                       :r dur))))))
(stop)
(o/stop)

(sini)

(o/recording-start "/home/diego/Desktop/doriquiando.wav")
(o/recording-stop)
