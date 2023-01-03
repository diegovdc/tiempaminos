(ns tieminos.piraran.dorian-scales
  (:require
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [tieminos.piraran.scale :refer [polydori polydori-v2]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(def dorico-1v1
  "dorico 1 -  2)4 of 4)7 7.15-1.3.9.19 with added 3th.
  NOTE: this one has errors (no third, two sixths) but sounds nice, use v2"
  [{:set #{7 15 19 9} , :ratio 1, :bounded-ratio 1, :bounding-period 2, :cents 0.0}
   {:set #{7 1 15 3} , :ratio 1/57, :bounded-ratio 64/57, :bounding-period 2, :cents 200.5319830023106}
   {:set #{7 1 21 19} :bounded-ratio 7/5 :cents 378.6021908735147}
   {:set #{7 15 3 9} , :ratio 3/19, :bounded-ratio 24/19, :bounding-period 2, :cents 404.4419847330848}
   {:set #{7 15 3 19} , :ratio 1/3, :bounded-ratio 4/3, :bounding-period 2, :cents 498.04499913461217}
   {:set #{7 1 15 9} , :ratio 1/19, :bounded-ratio 32/19, :bounding-period 2, :cents 902.4869838676968}
   {:set #{7 1 15 19} , :ratio 1/9, :bounded-ratio 16/9, :bounding-period 2, :cents 996.0899982692252}])

(def anti-dorico-1v1
  (let [dorico (set (map :set dorico-1v1))]
    (->> polydori :scale (remove #(dorico (:set %))))))

(def dorico-1v2
  "NOTE this is the best version
  dorico 1 \"2)4 of 4)7 7.15-1.3.9.19\" with added 1st"
  [{:set #{15 21 19 9}
    :ratio 1
    :bounded-ratio 1
    :bounding-period 2
    :degree 0
    :cents 0.0}
   {:set #{7 1 15 9}
    :ratio 1/57
    :bounded-ratio 64/57
    :bounding-period 2
    :cents 200.5319830023106}
   {:set #{7 1 15 19}
    :ratio 1/27
    :bounded-ratio 32/27
    :bounding-period 2
    :cents 294.1349974038373}
   {:set #{7 15 19 9}
    :ratio 1/3
    :bounded-ratio 4/3
    :bounding-period 2
    :cents 498.04499913461217}
   {:set #{7 1 15 3}
    :ratio 1/171
    :bounded-ratio 256/171
    :bounding-period 2
    :cents 698.5769821369228}
   {:set #{7 15 3 9}
    :ratio 1/19
    :bounded-ratio 32/19
    :bounding-period 2
    :cents 902.4869838676968}
   :ratio 1/9
   :bounded-ratio 16/9
   :bounding-period 2
   :cents 996.0899982692252])

(comment
  (require '[tieminos.synths :as s]
           '[tieminos.afable-diablo.analysis :refer [dorian-hexanies-in-polydori]]
           '[tieminos.utils :refer [map-subscale-degs]])

  (gp/stop)

  (defn- deg->freq [base-freq diatonic-scale-idx degree]
    (scale/deg->freq (:scale polydori-v2)
                     base-freq
                     (map-subscale-degs (count (:scale polydori-v2))
                                        (:degrees
                                         (nth
                                          dorian-hexanies-in-polydori
                                          diatonic-scale-idx))
                                        degree)))
  (ref-rain
   :id ::scale-tester-1
   :durs [3 2 2]
   :ratio 1/9
   :on-event (on-event
              (let [deg (at-i [(at-i [0 1 12 0 6 7])
                               2
                               (at-i [-1 -7 -8])
                               4
                               (at-i [5 6])
                               8
                               (at-i [7 11 6])
                               (at-i [7 14 13])])]
                ((rand-nth [s/low s/short-plate])
                 (deg->freq (rand-nth [50 100]) (at-i [0 0 0 0 0 9 9 9 9]) deg)
                 :atk (rand-nth [0.1])
                 :mod-freq (rrand 300 10000))
                #_((rand-nth [s/low s/short-plate])
                   (deg->freq 400 (at-i [2 2 2 2 2 11 11 11  11]) deg)
                   :dcy 1
                   :amp (at-i [0.5 0.3 0.8])
                   :mod-freq (rrand 300 10000)))))
  (gp/stop)
  (ref-rain
   :id ::scale-tester-2
   :durs [5 5 3]
   :ratio 1/9
   :on-event (on-event
              (case (mod index 2)
                0 ((rand-nth [s/low s/short-plate])
                   (deg->freq 100 2 (at-i [0 -4 2 5 8 7 11]))
                   :atk 3
                   :dcy 3
                   :amp (rand 0.7)
                   :mod-freq (rrand 600 10000))
                1 ((rand-nth [s/low s/short-plate])
                   (deg->freq 100 0 (rand-nth [11 16 12 13 17 19]))
                   :atk (rand 0.3)
                   :dcy (rrand 2 8)
                   :amp (rand 0.7)
                   :mod-freq (rrand 6000 10000))
                nil))))

(def dorico-1v3*
  "dorico 1 \"2)4 of 4)7 7.15-1.3.9.19\" with added sharp 1st degree"
  [{:set #{1 15 3 19}
    :ratio 1/63
    :bounded-ratio 64/63
    :bounding-period 2
    :degree 1
    :cents 27.264091800100516}
   {:set #{7 1 15 9}
    :ratio 1/57
    :bounded-ratio 64/57
    :bounding-period 2
    :cents 200.5319830023106}
   {:set #{7 1 15 19}
    :ratio 1/27
    :bounded-ratio 32/27
    :bounding-period 2
    :cents 294.1349974038373}
   {:set #{7 15 19 9}
    :ratio 1/3
    :bounded-ratio 4/3
    :bounding-period 2
    :cents 498.04499913461217}
   {:set #{7 1 15 3}
    :ratio 1/171
    :bounded-ratio 256/171
    :bounding-period 2
    :cents 698.5769821369228}
   {:set #{7 15 3 9}
    :ratio 1/19
    :bounded-ratio 32/19
    :bounding-period 2
    :cents 902.4869838676968}
   {:set [7 15 3 19]
    :ratio 1/9
    :bounded-ratio 16/9
    :bounding-period 2
    :cents 996.0899982692252}])

(def dorico-1v3
  "dorico 1 \"2)4 of 4)7 7.15-1.3.9.19\" with added flat 1st degree"
  [{:set #{7 1 15 9}
    :ratio 1/57
    :bounded-ratio 64/57
    :bounding-period 2
    :cents 200.5319830023106}
   {:set #{7 1 15 19}
    :ratio 1/27
    :bounded-ratio 32/27
    :bounding-period 2
    :cents 294.1349974038373}
   {:set #{7 15 19 9}
    :ratio 1/3
    :bounded-ratio 4/3
    :bounding-period 2
    :cents 498.04499913461217}
   {:set #{7 1 15 3}
    :ratio 1/171
    :bounded-ratio 256/171
    :bounding-period 2
    :cents 698.5769821369228}
   {:set #{7 15 3 9}
    :ratio 1/19
    :bounded-ratio 32/19
    :bounding-period 2
    :cents 902.4869838676968}
   {:set [7 15 3 19]
    :ratio 1/9
    :bounded-ratio 16/9
    :bounding-period 2
    :cents 996.0899982692252}
   {:set #{7 15 21 3}
    :ratio 7/57
    :bounded-ratio 112/57
    :bounding-period 2
    :degree 34
    :cents 1169.3578894714346}])

(def dorico-2v1
  "2)4 of 4)7 7.21-1.3.9.19 with missing 1st. 1st degree is sharp"
  [{:set #{7 1 19 9}
    :ratio 1
    :bounded-ratio 1
    :bounding-period 2
    :degree 0
    :cents 0.0}
   {:set #{7 1 21 9}
    :ratio 21/19
    :bounded-ratio 21/19
    :bounding-period 2
    :cents 173.2678912022099}
   {:set #{7 1 21 19}
    :ratio 7/3
    :bounded-ratio 7/6
    :bounding-period 2
    :cents 266.8709056037379}
   {:set #{7 21 19 9}
    :ratio 21
    :bounded-ratio 21/16
    :bounding-period 2
    :cents 470.7809073345124}
   {:set #{7 1 21 3}
    :ratio 7/19
    :bounded-ratio 28/19
    :bounding-period 2
    :cents 671.3128903368225}
   {:set #{7 21 3 9}
    :ratio 63/19
    :bounded-ratio 63/38
    :bounding-period 2
    :cents 875.222892067597}
   {:set #{7 21 3 19}
    :ratio 7
    :bounded-ratio 7/4
    :bounding-period 2
    :cents 968.8259064691249}])

(def dorico-2v2
  "2)4 of 4)7 7.21-1.3.9.19 with missing 1st.
  Sames a 2v1 but without 1st degree for practical purposes"
  [{:set #{7 1 21 9}
    :ratio 21/19
    :bounded-ratio 21/19
    :bounding-period 2
    :cents 173.2678912022099}
   {:set #{7 1 21 19}
    :ratio 7/3
    :bounded-ratio 7/6
    :bounding-period 2
    :cents 266.8709056037379}
   {:set #{7 21 19 9}
    :ratio 21
    :bounded-ratio 21/16
    :bounding-period 2
    :cents 470.7809073345124}
   {:set #{7 1 21 3}
    :ratio 7/19
    :bounded-ratio 28/19
    :bounding-period 2
    :cents 671.3128903368225}
   {:set #{7 21 3 9}
    :ratio 63/19
    :bounded-ratio 63/38
    :bounding-period 2
    :cents 875.222892067597}
   {:set #{7 21 3 19}
    :ratio 7
    :bounded-ratio 7/4
    :bounding-period 2
    :cents 968.8259064691249}])

(def dorico-3v1
  "2)4 of 4)7 1.15-3.7.19.2 (missing 2nd degree).
  2nd degree close to tempered"
  [{:set #{7 1 15 3}
    :ratio 1
    :bounded-ratio 1
    :bounding-period 2
    :cents 0.0}
   {:set #{1 15 21 9}
    :ratio 9
    :bounded-ratio 9/8
    :bounding-period 2
    :degree 6
    :cents 203.91000173077484}
   {:set #{1 15 21 19}
    :ratio 19
    :bounded-ratio 19/16
    :bounding-period 2
    :cents 297.5130161323026}
   {:set #{1 15 3 19}
    :ratio 19/7
    :bounded-ratio 19/14
    :bounding-period 2
    :cents 528.6871096631775}
   {:set #{1 15 21 3}
    :ratio 3
    :bounded-ratio 3/2
    :bounding-period 2
    :cents 701.9550008653874}
   {:set #{7 1 15 19}
    :ratio 19/3
    :bounded-ratio 19/12
    :bounding-period 2
    :cents 795.5580152669148}
   {:set #{7 1 15 21}
    :ratio 7
    :bounded-ratio 7/4
    :bounding-period 2
    :cents 968.8259064691249}])

(def dorico-3v1
  "2)4 of 4)7 1.15-3.7.19.2 (missing 2nd degree).
  2nd degree flat"
  [{:set #{7 1 15 3}
    :ratio 1
    :bounded-ratio 1
    :bounding-period 2
    :cents 0.0}
   {:set #{7 1 21 19}
    :ratio 133/15
    :bounded-ratio 133/120
    :bounding-period 2
    :degree 5
    :cents 178.0702078712047}
   {:set #{1 15 21 19}
    :ratio 19
    :bounded-ratio 19/16
    :bounding-period 2
    :cents 297.5130161323026}
   {:set #{1 15 3 19}
    :ratio 19/7
    :bounded-ratio 19/14
    :bounding-period 2
    :cents 528.6871096631775}
   {:set #{1 15 21 3}
    :ratio 3
    :bounded-ratio 3/2
    :bounding-period 2
    :cents 701.9550008653874}
   {:set #{7 1 15 19}
    :ratio 19/3
    :bounded-ratio 19/12
    :bounding-period 2
    :cents 795.5580152669148}
   {:set #{7 1 15 21}
    :ratio 7
    :bounded-ratio 7/4
    :bounding-period 2
    :cents 968.8259064691249}])
