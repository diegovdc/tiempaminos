(ns tieminos.compositions.garden-earth.misterioso
  (:require
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.base
    :refer
    [algo-note
     eik
     get-cps-pitch-class
     midi-out-1
     on-event
     ref-rain
     stop
     wrap-at]]))

(do
  (defn misterioso [& {:keys [id vel-amp durs moments ratio offsets]
                       :or {id :misterioso
                            ratio 1
                            moments [1]
                            offsets [-6]
                            vel-amp 0.7
                            durs [4 7 4 7 7]}}]
    (timbre/info "Misterioso")
    (ref-rain
     :id id
     :tempo 900
     :ratio ratio
     :durs durs
     :on-event
     (on-event
      (when-let [{:keys [scale note vel durs delay]}
                 (case (wrap-at index moments)
                   1 {:scale (:scale eik)
                      :note (wrap-at index [0 4 3 -1 2 8 11])
                      :vel (wrap-at index [10 30 70])}
                   2 {:scale (:scale eik)
                      :note (wrap-at index [0 4 3 nil 23 nil 20 25 24])
                      :vel (wrap-at index [10 30 70])}
                   2.1 {:scale (:scale eik)
                        :note (wrap-at index [0 4 3 nil 23 nil 20 25 24])
                        :vel (wrap-at index [100 130 120])
                        :durs [300 600]}
                   3 {:scale (:scale eik)
                      :note (wrap-at index [23 nil 20 nil 24 39])
                      :vel (wrap-at index [120])
                      :durs [400 500]}
                   3.1 {:scale (:scale eik)
                        :note (wrap-at index [[24 38] nil nil 39])
                        :vel (wrap-at index [120])
                        :durs [2000 700]}
                   4 {:scale (:scale eik)
                      :note (wrap-at index [-20])
                      :vel (wrap-at index [20])
                      :durs [900 750]}
                   4.1 {:scale (:scale eik)
                        :note (wrap-at index [-10 nil -30])
                        :vel (wrap-at index [20])
                        :durs [900 750]}
                   4.2 {:scale (:scale eik)
                        :note (wrap-at index [-20 -16 nil -21 -17])
                        :vel (wrap-at index [20])
                        :durs [900 750]}
                   5 {:scale (:scale eik)
                      :note (wrap-at index [[31 62] nil [45 34] [37 63] nil [52 33]])
                      :vel (wrap-at index [3 5 5 10 3])
                      :delay (wrap-at index [300 20 500 0])
                      :durs [[1000 550]
                             [200 1050]
                             [2000 100]
                             [700 950]]}
                   5.1 {:scale (:scale eik)
                        :note (wrap-at index [33 nil [22 24] nil 20 [33 35]])
                        :vel (wrap-at index [3 5 5 10 3])
                        :delay (wrap-at index [300 200 500])
                        :durs [3000]}
                   {})]

        (when note
          #_(timbre/info
             "pitch"
             (conv/cps->name* (scale/deg->freq
                               scale
                               440
                               (+ note -6))))

          (algo-note :sink midi-out-1
                     :dur (wrap-at index (or durs [7000 10000])) ;; millisecons
                     :scale scale
                     :delay (or delay 0)
                     :base-freq 440
                     :get-pitch-class get-cps-pitch-class
                     :deg-offset (wrap-at index offsets)
                     :midi-note note
                     :vel (min 127 (int (* vel-amp vel))))))))))

(def sections
  [[30 #(misterioso  :vel-amp 0.7 :durs [4 7 4 7 7]
                     :moments [1])]
   [20 #(misterioso  :vel-amp 0.7 :durs [4 7 4 7 7]
                     :moments [1 2])]
   [10 #(misterioso  :vel-amp 0.7 :durs [4 7 4 7 7]
                     :moments [2])]
   [20 #(misterioso  :vel-amp 0.7 :durs [4 7 4 7 7]
                     :ratio 3/2
                     :moments [2 2.1])]
   [20 #(misterioso  :vel-amp 0.7 :durs [4 7 4 7 7]
                     :moments [2 2.1 nil])]
   [10 #(misterioso  :vel-amp 0.7 :durs [4 7 4 7 7]
                     :ratio 2
                     :moments [1 nil 2.1])]
   [10 #(misterioso  :vel-amp 0.7 :durs [4 7 4 7 7]
                     :ratio 2
                     :moments [1 nil 2.1 4.1])]
   [8 #(misterioso  :vel-amp 0.7 :durs [4 7 4 7 7]
                    :moments [1 nil 2.1 4.2])]
   [8 #(misterioso  :vel-amp 0.7 :durs [4 7 4 7 7]
                    :moments [1 nil 2.1 4.2 nil 1 3 3.1])]
   [8 #(misterioso  :vel-amp 0.7 :durs [4 7 4 7 7]
                    :moments [3 2.1 nil 2.1 nil 2.1 nil 3 nil nil])]
   [8 #(misterioso  :vel-amp 0.7 :durs [4 7 4 7 7]
                    :moments [2 2.1 nil 2.1 3.1 2.1 nil 2 nil 3])]
   [8 #(misterioso  :vel-amp 0.7 :durs [4 11 7 7]
                    :moments [2 2.1 4 nil 2.1 3.1 2.1 nil 2 nil 3])]
   [20 #(misterioso  :vel-amp 0.7 :durs [4 11 7 7]
                     :moments [2 2.1 4 nil 2.1 4.1 4.1 3.1 2.1 nil 2 nil 3])]

   [10 #(misterioso  :vel-amp 0.7 :durs [4 11 7 7]
                     :moments [2 2.1 4 nil 2.1 4.1 4.1 3.1 2.1 nil 2 nil 3])]

   [10 #(misterioso  :vel-amp 0.7 :durs [4 11 14]
                     :moments [2 2.1 4 nil 2.1 4.1 4.1 3.1 2.1 nil 2 nil 3]
                     :offsets [-5])]

   [10 #(misterioso  :vel-amp 0.7 :durs [4 11 7 7]
                     :moments [2 2.1 4 nil 2.1 4.1 4.1 3.1 2.1 nil 2 nil 3])]

   [10 #(misterioso  :vel-amp 0.7 :durs [4 11 14]
                     :moments [2 2.1 4 nil 2.1 4.1 4.1 3.1 2.1 nil 2 nil 3]
                     :offsets [-5])]

   [20 #(misterioso  :vel-amp 0.7 :durs [4 11 7 7]
                     :moments [2 2.1 nil 2.1 4.1 4.1 3.1 2.1 nil 2 nil 3]
                     :offsets [-4])]

   [30 #(misterioso  :vel-amp 0.7 :durs [4 11 7 7]
                     :moments [2 2.1 4 nil 2.1 4.1 3.1 2.1 nil 2 nil 3]
                     :offsets [-3])]
   [30 #(misterioso  :vel-amp 0.7 :durs [4 11 7 7]
                     :moments [2 2.1 4 nil 2.1 4.1 4.2 3.1 2.1 nil 2 nil 3]
                     :offsets [-1])]
   [20 #(misterioso  :vel-amp 0.7 :durs [4 11 7 7]
                     :ratio 3
                     :moments [1 nil nil]
                     :offsets [-1])]
   [20 #(misterioso  :vel-amp 0.7 :durs [4 11 7 7]
                     :ratio 3
                     :moments [1 nil nil]
                     :offsets [-6])]
   [10 #(misterioso  :vel-amp 0.7 :durs [4 11 14 4 11]
                     :ratio 3
                     :moments [1 nil nil]
                     :offsets [-6 -6 -6 -26 14])]
   [20 #(misterioso  :vel-amp 0.7 :durs [4 11 14 4 11]
                     :ratio 3
                     :moments [1 nil nil]
                     :offsets [-6 -6 -6 -1 -26 14 19])]])

#_(stop)
