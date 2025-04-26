(ns tieminos.scales.17o7.some-constant-structures1
  (:require
   [erv.constant-structures.brute-force :as cs.brute-force]
   [erv.utils.ratios :refer [ratios->scale]]
   [tieminos.pitch-wheel.v1.pitch-wheel :as pitch-wheel.v1]
   [tieminos.scales.17o7.original :refer [original-tritavic-scale-ratios]]))

;; NOTE see below for construction details of these scales

(def scales
  {:11t-cs-v1 {:meta {:scl/name "11t-cs-of-17o7_v1.scl"
                      :scl/description "11 tone constant structure extension for the original 17/7 scale."}
               :scale [{:ratio 1, :bounded-ratio 1, :bounding-period 3}
                       {:bounded-ratio 9/8, :color [255 0 0]}
                       {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                       {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                       {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                       {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                       {:bounded-ratio 17/9, :color [255 0 0]}
                       {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                       {:bounded-ratio 9/4, :color [255 0 0]}
                       {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                       {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}]}
   :11t-cs-v2 {:meta {:scl/name "11t-cs-of-17o7_v2.scl"
                      :scl/description "11 tone constant structure extension for the original 17/7 scale."}
               :scale [{:ratio 1, :bounded-ratio 1, :bounding-period 3}
                       {:bounded-ratio 8/7, :color [255 0 0]}
                       {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                       {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                       {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                       {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                       {:bounded-ratio 51/28, :color [255 0 0]}
                       {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                       {:bounded-ratio 867/392, :color [255 0 0]}
                       {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                       {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}]}
   :15t-cs {:meta {:scl/name "15t-cs-of-17o7.scl"
                   :scl/description "15 tone constant structure extension for the original 17/7 scale."}
            :scale [{:ratio 1, :bounded-ratio 1, :bounding-period 3}
                    {:bounded-ratio 9/8}
                    {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                    {:bounded-ratio 17/13, :color [255 0 0]}
                    {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                    {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                    {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                    {:bounded-ratio 17/10, :color [255 0 0]}
                    {:bounded-ratio 17/9}
                    {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                    {:bounded-ratio 9/4}
                    {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                    {:bounded-ratio 34/13, :color [255 0 0]}
                    {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}
                    {:bounded-ratio 85/26, :color [255 0 0]}]}
   :17t-cs {:meta {:scl/name "17t-cs-v1-of-17o7.scl"
                   :scl/description "17 tone constant structure extension for the original 17/7 scale."}
            :scale [{:bounded-ratio 11/12, :color [255 0 255]}
                    {:bounded-ratio 25/26, :color [255 0 255]}
                    {:ratio 1, :bounded-ratio 1, :bounding-period 3}
                    {:bounded-ratio 9/8}
                    {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                    {:bounded-ratio 17/13}
                    {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                    {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                    {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                    {:bounded-ratio 17/10}
                    {:bounded-ratio 17/9}
                    {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                    {:bounded-ratio 9/4}
                    {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                    {:bounded-ratio 34/13}
                    {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}
                    {:bounded-ratio 85/26}]}})
(comment
  ;; original+ 3 tones CS (11 tone)
  (def brute-force-results
    (cs.brute-force/find-1
     (ratios->scale 3 original-tritavic-scale-ratios)
     3 100
     4 (fn [n] (assoc n  :color [255 0 0]))))

  (def pw-atom
    (pitch-wheel.v1/init! {:scale (ratios->scale 3 original-tritavic-scale-ratios)
                           :period 3}))

  (-> brute-force-results
      :constant-structures
      (nth 0)
      :added-notes)

  ;; v1
  (def *11t-v1
    (:scale+added-notes
     (pitch-wheel.v1/update! pw-atom
                             {:analyze-cs? true
                              :scale (ratios->scale 3 original-tritavic-scale-ratios)
                              :added-notes
                              (map (fn [r] {:bounded-ratio r :color [255 0 0]})
                                   [9/8 17/9 9/4])})))
  ;; v2
  (def *11t-v2
    (:scale+added-notes
     (pitch-wheel.v1/update! pw-atom
                             {:analyze-cs? true
                              :scale (ratios->scale 3 original-tritavic-scale-ratios)
                              :added-notes
                              (map (fn [r] {:bounded-ratio r :color [255 0 0]})
                                   [8/7
                                    (* 3/2 8/7 17/16)
                                    (* 3/2 8/7 17/16 17/14)])}))))
(comment
  ;; 11+4 = 15 tone
  ;; NOTE ensure the code above has been compiled
  (def *15t
    (:scale+added-notes
     (pitch-wheel.v1/update! pw-atom
                             {:analyze-cs? true
                              :scale (map #(dissoc % :color) *11t-v1)
                              :added-notes (map (fn [r] {:bounded-ratio r :color [255 0 0]})

                                                [17/13 ;; 1.2570133745218284
                                                 17/10 ;; 1.6414832176209966
                                                 (* 17/13 2) ;; 2.4966610978032238
                                                 (* 17/13 2 5/4) ;; 2.6944671537313805
                                                 ])})))

  ;; 11+4+2 17 tone
  (def *17t (:scale+added-notes
             (pitch-wheel.v1/update! pw-atom
                                     {:analyze-cs? true
                                      :scale (map #(dissoc % :color) *15t)
                                      :added-notes (map (fn [r] {:bounded-ratio r :color [255 0 255]})
                                                        ;; These can be modifed a lot, but not an all positions
                                                        [(#(/ % (inc %)) 11)
                                                         (#(/ % (inc %)) 25)])}))))
