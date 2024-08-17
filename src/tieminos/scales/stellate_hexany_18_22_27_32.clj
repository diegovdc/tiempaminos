(ns tieminos.scales.stellate-hexany-18-22-27-32
  "From page 14 of https://www.anaphoria.com/HexanyStellatesExpansions.pdf"
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [erv.utils.core :refer [pick-pattern]]
   [erv.utils.ratios :refer [ratios->scale]]
   [erv.utils.scale :refer [dedupe-scale]]
   [overtone.core :as o]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [bh]]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [time-time.dynacan.players.gen-poly :as gp]))

(def stellate-hexany-18-22-27-32+24
  (let [stell (dedupe-scale (sort-by :bounded-ratio
                                     (concat
                                      (:scale (cps/make 2 [18 22 27 32]))

                                      (ratios->scale 2
                                                     [;; squares
                                                      (* 18 18)
                                                      (* 22 22)
                                                      (* 27 27)
                                                      (* 32 32)
                                                       ;; projections by 24
                                                      (* 24 18)
                                                      (* 24 22)
                                                      (* 24 27)
                                                      (* 24 32)
                                                       ;; subharmonic combinations
                                                      (/ (* 22 27 32) ;; in subharm 1
                                                         18)
                                                      (/ (* 22 27 32) ;; in subharm 1
                                                         24)
                                                      (/ (* 18 27 32) ;; in subharm 2
                                                         22)
                                                      (/ (* 18 27 32) ;; in subharm 2
                                                         24)
                                                      (/ (* 18 22 27) ;; in subharm 3
                                                         32)
                                                      (/ (* 18 22 27) ;; in subharm 3
                                                         24)
                                                      (/ (* 18 22 32) ;; in subharm 4
                                                         24)
                                                      (/ (* 18 22 32) ;; in subharm 4
                                                         27)]))))]

    {:meta {:scale :stellate-cps
            :period 2
            :size (count stell)
            :scl/name "wilson_stellate-hexany_18-22-27-32+24"
            :scl/description "18, 22, 27, 32 Stellate Hexany with 24 added to Stellae. By Erv Wilson. Taken from page 14 (at the time of consultation) of https://www.anaphoria.com/HexanyStellatesExpansions.pdf. Interesting petatonics in the PDF."}
     :scale stell}))

(comment
  (user/spit-scl stellate-hexany-18-22-27-32+24))

(comment

  (defn harmonic-pattern [harmonic-transps]
    (map (fn [h] (partial * h))
         harmonic-transps))
  (defn make-freqs [scale-degrees harmonic-tranps]
    (let [ratios (map :bounded-ratio (:scale stellate-hexany-18-22-27-32+24))]
      (map (fn [r f] (f r))
           (pick-pattern ratios scale-degrees)
           (harmonic-pattern harmonic-tranps))))

  (do (def f1 (make-freqs
                (sort (map (fn [_] (rand-int 15)) (range 10)))
                (sort (map (fn [_] (rand-nth [1 2 4 8 16])) (range 10)))))
      (o/defsynth t1
        [freq 100
         a 0.01
         r 0.1
         amp 0.2
         out 0]
        (o/out out (o/mix (map-indexed
                            (fn [i tfreq]
                              (-> (o/sin-osc (* freq tfreq)
                                             (lfo-kr 3 -1 1)
                                             (+ 1 (min 0.99 (* i -0.1))))
                                  (o/moog-ladder 4000 0.4)
                                  (o/pan2 (lfo-kr 1 -1 1))
                                  (o/free-verb)
                                  (* amp (o/env-gen (o/env-perc a r) :action o/FREE))))
                            f1)))))

  (do (def f2 (make-freqs
                (sort (map (fn [_] (rand-int 15)) (range 10)))
                (sort (map (fn [_] (rand-nth [1 2 4 8 16])) (range 10)))))

      (o/defsynth t2
        [freq 100
         a 0.01
         r 0.1
         amp 0.2
         out 0]
        (o/out out (o/mix (map-indexed
                            (fn [i tfreq]
                              (-> (o/sin-osc (* freq tfreq)
                                             (lfo-kr 0.1 -1 1)
                                             (+ 1 (min 0.99 (* i -0.1))))
                                  (o/moog-ladder 4000 0.4)
                                  (o/pan2 (lfo-kr 0.1 -1 1))
                                  (o/free-verb)
                                  (* amp (o/env-gen (o/env-perc a r) :action o/FREE))))
                            (make-freqs [0 6 9]
                                        [1 2 1]))))))
  (do (def f3 (let [range* 4]
                (make-freqs
                  (sort (map (fn [_] (rand-int 15)) (range range*)))
                  (sort (map (fn [_] (weighted {1 2
                                                2 2
                                                4 4
                                                8 3})) (range range*))))))
      (o/defsynth t3
        [freq 100
         a 0.01
         r 0.1
         amp 0.2
         out 0]
        (o/out out (o/mix (map-indexed
                            (fn [i tfreq]
                              (-> (o/sin-osc (* freq tfreq)
                                             0
                                             (+ 1 (min 0.99 (* i -0.1))))
                                  (o/moog-ladder 4000 0.4)
                                  (o/pan2 0)
                                  (o/free-verb)
                                  (* amp (o/env-gen (o/env-perc a r) :action o/FREE))))
                            f3)))))

  (gp/stop)
  (o/stop)
  (let [scale (:scale stellate-hexany-18-22-27-32+24)]
    (gp/ref-rain
      :id ::stella
      :durs [2 2 2 3 2 2 3 2 3]
      :tempo 80
      :on-event (gp/on-event
                  #_((at-i [t1 t1 t3 t2])
                     :freq (scale/deg->freq scale
                                            100
                                            (at-i [-2 1])
                                            :period 0)
                     :a 4
                     :r 4
                     :amp 1
                     :out (bh 2))))
    ;; 0 2 6 9 11
    ;; 1 3 6 10 11
    (gp/ref-rain
      :id ::stella1
      :durs [2]
      :tempo 70
      :ratio 1/8
      :on-event (gp/on-event
                  #_(synths/low
                      :freq (scale/deg->freq scale
                                             100
                                             (at-i [(at-i [1 1 -6])
                                                    (at-i [3 3 -6 1 6 11])])
                                             :period 1)
                      :a 0.01
                      :r 0.8
                      :amp (at-i [1 0.7 0.7 1 0.8])
                      :out (bh 2))
  
                  #_(when ((conj #{1 2} (at-i [1 3 4]))
                           (mod i (at-i [4 6 4 3])))
                      ((weighted {synths/low 10
                                  t2 4})
                       :freq (scale/deg->freq scale
                                              100
                                              (at-i [(at-i [10 11 10 16])
                                                     6
                                                     (at-i [-6 24 3 11])])
                                              :period 1)
                       :a 0.01
                       :r (at-i [0.8 2 0.2])
                       :amp (at-i [1 0.7 0.5])
                       :out (bh 2)))

                  #_(when ((conj #{1 2 5} (at-i [1 3]))
                           (mod i (at-i [7])))
                      ((weighted {synths/low 10
                                  t2 4})
                       :freq (scale/deg->freq scale
                                              100
                                              (at-i [(at-i [10 11 10 16])
                                                     6
                                                     (at-i [6 24 3 11])])
                                              :period 2)
                       :a 0.01
                       :r (at-i [0.8 2 0.2])
                       :rel (at-i [0.8 2 0.2])
                       :amp (at-i [1 0.7 0.5])
                       :out (bh 2)))

                  #_(when (#{0 1} (mod i 4))
                      ((at-i [t2])
                       :freq (scale/deg->freq scale
                                              100
                                              (at-i [3 6 11 1])
                                              :period 1)
                       :a 0.01
                       :r 0.5
                       :amp 1
                       :out (bh 2)))
                  #_(when (#{0 1} (mod i 7))
                      ((at-i [t3])
                       :freq (scale/deg->freq scale
                                              100
                                              (at-i [3 6 11 1])
                                              :period 2)
                       :a 0.01
                       :r 0.5
                       :amp 1
                       :out (bh 2)))
                  #_(when (#{0 1 2 3 4 5} (mod i 17))
                      ((at-i [t2])
                       :freq (scale/deg->freq scale
                                              100
                                              5
                                              :period 2)
                       :a 0.01
                       :r 0.5
                       :amp 1

                       :out (bh 2))))))
  (let [scale (:scale stellate-hexany-18-22-27-32+24)]
    (ndef/ndef
        ::stella

        (o/mix
          (map (fn [deg amp]
                 (-> (o/sin-osc (scale/deg->freq scale 200 deg)
                                0
 )
                     (o/pan2 (lfo-kr (rand 0.3) -1 1))
                     #_(#(+ % (o/moog-ff % (rand-nth [5000 15000]) 4 [0.2 1 2 0.1])))
                     #_(#(+ % (o/moog-ladder % [8000 15000])))
                     (* 0)
                     (o/free-verb 1 2)
                     (* amp (lfo-kr 1.1 0.2 1))))
               (mapcat (fn [d] [(+ d 16)
                             (+ d )
                                ])
                    [14])
               (concat [1 0.7 0.7 0.3 (rand)]
                       [1 0.7 0.7 0.3 (rand)]
                       [1 0.7 0.7 0.3 (rand)])))
        {:out (bh 0)
         :fade-time 30})

    (ndef/ndef
        ::stella2
        (o/mix
          (map (fn [deg amp]
                 (-> (o/sin-osc (scale/deg->freq scale 200 deg)
                                [(o/sin-osc:kr (scale/deg->freq scale 2500 (+ 2 deg)) :mul 0.1)
                                 (o/sin-osc:kr (scale/deg->freq scale 5000 (+ 6 deg)) :mul 0.1)] )
                     (o/pan2 (lfo-kr (rand 0.3) -1 1))
                     (#(+ % (o/moog-ff % (rand-nth [8000 15000]) 2 (rand-nth [0.2 1 2]))))
                     (#(+ % (o/moog-ladder % (rand-nth [8000 15000]))))
                     (* 0.)
                     (o/free-verb 1 2)
                     (* amp (lfo-kr 0.3 0.5 1))))
               (mapcat (fn [d] [(+ d 16)
                                (+ d )
                                (+ d 7 )])
                       [ 10 ])
               (concat [0.7 0.3 0.3 (rand)]
                       [0.2 0.3 0.3 (rand)])))
        {:out (bh 0)
         :fade-time 10})))
