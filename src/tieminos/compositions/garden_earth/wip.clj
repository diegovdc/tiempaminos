(ns tieminos.compositions.garden-earth.wip)

;; fl-grain-1

(defn ex1 []
    (let [bufs @rec/test-samples
          subcps "1)4 of 3)6 1.11-3.5.7.9"
          scale (-> eik :subcps (get subcps) :scale (->> (+names base-freq)))]
      (ref-rain
       :id ::ex1
       :tempo 120
       :ratio 1/10
       :durs [5 3 5 3 3]
       :on-event (on-event
                  (let [root (rand-int (count scale))
                        target (- (rand-int 29) 29)
                        pc (-> scale (nth root) :pitch :class)
                        b (get-any-buffer-for-pitch-class pc bufs)
                        rate (interval->ratio scale root target)
                        dur* (rand-nth [0.3 0.2 0.1])]
                    #_(println pc rate)
                    (println (pitch-class->pr-fingering pc)
                             "\n\n")
                    (vec
                     (for [g-dur (subvec (shuffle [1/10 1/8 1/12 1/20 1/2]) 0 2)
                           dur* (subvec (shuffle [0.3 0.2 0.1 1 2]) 0 2)]
                       (granular/dot
                        {:group [:head fx/early-g]
                         :out 8
                         :buf b
                         :dur dur*
                         :trig-rate 10
                         :grain-dur g-dur
                         :rate rate
                         :speed 0.1
                         :amp 10
                         :min-amp 0.8
                         :amp-lfo 0.01
                         :start 0 #_start
                         :end 1 #_ end
                         :mix 1
                         :room 3
                         :damp 0.8
                         :pos-noise-freq 1000
                         :pos-noise-amp 0.2
                         :a dur*
                         :r 0.1}))))))))
;;; long ocean
(defn oceanoises []
    (ref-rain
     :id :oceanic/noises
     :durs [1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 3]
     :tempo 120
     :ratio 2
     :on-event (on-event
                (let [[[pc] b] (-> (into []
                                         (filter (fn [[[_ id]]]
                                                   true
                                                   #_(= :oceanic id))
                                                 @rec/bufs))
                                   rand-nth)

                      dur* (* dur (weighted {1 5 2 5}))
                      rates (map (fn [i]
                                   (interval-from-pitch-class
                                    (subcps "3)4 of 3)6 3.5.7.9")
                                    pc
                                    (utils/wrap-at i [(at-index [1 2 1 2 1 2 5])
                                                      (at-index [1 3 2])
                                                      (at-index [-9 0])])
                                    #_(- (rand-int 29) 15)))
                                 (range 3))
                      trig-rate 100]
                  (doseq [g-dur (take 1 [1/20 1/10 1/3])
                          rate rates]
                    (granular/ocean
                     {:out 8
                      :group [:head early-g]
                      :buf b :a (* 0.1 dur*) :r (* 0.9 dur*)
                      :trig-rate trig-rate :grain-dur g-dur :dur dur*
                      :rate rate :speed (/ 1 dur*) :panl -1 :panr 1 :pan-lfo 2
                      :amp (* 3 (rand-nth [10 5 3]))
                      ;; :amp-lfo 5
                      :mix 1
                      :room 2}))))))
