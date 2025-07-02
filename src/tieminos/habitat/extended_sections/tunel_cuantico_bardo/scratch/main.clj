(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.scratch.main
  "Stuff that has been moved out of main. Only WIP stuff, some of it may be useful, most, probably not."
  (:require
   [overtone.core :as o]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.habitat.extended-sections.harmonies.chords
    :refer [meta-slendro1 rate-chord-seq transpose-chord]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.main :refer [algo-2-2-9]]
   [tieminos.habitat.routing :as habitat.route]
   [tieminos.habitat.synths.granular :refer [amanecer*guitar-clouds]]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp]))

(comment
  (algo-2-2-9 {:out-bus in1
               :chord [0 5 8 9]
               :transpositions (shuffle (range 0 60 4))
               :clouds-config {:amp (o/db->amp -24) ;; NOTE interesante cambiar la amplitud
                               }})
  (gp/stop ::clouds-refrain)
  (gp/stop :cuerpo-envolvente)

  (algo-2-2-9 {:out-bus in1
               :chord [10 15 20]
               :transpositions [0 5 0 5]})

  (def lor (lorentz/init-system :x 0.3 :y 0.02 :z 0.012))
  (def chord-seq
    (let [min* -12
          max* 12
          lor-speed 70
          total 1000]
      (map (fn [a b c] [a b c])
           (map
            #(int (lorentz/bound (lor (* lor-speed %)) :x min* max*))
            (range total))
           (map
            #(int (lorentz/bound (lor (* lor-speed %)) :y min* max*))
            (range total))
           (map
            #(int (lorentz/bound (lor (* lor-speed %)) :z min* max*))
            (range total)))))
  (-> chord-seq)
  ;; largos
  (algo-2-2-9 {:out-bus in1
               :clouds-config {:amp (o/db->amp -12)
                               :period nil
                               :durs (fn [{:keys [index] :as config}]
                                       (let [dur (lorentz/bound (lor (* 50 index)) :x 0.1 2)]
                                         (println "durs call=========" dur)
                                         dur)
                                       #_(rand 10))
                               :rates (rate-chord-seq meta-slendro1
                                                      chord-seq)}
               :on-play (fn [{:as config
                              :keys [index]}]
                          (println "adr" (select-keys config [:a :d :r]))
                          (let [min* 1 max* 4]
                            (amanecer*guitar-clouds (assoc config
                                                           :a (lorentz/bound (lor (* 50 index))
                                                                             :x min* max*)
                                                           :d (lorentz/bound (lor (* 50 index))
                                                                             :y min* max*)
                                                           :r (lorentz/bound (lor (* 50 index))
                                                                             :z min* max*)
                                                           :interp (rand-nth [1 2 4])
                                                           :amp (o/db->amp  (rrange -6 0))))))})
  ;; breves
  (algo-2-2-9 {:out-bus in1
               :clouds-config {:amp (o/db->amp -12)
                               :period nil
                               :durs (fn [{:keys [index] :as config}]
                                       (let [dur (lorentz/bound (lor (* 50 index)) :x 0.1 2)]
                                         (println "durs call=========" dur)
                                         dur)
                                       #_(rand 10))
                               :rates (rate-chord-seq meta-slendro1
                                                      chord-seq)}
               :on-play (fn [{:as config
                              :keys [index]}]
                          (println "adr" (select-keys config [:a :d :r]))
                          (let [min* 0.1 max* 2]
                            (amanecer*guitar-clouds (assoc config
                                                           :a (lorentz/bound (lor (* 50 index))
                                                                             :x min* max*)
                                                           :d (lorentz/bound (lor (* 50 index))
                                                                             :y min* max*)
                                                           :r (lorentz/bound (lor (* 50 index))
                                                                             :z min* max*)
                                                           :interp (rand-nth [1 2 4])
                                                           :amp (o/db->amp  (rrange -6 0))))))})
  ;; ataques
  (algo-2-2-9 {:out-bus in1
               :clouds-config {:amp (o/db->amp -12)
                               :period nil
                               :durs (fn [{:keys [index] :as config}]

                                       (rand 10))
                               :rates (rate-chord-seq meta-slendro1
                                                      (reverse chord-seq))}
               :on-play (fn [{:as config
                              :keys [index]}]
                          (println "adr" (select-keys config [:a :d :r]))
                          (amanecer*guitar-clouds (assoc config
                                                         :a (rrange 0.1 0.4)
                                                         #_(rrange 3 5)
                                                         :d 2
                                                         :r 3
                                                         :interp (rand-nth [1 2 4])
                                                         :amp (o/db->amp  (rrange -6  6)))))})

  ;; cluster lento
  (algo-2-2-9 {:on-play (fn [config]
                          (println "==================")
                          (smooth-clouds 200 config))
               :out-bus in1
               :clouds-config {:id :cuerpo-envolvente
                               :amp (o/db->amp -24)
                               :rates (rate-chord-seq meta-slendro1
                                                      (transpose-chord
                                                       [0 7]
                                                       [-24 6]))
                               :period 90
                               :durs [1 3 5]
                               :a-weights {10 1
                                           15 0.3}
                               :d-weights {40 1
                                           30 0.3}
                               :d-level-weights {0.8 5
                                                 0.6 8}}})

  ;; Usar sobretodo en el micro, para dar color a las partes piano/vacias
  (ndef/ndef ::cuerpo
             (* 2 (o/mix [(* 2 (o/pan4 (-> :mic-1
                                           habitat.route/get-input-bus
                                           (o/in 1)
                                           (o/pitch-shift  0.2
                                                           (first (rate-chord-seq meta-slendro1
                                                                                  [[-12 -7 -4 7 8 9 13 14]])))
                                           (o/mix))
                                       (lfo-kr 0.1 -1 1)
                                       (lfo-kr 0.1 -1 1)))
                          (o/pan4 (-> :guitar
                                      habitat.route/get-input-bus
                                      (o/in 1)
                                      (o/pitch-shift  0.2 (first (rate-chord-seq meta-slendro1
                                                                                 [[-12 -7 -4 7 8 9 13 14]])))
                                      (o/mix))
                                  (lfo-kr 0.1 -1 1)
                                  (lfo-kr 0.1 -1 1))]))
             {:out habitat.route/mixed-main-out}))
