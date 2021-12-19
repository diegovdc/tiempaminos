(ns tieminos.harmonies.hex-two
  (:require [clojure.string :as str]
            [erv-fib-synth.midi :refer [note-on]]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [erv.utils.conversions :as conv]
            [overtone.core :as o]
            [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
            [time-time.standard :refer [wrap-at]]
            [clojure.set :as set]))



(do
  (defn period [seconds durs]
    (let [ratio (/ seconds (apply + durs))]
      (mapv #(* ratio %) durs)))
  (period 1 [1 2])
  (defn periods [seconds & durs]
    (mapcat (partial period seconds) durs))
  (periods 1 [1] [1]))


(do
  (o/defsynth h1 [freq 400
                  amp 0.5]
    (o/out 0
           (-> (+ (o/sin-osc freq)
                  (* 0.5 (o/sin-osc (* 3 freq)))
                  (* (o/lf-tri freq) (o/saw freq))
                  #_(* 0.7 (o/pulse freq) (o/saw freq) (o/env-gen (o/env-perc 0.7 0.01))))
               (o/lpf 7000)
               (o/pan2)
               (* 1/10 amp (o/env-gen (o/env-perc 0.1 1) :action o/FREE)))))
  (h1))

(defn degs->freq [scale degs]
  (map #(scale/deg->freq scale 200 %) degs))

(do
  (defn +gen-vars [scale]
    (let [vars [:a :b :c :d :e :f :g :h :i]
          gen-indexes (->> scale
                           (map :set)
                           (apply (comp sort set concat))
                           (map-indexed (fn [i gen] [gen i]))
                           (into {}))]
      (map (fn [data]
             (assoc data :set-vars (set (map (comp vars gen-indexes) (:set data)))))
           scale)))
  (+gen-vars
   (->> [1 3 5 7]
        (cps/->cps 3) ;; 2 y 3 funcionan muy bien
        cps/set->maps
        (cps/bound-ratio 2)
        (cps/maps->data :bounded-ratio)
        :scale)))

(defn filter-scale
  "Get a subscale that only has degrees related to the `generators`.
  `generators` must be a set."
  [scale generators]
  (filter #(-> % :set-vars (set/intersection generators) not-empty)
          scale))

(defn gen-chord [scale [gen-vars degs]]
  (let [scale** (->> scale +gen-vars (#(filter-scale % gen-vars)))]
    (map (partial scale/deg->freq scale** 200) degs)))

(let [notes (gen-chord
             (->> [1 3 5 7]
                  (cps/->cps 2) ;; 2 y 3 funcionan muy bien
                  cps/set->maps
                  (cps/bound-ratio 2)
                  (cps/maps->data :bounded-ratio)
                  :scale)
             [#{:a :b :c} [0 1 2]])]
  (doseq [n notes]
    (println (conv/cps->name* n))
    (h1 n)))
(defn chord
  ([gens scale] (chord gens false scale))
  ([gens sub-harmonic? scale]
   (filter #(-> % :set (set/intersection gens)
                ((if sub-harmonic? empty? not-empty))
                ) scale))
  )

(let [scale**
      (->> [1 3 5 7]
           (cps/->cps 2) ;; 2 y 3 funcionan muy bien
           cps/set->maps
           (cps/bound-ratio 2)
           (cps/maps->data :bounded-ratio)
           :scale
           (chord #{3} 1))]
  (clojure.pprint/pprint scale**)
  (doseq [n [0 1 2]]
    (h1 (scale/deg->freq scale** 200 n))))

(->> (map (partial gen-chord
                   (->> [1 3 5 7]
                        (cps/->cps 3) ;; 2 y 3 funcionan muy bien
                        cps/set->maps
                        (cps/bound-ratio 2)
                        (cps/maps->data :bounded-ratio)
                        :scale))
          [[#{:a :c} [0 1 2 10]]
           [#{:a :b} [0 1 2 4 -9]]
           [#{:b} [0 1 2 4 -7]]
           [#{:b :c} [0 1 2 3 11 4]]
           [#{:c} [0 1 2 4 6 9]]
           [#{:b} [3 1 2 -1 8]]
           [#{:b :c} [3 1 2 6 7 11 -10]]
           [#{:c} [4 3 2 0 6]]
           [#{:b} [0 1 2 3 6 10]]
           [#{:d} [0 1 2 3 7]]
           [#{:a :d} [0 1 2 3]]])
     (map sort)
     (map #(map conv/cps->name %)))

(->> [1 3 5 7]
     (cps/->cps 2) ;; 2 y 3 funcionan muy bien
     cps/set->maps
     (cps/bound-ratio 2)
     (cps/maps->data :bounded-ratio)
     :scale
     (map :set)
     cps/cps-intervals)



(comment
  (gp/stop)

  (let [scale* (->> [23 19 51 17]
                    #_[1 19 51 17]
                    #_[1 19 51 9]
                    #_[1 19 3 9]
                    #_[1 3 7 9]
                    #_[1 3 23 9]
                    (cps/->cps 2) ;; 2 y 3 funcionan muy bien
                    cps/set->maps
                    (cps/bound-ratio 2)
                    (cps/maps->data :bounded-ratio)
                    :scale)
        harmonies (map (partial degs->freq scale*)
                       [[0 4 5 9 11]
                        [1 4 5]
                        [1 2 3]
                        [0 3 5]])
        harmonies2 (mapv (partial gen-chord scale*)
                         [[#{:a :c} [0 1 2 10]]
                          [#{:a :b} [0 1 2 4 -9]]
                          [#{:b} [0 1 2 4 -7]]
                          [#{:b :c} [0 1 2 3 11 4]]
                          [#{:c} [0 1 2 4 6 9]]
                          [#{:b} [3 1 2 -1 8]]
                          [#{:b :c} [3 1 2 6 7 11 -10]]
                          [#{:c} [4 3 2 0 6]]
                          [#{:b} [0 1 2 3 6 10]]
                          [#{:d} [0 1 2 3 7]]
                          [#{:a :d} [0 1 2 3]]])
        #_ #_harmonies2 (mapv (partial gen-chord scale*)
                              [[#{:a} [0]]
                               [#{:a} [-1]]
                               [#{:b} [1]]
                               [#{:a} [2]]
                               [#{:b} [3]]
                               [#{:a} [8]]
                               [#{:b} [9]]
                               [#{:b} [10]]
                               [#{:b} [11]]
                               [#{:c} [10]]
                               [#{:d} [0]]
                               [#{:c} [1]]
                               [#{:c} [2]]
                               [#{:c} [3]]
                               [#{:d} [2]]
                               [#{:d} [3]]
                               [#{:d} [1]]])]

    (ref-rain :id ::b
              :durs (periods 2.7
                             [3 1 2 1 1 1 2 1/2 1/2 1 2]
                             [1 3 2 1 1 1 2 1 2 1]
                             )
              :on-event (on-event
                         (case (wrap-at index [0 1 1])
                           0 (mapv h1 (wrap-at index harmonies))
                           1 (mapv #(h1 % (wrap-at index [0.7 0.9 0.5]))
                                   (wrap-at index harmonies2))
                           nil)))))
