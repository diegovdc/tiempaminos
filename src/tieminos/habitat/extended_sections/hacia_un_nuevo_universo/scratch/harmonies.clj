(ns tieminos.habitat.extended-sections.hacia-un-nuevo-universo.scratch.harmonies
  (:require
   [erv.utils.core :refer [period-reduce pow]]
   [tieminos.lattice.v1.lattice :refer [draw-lattice]]))

(comment
  ;; current transpositions used
  (draw-lattice
    {:ratios (->> (for [a [8/7 11/8 7/4]
                        b [1/4 11/32 4/11 13/32]]
                    (period-reduce (* a b #_c)))
                  sort)})

  ;;  another possibility
  (draw-lattice
    {:ratios (->> (for [a [8/7 33/28 7/4]
                        b [1/4  4/11 4/7]
                        ;; c [1 3 5 7 9]
                        ]
                    (period-reduce (* a b #_c)))
                  sort)})

  ;;  another one
  (draw-lattice
    {:ratios (->> (for [a [9/7 4/7 7/4]
                        b [1/4 4/7 7/10]
                        c [1 #_#_#_#_3 5 7 9]]
                    (period-reduce (* a b c)))
                  sort)})

  (draw-lattice
    {:ratios (->> (for [a [5/4 5/3 25/11]
                        b [1/4 #_4/7 7/10 13/8]
                        c [1 #_#_#_#_3 5 7 9]]
                    (period-reduce (* a b c)))
                  sort)})
  (draw-lattice
    {:ratios (->> (for [a [5/4 5/3 25/11 8/13]
                        b [1/4 #_4/7 7/10]
                        c [1 #_#_#_#_3 5 7 9]]
                    (period-reduce (* a b c)))
                  sort)})

  ;; la golondrina
  (draw-lattice
    {:ratios (->> (for [a [5/4 5/3 25/11]
                        b [1/4 7/10 13/8]]
                    (period-reduce (* a b)))
                  sort)})

  (defn golondrina
    [{:keys [a b c d e transp]
      :or {transp 1}}]
    (let [as [(period-reduce 2 b) (/ b a) (period-reduce 4 (/ (pow b 2) d))]
          bs [1/4 (/ c (* 2 b)) (period-reduce 2 e)]]

      {:a as
       :b bs
       :c [transp]
       :chord (->> (for [a as
                         b bs]
                     (period-reduce (* a b transp)))
                   sort)}))


  (draw-lattice
    {:ratios (->> (golondrina
                    {:a 13 :b 7 :c 5 :d 11 :e 3 :transp 1})
                  :chord
                  sort)})

  (draw-lattice
    {:ratios (->> (golondrina
                    {:a 13 :b 5 :c 7 :d 11 :e 3 :transp 1})
                  :chord
                  sort)})

  (draw-lattice
    {:ratios (->> (for [a [13/16 13/24 13/11 1/22]
                        b [1/4 #_4/7 11/26 33/64]
                        c [1 #_#_#_#_3 5 7 9]]
                    (period-reduce (* a b c)))
                  sort)}))
