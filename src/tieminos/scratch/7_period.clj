(ns tieminos.scratch.7-period
  (:require
   [erv.constant-structures.graphics :as csg]
   [erv.scale.scl :as scl]
   [erv.utils.conversions :as convo]
   [erv.utils.core :refer [period-reduce prime-factors]]
   [erv.utils.ratios :refer [ratios->harmonic-series ratios->scale]]
   [erv.utils.scale :refer [scale->stacked-subscale]]
   [tieminos.lattice.v1.lattice :refer [draw-lattice]]))

(def ^:private nice-one-10

    {:meta {:calculation
            [1
             '(* 1 8/7)
             '(* 5 8/7)
             '(* 5 8/7 4/3)
             4/3
             3/2
             '(* 7/4 3)
             '(* 7/4 9 3/2)
             '(* 7/4 5)
             '(* 7/4 5 4/3)]}
     :scale [{:ratio 1, :bounded-ratio 1, :bounding-period 7/4}
             {:ratio 2560/2401, :bounded-ratio 2560/2401, :bounding-period 7/4}
             {:ratio 8/7, :bounded-ratio 8/7, :bounding-period 7/4}
             {:ratio 1280/1029, :bounded-ratio 1280/1029, :bounding-period 7/4}
             {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 7/4}
             {:ratio 10240/7203, :bounded-ratio 10240/7203, :bounding-period 7/4}
             {:ratio 3456/2401, :bounded-ratio 3456/2401, :bounding-period 7/4}
             {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 7/4}
             {:ratio 80/49, :bounded-ratio 80/49, :bounding-period 7/4}
             {:ratio 12/7, :bounded-ratio 12/7, :bounding-period 7/4}],
     :added-notes []})

(comment

  (def v (csg/init-cs-tool! (ratios->scale
                              7/4
                              [1
                               (* 1 8/7)
                               (* 7/6 1)
                               (* 7/6 4/3)
                               (* 8/7 3/2)
                               (* 8/7 4/3)
                               (* 8/7 8/7 3/2)
                               (* 8/7 8/7 4/3)
                               4/3
                               3/2]) []))

  ;; nice one
  (csg/update-state v (ratios->scale
                        7/4
                        [1
                         (* 1 8/7)
                         4/3
                         3/2
                         (* 7/4 3)])
                    [])
  (swap! v assoc :period (convo/ratio->cents 7/4))


  ;; nice one - period-7-10tone.scl
  (csg/update-state v (ratios->scale
                        7/4
                        [1
                         (* 1 8/7)
                         (* 5 8/7)
                         (* 5 8/7 4/3)
                         4/3
                         3/2
                         (* 7/4 3)
                         #_(* 7/4 16 3/2) ;; funciona
                         (* 7/4 9 3/2)    ;; funciona
                         (* 7/4 5)
                         (* 7/4 5 4/3)])
                    [])

  (convo/ratio->cents (period-reduce 7/4 (* 5 8/7 4/3)))

  (spit "/Users/diego/Desktop/period-7-10tone.scl"
        (:content (scl/make-scl-file
                    {:scale [{:ratio 1, :bounded-ratio 1, :bounding-period 7/4}
                             {:ratio 2560/2401, :bounded-ratio 2560/2401, :bounding-period 7/4}
                             {:ratio 8/7, :bounded-ratio 8/7, :bounding-period 7/4}
                             {:ratio 1280/1029, :bounded-ratio 1280/1029, :bounding-period 7/4}
                             {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 7/4}
                             {:ratio 10240/7203, :bounded-ratio 10240/7203, :bounding-period 7/4}
                             {:ratio 3456/2401, :bounded-ratio 3456/2401, :bounding-period 7/4}
                             {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 7/4}
                             {:ratio 80/49, :bounded-ratio 80/49, :bounding-period 7/4}
                             {:ratio 12/7, :bounded-ratio 12/7, :bounding-period 7/4}],
                     :added-notes []})))

  (draw-lattice
    {:ratios (map :ratio
                  (ratios->scale
                    7/4
                    [1
                     (* 1 8/7)
                     (* 5 8/7)
                     (* 5 8/7 4/3)
                     4/3
                     3/2
                     (* 7/4 3)
                     #_(* 7/4 16 3/2) ;; funciona
                     (* 7/4 9 3/2)    ;; funciona
                     (* 7/4 5)
                     (* 7/4 5 4/3)]))
     :text-type :factors}))


(comment



  (-> (scale->stacked-subscale
        {:scale (:scale nice-one-10)
         :gen 4
         :offset 0
         :size 10})
      (update :meta merge {:scl/name "9t-p7o4-g4_subscale-stack"
                           :scl/description "From the 10 tone scale in tieminos.scratch.7-period, offset 0"})

      (->> (scl/spit-file "/Users/diego/Music/tunings/9t-p7o4-g4_subscale-stack.scl"))
      )
  (scale->stacked-subscale
    {:scale (:scale nice-one-10)
     :gen 4
     :offset 1
     :size 10})
  (scale->stacked-subscale
    {:scale (:scale nice-one-10)
     :gen 4
     :offset 2
     :size 10}))
