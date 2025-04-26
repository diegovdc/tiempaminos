(ns tieminos.scales.17o7.original
  (:require
   [clojure.set :as set]
   [erv.lattice.v2]
   [erv.utils.ratios :refer [ratios->scale]]
   [erv.utils.scale :refer [dedupe-scale]]
   [tieminos.lattice.v1.lattice :as lattice.v1]
   [tieminos.osc.surge :as surge]))

(def original-tritavic-scale-ratios
  (->> (let [cell [1 17/7 4/3]]
         (concat cell
                 (map #(* 3/2 %) cell)
                 (map #(* 3/2 4/3 %) cell)))
       (ratios->scale 3)
       (map :ratio)
       sort
       (dedupe)))

(comment
  (lattice.v1/draw-lattice
   {:ratios original-tritavic-scale-ratios
    :period 3
    :custom-edges #{17/7}
    :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[2 3]])}))

;;  Other scales with 17 and 7
;;  TODO explore this possible scales
(def scales-17&7
  (map #(ratios->scale 3 %)
       [(let [cell [1 17/7]]
          ;; chida
          (concat cell
                  (map #(* 3/2 %) cell)
                  (map #(* 17/5 3/2 %) cell)))

        (let [cell [1 17/16 (* 3 7/17) 4/3]]
          ;; no ta mal
          (concat cell
                  (map #(* 3/2 %) cell)
                  (map #(* 3/2 3/2 %) cell)))
        ;; bonita
        [1
         2
         (* 2 17/5)
         (* 2 17/5 17/5)
         4/3
         3/2
         17/5
         (* 3/2 17/5)]
        [1
         17/11
         17/7
         7/4
         (* 8/7 7/4)
         2
         3/2
         4/3
         17/5
         17/9
         (* 17/9 10/9)
         10/6]
        (let [cell [1 17/7 4/3]]
          ;; chida
          (concat cell
                  (map #(* 3/2 %) cell)
                  (map #(* 3/2 4/3 %) cell)))]))
