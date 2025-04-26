(ns tieminos.scales.17o7.22tone-through-parallel-tranpositions
  (:require
   [clojure.edn :as edn]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.constant-structures.brute-force :as cs.brute-force]
   [erv.lattice.v2]
   [erv.utils.ratios :refer [ratios->scale]]
   [taoensso.timbre :as timbre]
   [tieminos.lattice.v1.lattice :as lattice.v1]
   [tieminos.pitch-wheel.v1.pitch-wheel :as pitch-wheel.v1]
   [tieminos.scales.17o7.original :refer [original-tritavic-scale-ratios]]
   [tieminos.utils :refer [careful-merge]]))

(declare scales *22tone17o7 subsets generate-22t-ltn-mapping-intervals)

(comment
  (-> scales keys)
  (generate-22t-ltn-mapping-intervals))

(def *22tone17o7-development-version
  (concat
   (->> original-tritavic-scale-ratios
        (ratios->scale 3)
        (map #(assoc % :scale-order 0)))
   (->> (map #(* % 2) original-tritavic-scale-ratios)
        (ratios->scale 3)
        (map #(assoc %
                     :color [0 255 255]
                     :scale-order 1)))
   (->> (map #(* % 4/3) original-tritavic-scale-ratios)
        (ratios->scale 3)
        (map #(assoc %
                     :color [0 0 255]
                     :scale-order 2)))
   (->> (map #(* % 8/3) original-tritavic-scale-ratios)
        (ratios->scale 3)
        (map #(assoc %
                     :color [255 0 255]
                     :scale-order 3)))
   (->> (map #(* % 16/9) original-tritavic-scale-ratios)
        (ratios->scale 3)
        (map #(assoc %
                     :color [100 255 25]
                     :scale-order 4)))
   (->> (map #(* % 32/27) original-tritavic-scale-ratios)
        (ratios->scale 3)
        (map #(assoc %
                     :color [180 100 155]
                     :scale-order 5)))
   (->> (map #(* % 64/27) original-tritavic-scale-ratios)
        (ratios->scale 3)
        (map #(assoc %
                     :color [180 155 100]
                     :scale-order 6)))
   (->> (map #(* % 128/81) original-tritavic-scale-ratios)
        (ratios->scale 3)
        (map #(assoc %
                     :color [180 0 0]
                     :scale-order 6)))))

(comment
  (count *22tone17o7-development-version) ;; NOTE this scale has 64 notes , needs to be deduplicated
  (def pw-atom
    (pitch-wheel.v1/init! {:scale *22tone17o7-development-version
                           :period 3}))

  ;; Deduplicated version of the scale
  (def pw-atom
    (pitch-wheel.v1/init! {:scale *22tone17o7
                           :period 3}))

  (lattice.v1/draw-lattice
   {:ratios (map :bounded-ratio *22tone17o7)
    :period 3
    :custom-edges #{17/7}
    :width 1600
    :height 600
    :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[2 3]])}))
(defn merge-notes
  [notes]
  (reduce
   (fn [data note]
     (-> data
         (merge (dissoc note :scale-order))
         (update :scale-orders (fnil conj #{}) (:scale-order note))))
   {}
   notes))

(def *22tone17o7
  (->> *22tone17o7-development-version
       (group-by (juxt :bounding-period :bounded-ratio))
       (map (fn [[_k notes]]
              (merge-notes notes)))
       (sort-by :bounded-ratio)
       (map-indexed (fn [i note]
                      (assoc note :degree i)))))

;; Constant structures

(comment
  ;; dynamically calculate the CS subsets (takes a long time)
  (def cs-of-22t (->> *22tone17o7 (cs.brute-force/cs-subsets 11)))
  (spit "src/tieminos/scales/17o7/constant-structures-of-22tone-17o7_v1.edn"
        (str (into [] cs-of-22t)))

  ;; A quicker way
  (def cs-of-22t
    (do
      (timbre/info "Reading the constant-structures-of-22tone-17o7_v1.edn file. This may take some time.")
      (let [data (edn/read-string
                  (slurp "src/tieminos/scales/17o7/constant-structures-of-22tone-17o7_v1.edn"))]
        (timbre/info "Done!")
        data)))

  (def cs-of-22t-containing-original-17o7
    (->>  cs-of-22t
          (filter #(set/subset? (set original-tritavic-scale-ratios) (->> % (map :bounded-ratio) set)))
          (group-by count)
          (mapcat (fn [[size scales]]
                    (map-indexed
                     (fn [index scale]
                       (let [name* (format "%st-cs-including-17o7_variation-%s.scl" size (inc index))]
                         {:meta {:scl/name name*
                                 :scl/description (format "%s tone constant structure subset containing the original 8 tone 17o7. Derived from the 22 tone parallel transpositions scale." size)
                                 :size size}
                          :scale scale}))
                     scales)))))

  (-> cs-of-22t-containing-original-17o7))
(def cs-of-22t-containing-original-17o7*
  "Hardcoded versions, derived from `cs-of-22t-containing-original-17o7`"
  [{:meta
    {:scl/name "11t-cs-from-22t-parallel-of-17o7_v1.scl",
     :scl/description
     "11 tone constant structure subset containing the original 8 tone 17o7. Derived from the 22 tone parallel transpositions scale.",
     :size 11},
    :scale
    [{:ratio 1,
      :bounded-ratio 1,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 0}
     {:ratio 256/243,
      :bounded-ratio 256/243,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{6 5},
      :degree 1}
     {:ratio 17/14,
      :bounded-ratio 17/14,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 4}
     {:ratio 4/3,
      :bounded-ratio 4/3,
      :bounding-period 3,
      :scale-orders #{0 1 3 2},
      :color [255 0 255],
      :degree 6}
     {:ratio 3/2,
      :bounded-ratio 3/2,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 9}
     {:ratio 34/21,
      :bounded-ratio 34/21,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 11}
     {:ratio 16/9,
      :bounded-ratio 16/9,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{1 4 3 2 5},
      :degree 12}
     {:ratio 2N,
      :bounded-ratio 2N,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 14}
     {:ratio 512/243,
      :bounded-ratio 512/243,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{6},
      :degree 15}
     {:ratio 17/7,
      :bounded-ratio 17/7,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 18}
     {:ratio 8/3,
      :bounded-ratio 8/3,
      :bounding-period 3,
      :scale-orders #{0 1 4 3 2},
      :color [100 255 25],
      :degree 20}]}
   {:meta
    {:scl/name "11t-cs-from-22t-parallel-of-17o7_v2.scl",
     :scl/description
     "11 tone constant structure subset containing the original 8 tone 17o7. Derived from the 22 tone parallel transpositions scale.",
     :size 11},
    :scale
    [{:ratio 1,
      :bounded-ratio 1,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 0}
     {:ratio 256/243,
      :bounded-ratio 256/243,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{6 5},
      :degree 1}
     {:ratio 17/14,
      :bounded-ratio 17/14,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 4}
     {:ratio 4/3,
      :bounded-ratio 4/3,
      :bounding-period 3,
      :scale-orders #{0 1 3 2},
      :color [255 0 255],
      :degree 6}
     {:ratio 3/2,
      :bounded-ratio 3/2,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 9}
     {:ratio 34/21,
      :bounded-ratio 34/21,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 11}
     {:ratio 16/9,
      :bounded-ratio 16/9,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{1 4 3 2 5},
      :degree 12}
     {:ratio 2N,
      :bounded-ratio 2N,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 14}
     {:ratio 136/63,
      :bounded-ratio 136/63,
      :bounding-period 3,
      :color [100 255 25],
      :scale-orders #{4 3 2},
      :degree 16}
     {:ratio 17/7,
      :bounded-ratio 17/7,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 18}
     {:ratio 8/3,
      :bounded-ratio 8/3,
      :bounding-period 3,
      :scale-orders #{0 1 4 3 2},
      :color [100 255 25],
      :degree 20}]}
   {:meta
    {:scl/name "11t-cs-from-22t-parallel-of-17o7_v3.scl",
     :scl/description
     "11 tone constant structure subset containing the original 8 tone 17o7. Derived from the 22 tone parallel transpositions scale.",
     :size 11},
    :scale
    [{:ratio 1,
      :bounded-ratio 1,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 0}
     {:ratio 68/63,
      :bounded-ratio 68/63,
      :bounding-period 3,
      :color [255 0 255],
      :scale-orders #{1 3 2},
      :degree 2}
     {:ratio 17/14,
      :bounded-ratio 17/14,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 4}
     {:ratio 4/3,
      :bounded-ratio 4/3,
      :bounding-period 3,
      :scale-orders #{0 1 3 2},
      :color [255 0 255],
      :degree 6}
     {:ratio 3/2,
      :bounded-ratio 3/2,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 9}
     {:ratio 34/21,
      :bounded-ratio 34/21,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 11}
     {:ratio 16/9,
      :bounded-ratio 16/9,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{1 4 3 2 5},
      :degree 12}
     {:ratio 2N,
      :bounded-ratio 2N,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 14}
     {:ratio 512/243,
      :bounded-ratio 512/243,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{6},
      :degree 15}
     {:ratio 17/7,
      :bounded-ratio 17/7,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 18}
     {:ratio 8/3,
      :bounded-ratio 8/3,
      :bounding-period 3,
      :scale-orders #{0 1 4 3 2},
      :color [100 255 25],
      :degree 20}]}
   {:meta
    {:scl/name "11t-cs-from-22t-parallel-of-17o7_v4.scl",
     :scl/description
     "11 tone constant structure subset containing the original 8 tone 17o7. Derived from the 22 tone parallel transpositions scale.",
     :size 11},
    :scale
    [{:ratio 1,
      :bounded-ratio 1,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 0}
     {:ratio 68/63,
      :bounded-ratio 68/63,
      :bounding-period 3,
      :color [255 0 255],
      :scale-orders #{1 3 2},
      :degree 2}
     {:ratio 17/14,
      :bounded-ratio 17/14,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 4}
     {:ratio 4/3,
      :bounded-ratio 4/3,
      :bounding-period 3,
      :scale-orders #{0 1 3 2},
      :color [255 0 255],
      :degree 6}
     {:ratio 3/2,
      :bounded-ratio 3/2,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 9}
     {:ratio 34/21,
      :bounded-ratio 34/21,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 11}
     {:ratio 16/9,
      :bounded-ratio 16/9,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{1 4 3 2 5},
      :degree 12}
     {:ratio 2N,
      :bounded-ratio 2N,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 14}
     {:ratio 136/63,
      :bounded-ratio 136/63,
      :bounding-period 3,
      :color [100 255 25],
      :scale-orders #{4 3 2},
      :degree 16}
     {:ratio 17/7,
      :bounded-ratio 17/7,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 18}
     {:ratio 8/3,
      :bounded-ratio 8/3,
      :bounding-period 3,
      :scale-orders #{0 1 4 3 2},
      :color [100 255 25],
      :degree 20}]}
   {:meta
    {:scl/name "12t-cs-from-22t-parallel-of-17o7_v1.scl",
     :scl/description
     "12 tone constant structure subset containing the original 8 tone 17o7. Derived from the 22 tone parallel transpositions scale.",
     :size 12},
    :scale
    [{:ratio 1,
      :bounded-ratio 1,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 0}
     {:ratio 256/243,
      :bounded-ratio 256/243,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{6 5},
      :degree 1}
     {:ratio 17/14,
      :bounded-ratio 17/14,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 4}
     {:ratio 4/3,
      :bounded-ratio 4/3,
      :bounding-period 3,
      :scale-orders #{0 1 3 2},
      :color [255 0 255],
      :degree 6}
     {:ratio 3/2,
      :bounded-ratio 3/2,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 9}
     {:ratio 34/21,
      :bounded-ratio 34/21,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 11}
     {:ratio 16/9,
      :bounded-ratio 16/9,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{1 4 3 2 5},
      :degree 12}
     {:ratio 2N,
      :bounded-ratio 2N,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 14}
     {:ratio 512/243,
      :bounded-ratio 512/243,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{6},
      :degree 15}
     {:ratio 17/7,
      :bounded-ratio 17/7,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 18}
     {:ratio 8/3,
      :bounded-ratio 8/3,
      :bounding-period 3,
      :scale-orders #{0 1 4 3 2},
      :color [100 255 25],
      :degree 20}
     {:ratio 544/189,
      :bounded-ratio 544/189,
      :bounding-period 3,
      :color [180 155 100],
      :scale-orders #{4 6 5},
      :degree 21}]}
   {:meta
    {:scl/name "12t-cs-from-22t-parallel-of-17o7_v2.scl",
     :scl/description
     "12 tone constant structure subset containing the original 8 tone 17o7. Derived from the 22 tone parallel transpositions scale.",
     :size 12},
    :scale
    [{:ratio 1,
      :bounded-ratio 1,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 0}
     {:ratio 256/243,
      :bounded-ratio 256/243,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{6 5},
      :degree 1}
     {:ratio 17/14,
      :bounded-ratio 17/14,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 4}
     {:ratio 4/3,
      :bounded-ratio 4/3,
      :bounding-period 3,
      :scale-orders #{0 1 3 2},
      :color [255 0 255],
      :degree 6}
     {:ratio 3/2,
      :bounded-ratio 3/2,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 9}
     {:ratio 34/21,
      :bounded-ratio 34/21,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 11}
     {:ratio 16/9,
      :bounded-ratio 16/9,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{1 4 3 2 5},
      :degree 12}
     {:ratio 2N,
      :bounded-ratio 2N,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 14}
     {:ratio 136/63,
      :bounded-ratio 136/63,
      :bounding-period 3,
      :color [100 255 25],
      :scale-orders #{4 3 2},
      :degree 16}
     {:ratio 17/7,
      :bounded-ratio 17/7,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 18}
     {:ratio 8/3,
      :bounded-ratio 8/3,
      :bounding-period 3,
      :scale-orders #{0 1 4 3 2},
      :color [100 255 25],
      :degree 20}
     {:ratio 544/189,
      :bounded-ratio 544/189,
      :bounding-period 3,
      :color [180 155 100],
      :scale-orders #{4 6 5},
      :degree 21}]}
   {:meta
    {:scl/name "12t-cs-from-22t-parallel-of-17o7_v3.scl",
     :scl/description
     "12 tone constant structure subset containing the original 8 tone 17o7. Derived from the 22 tone parallel transpositions scale.",
     :size 12},
    :scale
    [{:ratio 1,
      :bounded-ratio 1,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 0}
     {:ratio 68/63,
      :bounded-ratio 68/63,
      :bounding-period 3,
      :color [255 0 255],
      :scale-orders #{1 3 2},
      :degree 2}
     {:ratio 17/14,
      :bounded-ratio 17/14,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 4}
     {:ratio 4/3,
      :bounded-ratio 4/3,
      :bounding-period 3,
      :scale-orders #{0 1 3 2},
      :color [255 0 255],
      :degree 6}
     {:ratio 3/2,
      :bounded-ratio 3/2,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 9}
     {:ratio 34/21,
      :bounded-ratio 34/21,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 11}
     {:ratio 16/9,
      :bounded-ratio 16/9,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{1 4 3 2 5},
      :degree 12}
     {:ratio 2N,
      :bounded-ratio 2N,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 14}
     {:ratio 512/243,
      :bounded-ratio 512/243,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{6},
      :degree 15}
     {:ratio 17/7,
      :bounded-ratio 17/7,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 18}
     {:ratio 8/3,
      :bounded-ratio 8/3,
      :bounding-period 3,
      :scale-orders #{0 1 4 3 2},
      :color [100 255 25],
      :degree 20}
     {:ratio 544/189,
      :bounded-ratio 544/189,
      :bounding-period 3,
      :color [180 155 100],
      :scale-orders #{4 6 5},
      :degree 21}]}
   {:meta
    {:scl/name "12t-cs-from-22t-parallel-of-17o7_v4.scl",
     :scl/description
     "12 tone constant structure subset containing the original 8 tone 17o7. Derived from the 22 tone parallel transpositions scale.",
     :size 12},
    :scale
    [{:ratio 1,
      :bounded-ratio 1,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 0}
     {:ratio 68/63,
      :bounded-ratio 68/63,
      :bounding-period 3,
      :color [255 0 255],
      :scale-orders #{1 3 2},
      :degree 2}
     {:ratio 17/14,
      :bounded-ratio 17/14,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 4}
     {:ratio 4/3,
      :bounded-ratio 4/3,
      :bounding-period 3,
      :scale-orders #{0 1 3 2},
      :color [255 0 255],
      :degree 6}
     {:ratio 3/2,
      :bounded-ratio 3/2,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 9}
     {:ratio 34/21,
      :bounded-ratio 34/21,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 11}
     {:ratio 16/9,
      :bounded-ratio 16/9,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{1 4 3 2 5},
      :degree 12}
     {:ratio 2N,
      :bounded-ratio 2N,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 14}
     {:ratio 136/63,
      :bounded-ratio 136/63,
      :bounding-period 3,
      :color [100 255 25],
      :scale-orders #{4 3 2},
      :degree 16}
     {:ratio 17/7,
      :bounded-ratio 17/7,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 18}
     {:ratio 8/3,
      :bounded-ratio 8/3,
      :bounding-period 3,
      :scale-orders #{0 1 4 3 2},
      :color [100 255 25],
      :degree 20}
     {:ratio 544/189,
      :bounded-ratio 544/189,
      :bounding-period 3,
      :color [180 155 100],
      :scale-orders #{4 6 5},
      :degree 21}]}
   {:meta
    {:scl/name "15t-cs-from-22t-parallel-of-17o7_v1.scl",
     :scl/description
     "15 tone constant structure subset containing the original 8 tone 17o7. Derived from the 22 tone parallel transpositions scale.",
     :size 15},
    :scale
    [{:ratio 1,
      :bounded-ratio 1,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 0}
     {:ratio 68/63,
      :bounded-ratio 68/63,
      :bounding-period 3,
      :color [255 0 255],
      :scale-orders #{1 3 2},
      :degree 2}
     {:ratio 32/27,
      :bounded-ratio 32/27,
      :bounding-period 3,
      :color [180 155 100],
      :scale-orders #{4 6 3 2 5},
      :degree 3}
     {:ratio 17/14,
      :bounded-ratio 17/14,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 4}
     {:ratio 4/3,
      :bounded-ratio 4/3,
      :bounding-period 3,
      :scale-orders #{0 1 3 2},
      :color [255 0 255],
      :degree 6}
     {:ratio 272/189,
      :bounded-ratio 272/189,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{4 3 5},
      :degree 8}
     {:ratio 3/2,
      :bounded-ratio 3/2,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 9}
     {:ratio 34/21,
      :bounded-ratio 34/21,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 11}
     {:ratio 16/9,
      :bounded-ratio 16/9,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{1 4 3 2 5},
      :degree 12}
     {:ratio 1088/567,
      :bounded-ratio 1088/567,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{6 5},
      :degree 13}
     {:ratio 2N,
      :bounded-ratio 2N,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 14}
     {:ratio 136/63,
      :bounded-ratio 136/63,
      :bounding-period 3,
      :color [100 255 25],
      :scale-orders #{4 3 2},
      :degree 16}
     {:ratio 64/27,
      :bounded-ratio 64/27,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{4 6 3 5},
      :degree 17}
     {:ratio 17/7,
      :bounded-ratio 17/7,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 18}
     {:ratio 8/3,
      :bounded-ratio 8/3,
      :bounding-period 3,
      :scale-orders #{0 1 4 3 2},
      :color [100 255 25],
      :degree 20}]}
   {:meta
    {:scl/name "16t-cs-from-22t-parallel-of-17o7_v1.scl",
     :scl/description
     "16 tone constant structure subset containing the original 8 tone 17o7. Derived from the 22 tone parallel transpositions scale.",
     :size 16},
    :scale
    [{:ratio 1,
      :bounded-ratio 1,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 0}
     {:ratio 68/63,
      :bounded-ratio 68/63,
      :bounding-period 3,
      :color [255 0 255],
      :scale-orders #{1 3 2},
      :degree 2}
     {:ratio 32/27,
      :bounded-ratio 32/27,
      :bounding-period 3,
      :color [180 155 100],
      :scale-orders #{4 6 3 2 5},
      :degree 3}
     {:ratio 17/14,
      :bounded-ratio 17/14,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 4}
     {:ratio 4/3,
      :bounded-ratio 4/3,
      :bounding-period 3,
      :scale-orders #{0 1 3 2},
      :color [255 0 255],
      :degree 6}
     {:ratio 272/189,
      :bounded-ratio 272/189,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{4 3 5},
      :degree 8}
     {:ratio 3/2,
      :bounded-ratio 3/2,
      :bounding-period 3,
      :scale-orders #{0},
      :degree 9}
     {:ratio 34/21,
      :bounded-ratio 34/21,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 11}
     {:ratio 16/9,
      :bounded-ratio 16/9,
      :bounding-period 3,
      :color [180 100 155],
      :scale-orders #{1 4 3 2 5},
      :degree 12}
     {:ratio 1088/567,
      :bounded-ratio 1088/567,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{6 5},
      :degree 13}
     {:ratio 2N,
      :bounded-ratio 2N,
      :bounding-period 3,
      :scale-orders #{0 1 2},
      :color [0 0 255],
      :degree 14}
     {:ratio 136/63,
      :bounded-ratio 136/63,
      :bounding-period 3,
      :color [100 255 25],
      :scale-orders #{4 3 2},
      :degree 16}
     {:ratio 64/27,
      :bounded-ratio 64/27,
      :bounding-period 3,
      :color [180 0 0],
      :scale-orders #{4 6 3 5},
      :degree 17}
     {:ratio 17/7,
      :bounded-ratio 17/7,
      :bounding-period 3,
      :scale-orders #{0 1},
      :color [0 255 255],
      :degree 18}
     {:ratio 8/3,
      :bounded-ratio 8/3,
      :bounding-period 3,
      :scale-orders #{0 1 4 3 2},
      :color [100 255 25],
      :degree 20}
     {:ratio 544/189,
      :bounded-ratio 544/189,
      :bounding-period 3,
      :color [180 155 100],
      :scale-orders #{4 6 5},
      :degree 21}]}])

(def subsets
  (->> cs-of-22t-containing-original-17o7*
       (map
        (fn [scale-data]
          [(-> scale-data
               :meta :scl/name
               (str/replace #".scl" "")
               keyword)
           scale-data]))
       (into {})))

(defn generate-22t-ltn-mapping-intervals
  "Generate a intervalic mapping that the lumatone workfile expects to color a 22 tone layout for the :22t-by-parallel-transpositions scale"
  []
  (let [scale-orders (->> *22tone17o7
                          (map :scale-orders)
                          (apply concat)
                          set
                          sort)
        degss (->> scale-orders
                   (map (fn [order] (->> *22tone17o7
                                         (filter #((:scale-orders %) order))
                                         (map :degree)))))
        intervals (fn [degs]
                    (let [init (if (= 0 (first degs)) [] [0])
                          final (if (= 22 (last degs)) [] [22])]
                      (->> (concat init degs final)
                           (partition 2 1)
                           (map (fn [[a b]] (- b a))))))]
    (map intervals degss)))

(def scales
  (careful-merge
   {:22t-by-parallel-transpositions
    {:meta {:scl/name "22t-parallel-transpositions-of-17o7-v1-tritavic.scl"
            :scl/description "Tritavic 17/7 colored scale."}
     :scale *22tone17o7}}
   subsets))
