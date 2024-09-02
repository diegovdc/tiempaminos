(ns tieminos.compositions.garden-earth.analysis
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.scale.core :as scale]
   [erv.utils.conversions :as conv]
   [erv.utils.core :as utils]
   [tieminos.compositions.garden-earth.base :refer [eik pr-set subcps]]))

(def note-set->pitch-class
  (->> eik
       :scale
       (reduce (fn [m {:keys [set pitch]}]
                 (assoc m set (:class pitch)))
               {})))

(def pitch-class->note-set
  (->> eik
       :scale
       (reduce (fn [m {:keys [set pitch]}]
                 (assoc m (:class pitch) set))
               {})))

(def pitch-class->bounded-ratio
  (->> eik
       :scale
       (reduce (fn [m {:keys [bounded-ratio pitch]}]
                 (assoc m (:class pitch) bounded-ratio))
               {})))

(defn find-supersets
  "Get all subcps that include all the given `sets`.
  Note that `sets` is a set of sets."
  [sets]
  (->> eik :subcps
       (filter (fn [[k data]]
                 (= sets (set/intersection
                           (set (map :set (:scale data)))
                           sets))))))
(comment
  ;; TODO convert into tests
  (sort (keys (find-supersets #{#{3 11 9}
                                #{7 3 9}
                                #{5 3 9}})))
  (sort (keys (find-supersets #{#{7 3 9}
                                #{3 11 9}})))
  (sort (keys (find-supersets
                (set (map :set (subcps "1)4 of 3)6 1.11-3.5.7.9"))))))
  (sort (keys (find-supersets
                #{#{7 1 11} #{1 11 9} #{1 11 5}})))
  (sort (keys (find-supersets #{#{1 11 9} #{1 11 5}})))

  ;; e and g# cluster
  (sort (keys (find-supersets #{#{1 11 9} #{9 11 5}
                                #{7 1 9}
                                #{7 1 5}})))
  (->> (sort (keys (find-supersets #{#{11 9 5}
                                     #{11 3 7}})))
       #_(remove #(str/includes? % ")5"))))

(defn find-subcps-intersections
  "Find the intersecting notes as sets for a `subcps` (string key) using the row-column identifier of the cps (e.g. '2)4') to search in all the subcps sets that belong to that identifier.
  Note that `intersecting-cps-sizes-set` is a set of row-column identifiers.
  Options:
    `:data-output-type` #{:both :pitch-classes :sets} - defaults to `:both`."
  [subcps intersecting-cps-sizes-set &
   {:keys [data-output-type]
    :or {data-output-type :both}}]
  (let [subcps-sets (-> eik :subcps (get subcps) :scale (->> (map :set)) set)]
    (->> eik :subcps
         (filter (fn [[k]]
                   (some true? (map #(str/includes? k %) intersecting-cps-sizes-set))))
         (map (fn [[k data]]
                (let [sets* (seq (set/intersection (->> data :scale (map :set) set)
                                                   subcps-sets))
                      sets (case data-output-type
                             :both (map (fn [s] [(note-set->pitch-class s) s]) sets*)
                             :pitch-classes (map note-set->pitch-class sets*)
                             :sets sets*)]
                  [k sets])))
         (filter (fn [[_k data]] (seq data)))
         (sort-by (juxt (comp count second) first))
         reverse)))

(comment
    ;; TODO convert into test
  (find-subcps-intersections
    "1)4 of 3)6 1.3-5.7.9.11" #{"2)4"}))

(comment
  ;; projections (transpositions/stellations)
  (def scale (get-in eik [:subcps "1)4 of 3)6 1.3-5.7.9.11" :scale]))
  (def scale2 (get-in eik [:subcps "3)4 of 3)6 5.7.9.11" :scale]))
  (def scale3 (get-in eik [:subcps "1)4 of 3)6 1.11-3.5.7.9" :scale]))
  (scale/print-scale-intervals! scale :unit :ratios :ratio-type :ratio)
  (scale/print-scale-intervals! scale3 :unit :ratios)
  (utils/prime-factors 21)
  (utils/prime-factors 147)
  (utils/prime-factors 189)
  (utils/prime-factors 105)
  [(map (comp :class :pitch) (scale/+names 440 scale))
   (map #(conv/cps->name* (* 440 %)) [(* 21/16 1)
                                      (* 21/16 14/11)
                                      (* 21/16 18/11)
                                      (* 21/16 20/11)])]
  [(map (comp :class :pitch) (scale/+names 440 scale))
   (map #(conv/cps->name* (* 440 %)))]
  [(* 21 1)
   (* 21 11/9)
   (* 21 7/9)
   (* 21 5/9)]
  (* 35/3 27/16)

  ;; symmetries
  (def scale (get-in eik [:subcps "1)4 of 3)6 1.3-5.7.9.11" :scale]))
  (def scale2 (get-in eik [:subcps "3)4 of 3)6 5.7.9.11" :scale]))
  (do (println "1)4 of 3)6 1.3-5.7.9.11"
               (->> scale (map :ratio) (map #(/ % 15)) sort))
      (scale/print-scale-intervals! scale :unit :ratios :ratio-type :ratio))
  (do (println "3)4 of 3)6 5.7.9.11"
               (->> scale2 (map :ratio) (map #(/ % 693)) sort))
      (scale/print-scale-intervals! scale2 :unit :ratios :ratio-type :ratio))

  [(->> scale (map :ratio) (map #(/ % 15)) sort)
   (->> scale2 (map :ratio) (map #(/ % 693)) sort)])

(comment
  ;; neighboring tones in

  (let [subs* (->> eik :subcps (filter #(str/includes? % "2)4")) keys)
        subsets (:subcps eik)
        scale-set #(-> % subsets :scale set)]
    (->> (for [s1 subs* s2 subs*]
           (conj (vec (sort [s1 s2]))
                 (count (set/intersection (scale-set s1)
                                          (scale-set s2)))
                 (str/join " " (mapv (comp pr-set :set)
                                     (set/intersection (scale-set s1)
                                                       (scale-set s2))))))
         set
         (remove (fn [[_ _ count*]] (= 6 count*)))
         (sort-by (juxt #(nth % 2) first second))
         reverse
         (map #(str/join "," %))
         (str/join "\n"))))
