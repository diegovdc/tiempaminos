(ns tieminos.polydori.scale
  (:require
   [erv.cps.core :as cps]
   [erv.utils.conversions :as conv]
   [erv.scale.scl :refer [make-scl-file]]))

(def root 293.66476791741)

(defn +cents [scale]
  (map #(assoc % :cents (conv/ratio->cents (:bounded-ratio %)))
       scale))

(defn +degree [scale]
  (map-indexed (fn [i n] (assoc n :degree i)) scale))

(do
  (defn dedupe-scale [scale]
    (->>  scale
          (group-by :bounded-ratio)
          (map (fn [[_ notes]]
                 (let [sets (map :set notes)
                       archi-sets (map :archi-set notes)]
                   (-> notes first
                       (dissoc :set :archi-set)
                       (assoc :sets (set sets)
                              :archi-sets (set archi-sets))))))
          (sort-by :bounded-ratio)
          (into [])))
  #_(-> polydori :scale
        dedupe-scale))

(def polydori
  (-> (cps/make 4 [1 3 9 19 15 21 7]
                :norm-fac (* 15 21 19 9) ; la buena tonica del dorico
                ;; :norm-fac (* 7 1 15 3)
                )
      cps/+all-subcps
      (update :scale (comp +cents +degree))))

(def polydori-v2
  "Deduplicated version of polydori has `:sets` and `:archi-sets` keys"
  (-> polydori
      (update :scale dedupe-scale)
      (update :scale (comp +cents +degree))))

(def polydori-set->deg
  (->> polydori-v2
       :scale
       (group-by :sets)
       (mapcat (fn [[sets [note]]]
                 (map (fn [s] {s (:degree note)})
                      sets)))
       (apply merge)))

(def polydori-deg->sets
  (->> polydori-set->deg
       (group-by second)
       (map (fn [[k vs]] [k (mapv first vs)]))
       (into {})))

(comment
  (->> polydori-set->deg (sort-by second))
  (polydori-set->deg #{1 15 21 9})
  (-> polydori-v2))

(def polydori-by-sets (->> polydori :scale (group-by :set)))
#_(-> polydori-by-sets)

(comment
  (let [{:keys [filename content]} (make-scl-file polydori-v2)]
    (println filename)
    (spit (str "/Users/diego/Music/tunings/" filename) content)))

