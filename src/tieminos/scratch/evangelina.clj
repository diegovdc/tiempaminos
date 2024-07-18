(ns tieminos.scratch.evangelina
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [erv.constant-structures.core :as cs]
   [erv.constant-structures.graphics :refer [init-cs-tool!]]
   [erv.utils.core :as utils :refer [rotate]]
   [erv.utils.ratios :refer [ratios->scale]]))

(def ^:private evangelina
  [1
   19/18
   13/12
   10/9
   9/8
   7/6
   11/9
   5/4
   23/18
   4/3
   11/8
   45/32
   17/12
   3/2
   19/12
   13/8
   5/3
   27/16
   7/4
   11/6
   15/8
   23/12])

(comment
  #_(spit "/Users/diego/Desktop/evangelina-cs-analysis.csv"
          (str "ratio,steps,intervals ->\n"
               (->> (cs/analyze (ratios->scale evangelina))
                    :interval-data
                    (map (juxt first
                               (comp first :steps second)
                               (comp
                                 #(str/join ", " %)
                                 (partial map (comp #(str/join "-" %) :interval))
                                 :intervals
                                 second)))
                    (sort-by (juxt second first))
                    (map #(str/join "," %))
                    (str/join "\n"))))
  (init-cs-tool! (ratios->scale evangelina) [])

  (->> (combo/cartesian-product
         (->> (combo/combinations evangelina 21)
              (map ratios->scale)
              (filter (comp :constant-structure? cs/analyze))
              (map #(map :ratio %)))
         (->> (combo/combinations evangelina 20)
              (map ratios->scale)
              (filter (comp :constant-structure? cs/analyze))
              (map #(map :ratio %))))
       (filter (fn [pair]
                 (let [[a b] (sort-by count pair)]
                   (set/subset? (set a) (set b))))))

  (count (->> (combo/combinations evangelina 22)
              (filter (comp :constant-structure? cs/analyze ratios->scale))))

  (let [scale (first (->> (combo/combinations evangelina 12)
                          (filter (comp :constant-structure? cs/analyze ratios->scale))))]
    (first (->> (combo/combinations scale 11)
                (filter (comp :constant-structure? cs/analyze ratios->scale)))))

  (sort > [2 3 1])
  (defn cs-rings [scale ring-sizes nth-fn]
    (loop [rings []
           scale scale
           ring-sizes (sort > ring-sizes)]
      (let [current-ring (first ring-sizes)
            rest-rings (rest ring-sizes)
            subscale (->> (combo/combinations scale current-ring)
                          (filter (comp :constant-structure? cs/analyze ratios->scale))
                          nth-fn)]
        (cond
          (not subscale) rings
          (not (seq rest-rings)) rings
          :else (recur (conj rings subscale) subscale rest-rings)))))

  (cs-rings evangelina [13 8 5 3] (comp rand-nth (partial take 1000)))

  ;;  there's too many
  (->> (range  (count evangelina))
       (mapcat #(->> (combo/combinations evangelina %)
                     (map ratios->scale)
                     (filter (comp :constant-structure? cs/analyze ratios->scale))))
       count)

  (map (fn [i]
         (let [scale (sort (utils/pick-pattern (rotate evangelina i) [3 3 3 3 3 3 3 1]))]
           [i scale]))
       (range 22))

  ratios->scale)
