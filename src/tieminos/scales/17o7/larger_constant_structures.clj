(ns tieminos.scales.17o7.larger-constant-structures
  (:require
   [clojure.core.async :as async]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [erv.constant-structures.brute-force :as cs.brute-force]
   [erv.lattice.v2]
   [erv.utils.conversions :refer [ratio->cents]]
   [erv.utils.core :refer [period-reduce]]
   [erv.utils.ratios :refer [ratios->scale]]
   [erv.utils.scale :refer [find-subset-degrees]]
   [tieminos.lattice.v1.lattice :as lattice.v1]
   [tieminos.pitch-wheel.v1.pitch-wheel :as pitch-wheel.v1]
   [tieminos.scales.17o7.original :refer [original-tritavic-scale-ratios]]))

(comment

  (do
    (def ratios (let [chain (reduce (fn [acc i] (conj acc (apply * (repeat i 2)))) [] (range 8))
                      ratios  (->> (flatten [chain
                                             (map #(* 17/7 %) chain)
                                             (map #(* 7/17 %) chain)
                                             #_(map #(* 17/7 17/7 %) chain)])
                                   (map #(period-reduce 3 (/ % 8/3))))]
                  ratios))

    (count ratios))
  (lattice.v1/draw-lattice
   {:id :17o7/larger-cs-search
    :ratios ratios
    :period 3
    :custom-edges #{17/7}
    :width 1600
    :height 600
    :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[2 3]])})
  (pitch-wheel.v1/init!
   {:scale (ratios->scale 3 ratios)
    :period 3})

  (/ (combo/count-combinations (range (count ratios)) 11)
     1000000.0)
  (def subsets (atom nil))
  (async/go
    (reset! subsets (sequence (cs.brute-force/quick-cs-subsets [11] (ratios->scale 3 ratios))))
    (cs.brute-force/take-quick-cs-subsets 0 1 @subsets)
    (->> @subsets count (println "found subsets")))
  (->> @subsets)
  (spit (str user/datasets-dir "/17o7-constant-structures/11t-cs-from-24-tone-no1.edn")
        (str (into [] @subsets)))
  ;; 12, 15, 16, 18, 19, 20, 21,

  (def subset-index (atom -1))
  (lattice.v1/draw-lattice
   {:id :17o7/larger-cs-subset
    :ratios (map :bounded-ratio (nth subsets (swap! subset-index inc)))
    :period 3
    :custom-edges #{17/7}
    :width 1600
    :height 600
    :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[2 3]])}))

(comment
  (def cs-of-22t-from-30t
    (->> (slurp "resources/data-sets/17o7-constant-structures/22t-cs-from-30-tone-no1.edn")
         edn/read-string))

  (->> cs-of-22t-from-30t
       #_(take 2)
       (map-indexed
        (fn [i scale]
          (let [match-data (find-subset-degrees
                            {:scale scale
                             :subset-ratios original-tritavic-scale-ratios})]
            {:scale-index i
             :match-data {:total (count match-data)
                          :data match-data}})))
       (sort-by #(-> % :match-data :total) >))

  cs-of-22t-from-30t)

(do
  (defn analyse-subsets [subsets]
    (->> subsets
         #_(take 2)
         #_(take 2)
         (map-indexed
          (fn [i scale]
            (let [match-data (find-subset-degrees
                              {:scale scale
                               :subset-ratios original-tritavic-scale-ratios})
                  degs (mapcat :degrees match-data)
                  degs-freqs (frequencies degs)]
              {:scale-index i
               :match-data {:total (count match-data)
                            :data match-data
                            :degrees-freqs degs-freqs
                            :total-degrees (->> degs set count)}})))
         #_(sort-by (juxt #(-> % :match-data :total)))
         (sort-by (juxt (fn [data] (-> data :match-data :total))
                        (fn [data] (-> data :match-data :total-degrees))
                        #_(fn [data]
                            (->> data :match-data :data (mapcat :degrees) set count))))

         reverse))
  (analyse-subsets cs-of-19t-from-27t))

(comment

  (user/spit-scl
   {:meta {:scl/name "17o7_19t-cs-from-27t_no84"
           :description "Contains 4 versions of the original 8 tone 17/7 scale."}
    :scale (nth subsets* 84)})

  (->> (analyse-subsets cs-of-19t-from-27t)
       (filter (fn [{:keys [scale-index]}] (= scale-index 84)))
       first
       :match-data
       :data
       (map :degrees))

  (def cs-of-19t-from-27t
    (->> (slurp (str user/datasets-dir "/17o7-constant-structures/19t-cs-from-27-tone-no1.edn"))
         edn/read-string))

  (def subset-index (atom -1))

  (def subsets* cs-of-19t-from-27t)
  (def analysis* (->> (analyse-subsets subsets*)
                      (map (juxt (constantly :index)
                                 :scale-index
                                 (comp :total :match-data)
                                 (constantly :degrees-with-17o7)
                                 (comp :total-degrees :match-data)
                                 (constantly :common-tones-in-17o7s)
                                 (comp frequencies vals :degrees-freqs :match-data)))))
  (analyse-subsets subsets*)

  (let [analysis (nth analysis* 6 #_(swap! subset-index inc))
        scale  (nth subsets*
                    (second analysis))]
    (println analysis)
    #_(println (->> scale (map :bounded-ratio) (str/join "\n")))
    (lattice.v1/draw-lattice
     {:id :17o7/*19t-cs-subsets
      :scale-data {:meta {:scl/name (str analysis)}
                   :scale scale}
      :period 3
      :custom-edges #{17/7}
      :width 1600
      :height 600
      :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[2 3]])})
    (pitch-wheel.v1/init!
     {:id :17o7/*19t-cs-subsets
      :ratios (map :bounded-ratio scale)
      :period 3})
    nil))

(comment
  (ratio->cents 17/14)
  (ratio->cents 21/17))
