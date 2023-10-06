(ns tieminos.7d-percussion-ensamble.analysis
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [tieminos.polydori.analysis.dorian-hexanies
    :refer [dorian-hexanies-in-polydori]]
   [tieminos.polydori.scale :refer [polydori-set->deg polydori-v2]]
   [tieminos.utils :refer [indexes-of map-subscale-degs]]))

;;;;;;
;;; Making some sort of mapping from hexany to hexany with data about the
;;; common degrees (quantity) and parent hexany set
;;;;;;

;; PURPOSE look for a suitable harmonic path that traverses all of the 7 groups of hexanies (3 hexanies per group)


(comment

  ;; input data, for reference
  (-> polydori-v2)
  (-> polydori-set->deg)
  (-> dorian-hexanies-in-polydori))

;;;; Steps
;; 1.DONE  add id and full names to hexanies; i.e. id :hx1.1 :hx1.1-1.3.19.9*7.21
;; 2. DONE match names in data structure


#_"e.g" {:intersection #{:hx1.1 :hx1.2}
         :degrees/parent #{4 7 12}
         :degrees/local {:hx1.1 [0 1 2]
                         :hx1.2 [0 3 5]}}

;; 3. DONE (optional) make a function for getting the full intersection data
;; for a particular pair, for exploration purposes

;; 4. DONE make a summary map
#_"e.g." {:hx1.3 [[:hx1.1 {:count 4}] [:hx3.3 {:count 4}]]} ; 3 is the number of intersections



;; Step 1


(defn add-diatonic-hexany-id-and-name
  [index hexany]
  (let [id-str (format "hx%s.%s"
                       (inc (quot index 3))
                       (inc (mod index 3)))]
    (assoc hexany
           :id (keyword id-str)
           :full-name (keyword (format "%s-%s*%s"
                                       id-str
                                       (->> hexany :unique-factors sort (str/join "."))
                                       (->> hexany :common-factors sort (str/join ".")))))))

(def named-diatonic-hexanies
  (->> dorian-hexanies-in-polydori
       (map-indexed add-diatonic-hexany-id-and-name)))

(def diatonic-hexanies-by-id
  (reduce (fn [acc hx]
            (assoc acc (:id hx) hx))
          {}
          named-diatonic-hexanies))

;; Step 2

(defn intersect-diatonic-hexanies
  "NOTE that the intersection count is done by common degrees and not common sets,
  because some notes/ratios in the parent cps are produced by more than one of it's sets"
  [hexanies]
  (set (mapcat
        (fn [ref-hexany]
          (let [hex-sets (set (:sets ref-hexany))]
            (reduce (fn [acc other-hexany]
                      (let [other-sets (set (:sets other-hexany))
                            sets-x (set/intersection hex-sets other-sets)
                            degrees-x (set/intersection (set (:degrees ref-hexany))
                                                        (set (:degrees other-hexany)))
                            sorted-degrees (sort degrees-x)]
                        (cond
                          (= ref-hexany other-hexany) acc
                          (empty? degrees-x) acc
                          :else (conj acc {:intersection/pair #{(:id ref-hexany)
                                                                (:id other-hexany)}
                                           :intersection/sets sets-x
                                           :intersection/count (count degrees-x)
                                           :degrees/parent degrees-x
                                           :degrees/local {(:id ref-hexany) (mapcat #(indexes-of % (:degrees ref-hexany)) sorted-degrees)
                                                           (:id other-hexany) (mapcat #(indexes-of % (:degrees other-hexany)) sorted-degrees)}}))))
                    #{} hexanies)))
        hexanies)))

;; Step 4
(def diatonic-hexanies-intersection-data
  "See the NOTE in `intersect-diatonic-hexanies` fn"
  (intersect-diatonic-hexanies named-diatonic-hexanies))

(defn make-intersection-map [intersection-data]
  (reduce (fn [acc data]
            (let [[a b] (into [] (:intersection/pair data))]
              (-> acc
                  (assoc-in [a b] data)
                  (assoc-in [b a] data))))
          {}
          intersection-data))

(def diatonic-hexanies-intersection-map
  (make-intersection-map diatonic-hexanies-intersection-data))

;; Step 3

(def diatonic-hexanies-intersection-summary
  (->> diatonic-hexanies-intersection-map
       (map (fn [[hx-id hx-x-data]]
              [hx-id
               (->> hx-x-data
                    (map (fn [[other-hx-id data]]
                           [other-hx-id {:count (:intersection/count data)}]))
                    (sort-by (juxt (comp #(* -1 %) (comp :count second)) first))
                    (into []))]))
       (into {})))
