(ns tieminos.polydori.analysis.dorian-hexanies
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.cps.core :as cps]
   [erv.cps.similarity :as cpss]
   [tieminos.piraran.dorian-scales :as ds]
   [tieminos.polydori.scale
    :refer
    [+cents polydori polydori-set->deg polydori-v2]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Finding dorian scales
;;;;;;;;;;;;;;;;;;;;;;;;;;


(do
  (defn all-rotations
    "Zero based rotations. `scale` is in cents"
    [scale]
    (map (fn [note] (sort (map #(mod (- % note) 1200) scale))) scale))

  (def dorico-six-notes
    (->> (combo/combinations [0 200 300 500 700 900 1000] 6)
         (mapcat all-rotations)
         #_(filter #(and ((set %) 300)))
         set)))

(all-rotations [0 201 404 498 903 9962])
(-> ds/dorico-1v1)
(comment
  ;; cps-sorted-by-euclidean-distance
  (-> (range 1 31 2)
      (combo/combinations 4)
      (->> (pmap #(->> (cps/make 2 %)
                       (cpss/+gens %)
                       cpss/+cents
                       cpss/+euclidean-distance))
           (sort-by :euclidean-distance)
           (filter #(dorico-six-notes
                     (:closest-12-edo %)))
           (sort-by :euclidean-distance)
           (group-by #(->> (:factors %)
                           set
                           (set/intersection #{1 3 9 19})
                           count))
           (#(dissoc % 0))
           (map (fn [[k v]]
                  [k (map (juxt :euclidean-distance :factors :cents) v)])))))

(def dorian-hexanies
  (-> (combo/combinations [1 3 9 19 15 21 7] 4)
      (->> (pmap #(->> (cps/make 2 %)
                       (cpss/+gens %)
                       cpss/+cents
                       cpss/+euclidean-distance))
           (sort-by :euclidean-distance)
           ;; can use filter to look for scales that fit dorian
           (filter #(dorico-six-notes
                     (:closest-12-edo %)))
           (sort-by :euclidean-distance)
           (map (juxt :euclidean-distance :factors :cents)))
      #_count))

(def dorian-hexanies-in-polydori
  "A vector of maps each with the hexany `:sets` and the `:degrees` of the notes in `polydori-v2`
  NOTE: do use `polydori-v2` as it's degrees differ from `polydori`"
  (mapcat (fn [i [_ factors]]
            (let [f-set (set factors)
                  poly-factors #{1 3 9 19 15 21 7}
                  factors-not-on-f-set (set/difference poly-factors f-set)
                  hexany (cps/make 2 factors)
                  hexany-note-factors (->> hexany
                                           :scale
                                           (map :set))]
              (->> (combo/combinations factors-not-on-f-set 2)
                   (map (fn [j common-factors]
                          (let [sets (map #(into % common-factors)
                                          hexany-note-factors)]
                            {:name (format "diat%sv%s" i (inc j))
                             :hexany (:scale hexany)
                             :hexany-note-factors hexany-note-factors
                             :unique-factors f-set
                             :common-factors (set common-factors)
                             :sets sets
                             :degrees (map polydori-set->deg sets)}))
                        (range)))))
          (range)
          dorian-hexanies))

(def dorian-hexanies-in-polydori-by-name
  (reduce (fn [m hx] (assoc m (:name hx) hx))
          {}
          dorian-hexanies-in-polydori))

(->> dorian-hexanies)
(->> dorian-hexanies-in-polydori (map (juxt :name (comp sort :degrees))))
(polydori-set->deg #{1 15 21 9})
(-> dorian-hexanies-in-polydori
    (nth 2))
(-> (:scale polydori-v2)
    (->> (filter #(-> % :sets (contains? #{1 15 21 9}))))
    #_(nth 22))

(-> dorian-hexanies)

(:scale (cps/make 4 [1 3 9 19 15 21 7]))
(->> (cps/make 4 [1 3 9 19 15 21 7])
     cps/+all-subcps
     :subcps)

(-> (cps/make 4 [1 3 9 19 15 21 7] :norm-fac 315 #_(* 49/3 513))
    cps/+all-subcps
    :subcps
    ;; keys
    (select-keys '("2)4 of 4)7 1.15-3.7.19.21"
                   "2)4 of 4)7 9.15-3.7.19.21"
                   "2)4 of 4)7 1.9-3.7.19.21"))
    seq
    (nth 0)
    second
    :scale
    +cents)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; completing dorian modes from hexanies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dorian cents: [0 200 300 500 700 900 1000]
;; use this to find the dorian mode


(-> polydori :subcps (get "2)4 of 4)7 1.15-3.7.19.21") :scale +cents)
(def anti-dorico-1v1
  (let [dorico (set (map :set ds/dorico-1v1))]
    (->> polydori (remove #(dorico (:set %))))))

(def dorico-1v2-interdeks
  "Dekanies grouped by common tones with dorico-1v2"
  (let [dorico (set (map :set ds/dorico-1v2))
        dekany-keys (->> polydori :subcps keys
                         (filter #(or (str/includes? % "2)5")
                                      (str/includes? % "3)5"))))
        dekanies  (-> polydori :subcps (select-keys dekany-keys)
                      (->> (map #(assoc-in
                                  (val %)
                                  [:meta :subcps/name]
                                  (key %)))))]
    (->> dekanies
         (group-by #(count (set/intersection
                            dorico
                            (set (map :set (:scale %))))))
         #_(sort-by first))))
(-> dorico-1v2-interdeks)
(-> ds/dorico-1v2)


;;;;;;;;;;;;;;;;;;
;;;; Harmonic Form
;;;;;;;;;;;;;;;;;

;; [[intersection-count total-dekanies]]


(->> dorico-1v2-interdeks
     (sort-by first)
     (map (juxt first (comp count second))))
;; NOTE moments from intedeks: '([0 14] [1 16] [2 15] [3 13] [4 2] [5 3])

(->> dorian-hexanies)
'([6.782329983125268 (1 3 9 19) (0 201 404 498 903 996)]
  [19.77371993328519 (1 3 9 15) (0 204 387 591 702 1089)]
  [42.790185790669334 (3 19 21 7) (0 297 529 702 796 969)]
  [44.66542286825459 (9 19 21 7) (0 94 267 529 702 795)]
  [46.22769732530488 (1 9 21 7) (0 232 267 498 702 996)]
  [46.593991028886975 (1 3 19 21) (0 325 498 729 795 1026)]
  [55.072679252057455 (1 3 9 7) (0 232 498 730 933 996)])

(defn uniq-factors [scale-data]
  (with-meta
    (-> scale-data :meta :subcps/name
        (str/split #"-")
        second
        (str/split #"\.")
        (->> (mapv read-string)))
    {:scale-data scale-data}))

(defn get-subcps-name [dek]
  (-> dek meta :scale-data :meta :subcps/name))

(defn dorian-hexanies-in-dekany [dekany-factors]
  (let [hex-factors (->> dorian-hexanies (mapv second))]
    {:dek dekany-factors
     :hexs (->> hex-factors
                (filter #(set/subset? (set %) (set dekany-factors)))
                (mapv (comp (partial into []) sort)))
     :name (get-subcps-name dekany-factors)}))
;; momento 5

(-> dorico-1v2-interdeks (get 5)
    (->> (map uniq-factors)
         (map dorian-hexanies-in-dekany)))

;; momento 4
(-> dorico-1v2-interdeks (get 4)
    (->> (map uniq-factors)
         (map dorian-hexanies-in-dekany)))

;; momento 3
(-> dorico-1v2-interdeks (get 3)
    (->> (map uniq-factors)
         (map dorian-hexanies-in-dekany)))

(-> dorico-1v2-interdeks (get 2)
    (->> (map uniq-factors)
         (map dorian-hexanies-in-dekany)))

(-> dorico-1v2-interdeks (get 1)
    (->> (map uniq-factors)
         (map dorian-hexanies-in-dekany)))
(-> polydori :subcps (get "3)5 of 4)7 9-3.7.15.19.21") :scale)

(->> (cps/make 3  [3 7 15 19 21]))


;;;
;;;
;;;looking for where to transpose the scale so that one of the diatonic scales lands as close to 12edo as possible


(->> dorian-hexanies-in-polydori)
;;
;; diatonic 1
(->> polydori-v2
     :scale
     (filter (fn [n] (seq (set/intersection (:sets n)
                                            (-> dorian-hexanies-in-polydori
                                                (nth 0)
                                                :sets
                                                set))))))
;;=> 2 3 5 7 9 10 and we add the first degree to get a diatonic scale

;; diatonic 2
(->> polydori-v2
     :scale
     (filter (fn [n]
               (seq (set/intersection (:sets n)
                                      (-> dorian-hexanies-in-polydori
                                          (nth 2)
                                          :sets
                                          set))))))
;;=> 0 2 4 5 9 10
;;
;; We can join both of the above to get a dorian or a mixolydian

;;
;; It is also possible to get a decent tritone (9 cents off) and a mayor 7th (7 cents off)
(->> polydori-v2
     :scale
     (map (juxt :sets :cents)))

[[#{#{7 1 19 9} #{1 21 3 19}} 609.7762844043901]
 [#{#{7 1 3 19}} 1107.8212835390027]]
(-> dorian-hexanies-in-polydori
    (nth 0)
    :sets
    set)

(def dorian-hex-connections
  (->>
    (for [source dorian-hexanies-in-polydori
          target dorian-hexanies-in-polydori]
      (let [intersection (set/intersection (set (:degrees source))
                                           (set (:degrees target)))]
        (cond
          (= source target) nil
          (zero? (count intersection)) nil
          :else {(:name source) {:name (:name target)
                                 :degrees intersection
                                 :size (count intersection)}}
          )))
    (remove nil?)
    (reduce (fn [acc m]
              (update acc (ffirst m)
                      (comp #(sort-by :size > %) conj)
(first (vals m)))) {})))


(comment
  (-> dorian-hex-connections
      (select-keys ["diat0v1"
                    "diat0v2"
                    "diat0v3"]))
  (-> dorian-hex-connections
      (select-keys ["diat0v1"
                    "diat1v3"
                    "diat0v3"]))
  )
