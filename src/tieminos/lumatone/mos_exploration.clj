(ns tieminos.lumatone.mos-exploration
  (:require
   [erv.constant-structures.core :as cs]
   [erv.edo.core :as edo]
   [erv.mos.mos :as mos]
   [erv.utils.core :refer [rotate]]))

(def meru
  [1.61803
   1.46557
   1.32471
   1.38027
   1.22074
   1.32471
   1.23650
   1.19385
   1.16730
   1.28519
   1.13472
   1.25542
   1.19071
   1.15855
   1.13818
   1.12373
   1.11277
   1.23205
   1.14613
   1.11479
   1.09698
   1.21314
   1.16185
   1.19749
   1.13588
   1.08593
   1.07576])

(comment

  (->> meru
       (map (fn [meru] [meru (/ 311 meru)]))
       (filter (fn [[_ x]] (or
                            (<= (- x (int x))
                                0.2)
                            (>= (- x (int x))
                                0.8)))))
       ;; result

  [[1.61803 19.159100881936673]
   [1.46557 21.15217969800146]
   [1.2365 25.07076425394258]
   [1.19385 25.966411190685594]
   [1.28519 24.12094709731635]
   [1.19071 26.034886748242645]
   [1.11277 27.85840739775515]
   [1.23205 25.16131650501197]
   [1.14613 27.04754259987959]
   [1.11479 27.80792795055571]
   [1.19749 25.887481315084052]
   [1.07576 28.816836469100913]])

(map count (mos/make 311 241)) ; 22 31
(map count (mos/make 311 274))
(map count (mos/make 311 261)) ; 31
(map count (mos/make 311 278))
(map count (mos/make 311 289))

(filter #(= 31 (count %)) (mos/make 311 241))

(->> (mos/make 31 (- 31 20)) (drop 2) (take 5))
(->> (mos/make 31 (- 31 26)) (drop 5) (take 4))
(->> (mos/make 31 (- 31 27)) (drop 5) (take 4))
(->> (mos/make 31 (- 31 21)) (drop 2) (take 4))
(->> (mos/make 31 11) (drop 2) (take 5))
(->> (mos/make 31 (- 31 24)) (drop 3) (take 5))
(->> (mos/make 31 (- 31 26)) (drop 5) (take 4))
(->> (mos/make 31 (- 31 28)) (drop 8) (take 4))
(->> (mos/make 31 (- 31 29)) (drop 10) (take 4))

(mos/make 31 6)
;; a marwa of mothra [4 1 1 4 1 1 4 1 1 4 1 1 4 1 1 1]]
(mapv #(apply + %) [[1 4] [1] [1 3] [2] [2 2] [2] [2 2] [2] [2 2] [2] [1]])
(mapv #(apply + %) [[5 1] [4 2] [4 2] [4 2] [4 2] [1]])
(mapv #(apply + %) [6 6 6 6 6 1])

(vec (flatten [[5 1] [4 2] [4 2] [4 2] [4 2] [1]]))

[6 6 6 6 7]
[6 6 6 6 6 1]
[5 1 4 2 4 2 4 2 4 2 1]
[1 4 1 1 3 2 2 2 2 2 2 2 2 2 2 1]

(defn apply-sum-pattern
  [scale pattern]
  (loop [pattern pattern
         scale scale
         new-scale []]
    (if-not (seq pattern)
      new-scale
      (let [size (first pattern)]
        (recur (next pattern)
               (drop size scale)
               (conj new-scale (apply + (take size scale))))))))

(reduce
 (fn [acc pattern]
   (conj acc
         (apply-sum-pattern
          pattern
          (last acc))))
 [#_[1 4 1 1 3 2 2 2 2 2 2 2 2 2 2 1]
  #_[4 1 1 4 1 1 4 1 1 4 2 2 2 1 1 1]
  #_[1 1 4 1 1 5 1 1 4 1 1 1 4 1 1 3]
  #_(rotate [4 2 2 2 2 2 2 1 1 4 1 1 4 1 1 1] 1)
  #_(rotate [1 4 1 1 4 1 1 3 1 1 4 2 2 2 2 1] #_-4 -7)
  #_(rotate [1 1 3 1 1 3 1 1 4 2 2 2 4 1 1 3] #_0 6)
  (rotate [2 2 2 2 2 1 1 6 1 1 2 2 2 2 1 2] #_10 0)]
 [[2 1 2 1 2 1 2 1 2 1 1]
  [2 2 2 2 2 1]
  [1 1 1 1 2]
  [1 1 1 2]
  [1 1 2]
  [1 2]
  [2]])
(apply-sum-pattern
 [2 1 2 1 2 1 2 1 2 1 1]
 [1 4 1 1 3 2 2 2 2 2 2 2 2 2 2 1])
(edo/from-pattern [6,6,6,6,6,1])
(map
 #(:constant-structure?
   (cs/analyze (:scale (edo/from-pattern %))))
 ['(11 6 1 6 6 1)
  '(6 11 6 1 6 1)
  [14, 8, 8, 15, 8, 8, 9, 14, 8, 8, 15, 8, 8, 15, 8, 8, 8, 15, 8, 8, 15, 8, 8, 8, 15, 8, 8, 15, 8, 8, 9]])

(:constant-structure?
 (cs/analyze (map (fn [x] {:bounded-ratio x})
                  ;; semion 11
                  [4394/4335
                   17/15
                   338/289
                   17/13
                   338/255
                   289/195
                   26/17
                   289/169
                   26/15
                   4913/2535
                   2/1])))

(cs/analyze (map (fn [x] {:bounded-ratio (/ x 11)})
                 (range 12 23)))
