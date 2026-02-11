(ns tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.nil-space
  "Graph for the channel distribution of the NIL (Networed Imagination Laboratory)"
  (:require
   [overtone.core :as o]
   [tieminos.seq-utils.utils :refer [bigraph seq->graph subgraph]]))

(defn- zero-based-graph
  "Convert outputs to 0 by using `dec`"
  [graph]
  (map (fn [[k xs]] [(dec k) (set (mapv dec xs))])
       graph))

(def main-graph
  (->> {1 [9 10 17 11 3 2]
        2 [1 4 9 10 12 18]
        3 [1 4 5 11 19 13]
        4 [2 3 6 12 14 20]
        5 [3 6 7 13 15 21]
        6 [4 5 8 14 16 22]
        7 [5 8 15 23]
        8 [6 7 16 24]
        9 [1 2 10 17 18]
        10 [1 2 9 17 18]
        11 [1 3 17 19 12]
        12 [2 4 11 18 20]
        13 [3 5 14 19 21]
        14 [4 6 20 22 13]
        15 [5 7 21 23 16]
        16 [6 8 22 24 15]
        17 [1 9 10 11 18 19]
        18 [2 9 10 12 17 20]
        19 [3 11 13 17 20 21]
        20 [4 12 14 18 19 22]
        21 [5 13 15 19 22 23]
        22 [6 14 16 20 21 24]
        23 [7 15 21 24]
        24 [8 16 22 23]}
       zero-based-graph
       (into {})
       (bigraph)))

(def main-graph-2d
  (->> {1 [9 10 17 11 3]
        2 [4 9 10 12 18]
        3 [1 5 11 19 13]
        4 [2 6 12 14 20]
        5 [3 7 13 15 21]
        6 [4 8 14 16 22]
        7 [5 15 23]
        8 [6 16 24]
        9 [1 2 10 17 18]
        10 [1 2 9 17 18]
        11 [1 3 17 19]
        12 [2 4 18 20]
        13 [3 5 19 21]
        14 [4 6 20 22]
        15 [5 7 21 23]
        16 [6 8 22 24]
        17 [1 9 10 11 19]
        18 [2 9 10 12 20]
        19 [3 11 13 17 21]
        20 [4 12 14 18 22]
        21 [5 13 15 19 23]
        22 [6 14 16 20 24]
        23 [7 15 21]
        24 [8 16 22]}
       zero-based-graph
       (into {})
       (bigraph)))

(def right-wall
  (->> main-graph-2d
       (filter (fn [[k]] (odd? k)))
       (map (fn [[k v]] [k (set (filter odd? v))]))
       (into {})))

(def left-wall
  (->> main-graph-2d
       (filter (fn [[k]] (even? k)))
       (map (fn [[k v]] [k (set (filter even? v))]))
       (into {})))
(comment
  (-> left-wall*)
  (-> right-wall))

(defn seq->bigraph
  [xs]
  (->> xs
       seq->graph
       zero-based-graph
       (into {})
       (bigraph)))

(def cube-front
  (seq->bigraph [1 2 3 4 17 18 19 20]))

(def cube-mid
  (seq->bigraph [3 4 19 20 5 6 21 22]))

(def cube-back
  (seq->bigraph [21 5 6 22 23 7 8 24]))

(def octa-low-seq [1 2 3 4 5 6 7 8])
(def octa-low (subgraph main-graph octa-low-seq))

(def hepta-mid-seq [9 11 12 13 14 15 16])
(def hepta-mid (seq->bigraph hepta-mid-seq))

(def octa-top-seq [17 18 19 20 21 22 23 24])
(def octa-top (subgraph main-graph octa-top-seq))

(def tri6-1
  (subgraph main-graph (map dec [17 11 3 13 21 19])))

(defn- make-tri
  [nodes]
  (subgraph main-graph nodes))

(def tri-11d-seq (map dec [17 19 11]))
(def tri-11d (make-tri tri-11d-seq))

(def tri-15d-seq (map dec [15 21 23]))
(def tri-15d (make-tri  tri-15d-seq))

(def tri-12d-seq (map dec [12 18 20]))
(def tri-12d (make-tri tri-12d-seq))

(def tri-16d-seq (map dec [16 22 24]))
(def tri-16d (make-tri tri-16d-seq))

(def spiral-left-up-seq
  (map dec [1 3 5 7 8 6 4 2
            9
            11 13 15 16 14 12
            10
            17 19 21 23 24 22 20 18]))

(def spiral-left-down-seq
  (reverse spiral-left-up-seq))

(def spiral-right-up-seq
  (map dec [2 4 6 8 7 5 3 1
            9
            12 14 16 15 13 11
            10
            18 20 22 24 23 21 19 17]))

(def spiral-right-down-seq
  (reverse spiral-right-up-seq))

(def spiral-up-down-1-seq
  (concat spiral-left-up-seq
          (->> spiral-left-down-seq
               (drop 1)
               (drop-last 1))))

(def spiral-up-down-1
  (subgraph main-graph spiral-up-down-1-seq))

(def spiral-up-down-2-seq
  (concat spiral-right-up-seq
          (->> spiral-right-down-seq
               (drop 1)
               (drop-last 1))))

(def spiral-up-down-2
  (subgraph main-graph spiral-up-down-2-seq))

(def up-l1 (map dec [3 11 17]))
(def up-l1b (map dec [3 13 17]))
(def up-l2 (map dec [5 13 19]))
(def up-l2b (map dec [5 15 19]))
(def up-l3 (map dec [7 15 21]))

(def up-r1 (map dec [4 12 18]))
(def up-r1b (map dec [4 14 20]))
(def up-r2 (map dec [6 14 20]))
(def up-r2b (map dec [6 16 20]))
(def up-r3 (map dec [8 16 22]))
