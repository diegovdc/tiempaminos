(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-2.space
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

(def octa-low
  (subgraph main-graph [1 2 3 4 5 6 7 8]))

(def hepta-mid
  (seq->bigraph [10 11 12 13 14 15 16]))

(def octa-top
  (subgraph main-graph [17 18 19 20 21 22 23 24]))

(def tri6-1
  (subgraph main-graph (map dec [17 11 3 13 21 19])))
