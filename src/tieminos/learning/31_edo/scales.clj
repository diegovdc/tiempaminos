(ns tieminos.learning.31-edo.scales
  (:require
   [clojure.string :as str]
   [erv.edo.core :as edo]
   [erv.utils.conversions :as conv]
   [erv.utils.core :refer [round2]]))

(->> (edo/from-pattern [5 1 5 1 5 1 5 1 5 1 1])
     :scale
     (map :bounded-ratio)
     (map conv/ratio->cents)
     (map #(* % 0.01))
     (map #(round2 3 %))
     (str/join ","))

