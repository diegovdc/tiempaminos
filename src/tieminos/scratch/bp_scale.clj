(ns tieminos.scratch.bp-scale
  (:require
   [clojure.string :as str]))

(println
  (str/join "\n"
            (sort [25/9
                   25/21
                   75/49
                   5/3
                   15/7
                   7/3
                   9/7
                   7/5
                   9/5
                   27/25 63/25
                   49/25
                   3/1])))
