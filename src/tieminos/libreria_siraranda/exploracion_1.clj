(ns tieminos.libreria-siraranda.exploracion-1
  (:require [clojure.string :as str]
            [erv.cps.core :as cps]
            [erv.utils.core :refer [period-reduce]]))

(def limit-13
  [13/12
   11/10
   9/8
   11/9
   4/3
   11/8
   13/9
   3/2
   11/7
   13/8
   16/9
   11/6
   13/7
   2/1])

(def limit-13-v2
  [135/128
   13/12
   11/10
   9/8
   13/11
   11/9
   13/10
   4/3
   11/8
   13/9
   3/2
   11/7
   13/8
   16/9
   11/6
   13/7
   2/1])

(def limit-7
  [135/128
   13/12
   9/8
   7/6
   6/5
   5/4
   9/7
   4/3
   7/5
   3/2
   8/5
   5/3
   7/4
   9/5
   13/7
   2/1])

(comment

  (->> limit-13-v2
       (mapcat (fn [x] [x (period-reduce 2 (* x 11/8))]))
       set sort
       (str/join "\n")
       ;; println
       )

  (->> limit-7
       #_(mapcat (fn [x] [x (period-reduce 2 (* x 3/2))]))
       set sort
       (str/join "\n")
       println)

  (- (/ 13/8 11/9) 4/3))

(count [135/128
        13/12
        9/8
        7/6
        6/5
        5/4
        9/7
        4/3
        7/5
        3/2
        8/5
        5/3
        7/4
        9/5
        13/7
        2/1])

(comment
  (count limit-13-v2)
  (println
   (str/join "\n"
             limit-13-v2))
  (println
   (str/join "\n"
             (sort [7/6
                    13/7
                    7/5
                    3/2
                    7/4
                    13/12
                    135/128
                    9/7])))

  (->>
   (cps/make 2 [1 9 7 11 13] :norm-fac 7)
   :scale
   (map :bounded-ratio)
   (str/join "\n")
   println))

