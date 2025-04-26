(ns tieminos.compositions.7D-percusion-ensamble.dreams.dream-1-technical-analysis
  "Analysis of a few aspects of the isorhythmic processes"

  (:require
   [erv.utils.core :refer [interval]]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [deg->freq]]
   [tieminos.utils :refer [wrap-at]]))

(defn make-at-i [index]
  (fn at-i [col]
    (wrap-at index col)))

(defn m1
  [at-i] (at-i [0 2 0 (at-i [2 3]) #_#_#_(at-i [7 4]) (at-i [4 5]) 3]))

(def seq1 (map
           (fn [i]
             (m1 (make-at-i i)))
           (range 20)))
;; To recall, this 4)7 CPS was made by using the factors of 7 different hexanies. These hexanies have the property of being close to the 6 of the 7 notes of 12EDO diatonic. So they are diatonique-esque but are all missing some note of the diatonic structure. Each of this 7 hexanies appears 3 times in the 4)7 CPS. For example, the 1.3.9.19 hexany appears as 7.15-1.3.9.19, 7.21-1.3.9.19 and 15.21-1.3.9.19 (the numbers to the left of the "-" are the common factors to all of the hexanies notes).
;; In this piece I am considering a melody to be a sequence of degrees (independent of the scale). For example one of the earliest melodic patterns is [0, 2, 0, 3], this pattern is then used on
(-> seq1)

(map (fn [deg1 deg2]
       (let [n2 (if (not= :silence deg2)
                  (deg->freq :base-freq 1 :scale 2 :degree deg2)
                  :silence)
             chord [(deg->freq :base-freq 1 :scale 0 :degree deg1)
                    n2]]
         {:deg deg1
          :hex0-hex2 chord
          :interval (when (not= :silence deg2) (apply interval chord))}))
     (map
      (fn [i]
        (m1 (make-at-i i)))
      (range 20))
     (map
      (fn [i]
        (if (#{0 3} (mod i 5))
          (m1 (make-at-i i))
          :silence))
      (range 20)))
