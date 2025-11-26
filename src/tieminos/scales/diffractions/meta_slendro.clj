(ns tieminos.scales.diffractions.meta-slendro
  "Diffractions of Grady-Wilson Meta-Slendro"
  (:require
   [tieminos.utils :refer [careful-merge]]))

(def no1
  [{:ratio 1, :bounded-ratio 1, :bounding-period 2}
   {:ratio 46/45, :bounded-ratio 46/45, :bounding-period 2}
   {:ratio 299/288, :bounded-ratio 299/288, :bounding-period 2}
   {:ratio 499/480, :bounded-ratio 499/480, :bounding-period 2}
   {:ratio 171/160, :bounded-ratio 171/160, :bounding-period 2}
   {:ratio 391/360, :bounded-ratio 391/360, :bounding-period 2}
   {:ratio 665/576, :bounded-ratio 665/576, :bounding-period 2}
   {:ratio 1117/960, :bounded-ratio 1117/960, :bounding-period 2}
   {:ratio 901/720, :bounded-ratio 901/720, :bounding-period 2}
   {:ratio 113/90, :bounded-ratio 113/90, :bounding-period 2}
   {:ratio 101/80, :bounded-ratio 101/80, :bounding-period 2}
   {:ratio 319/240, :bounded-ratio 319/240, :bounding-period 2}
   {:ratio 193/144, :bounded-ratio 193/144, :bounding-period 2}
   {:ratio 43/30, :bounded-ratio 43/30, :bounding-period 2}
   {:ratio 14/9, :bounded-ratio 14/9, :bounding-period 2}
   {:ratio 29/18, :bounded-ratio 29/18, :bounding-period 2}
   {:ratio 13/8, :bounded-ratio 13/8, :bounding-period 2}
   {:ratio 59/36, :bounded-ratio 59/36, :bounding-period 2}
   {:ratio 299/180, :bounded-ratio 299/180, :bounding-period 2}
   {:ratio 301/180, :bounded-ratio 301/180, :bounding-period 2}
   {:ratio 69/40, :bounded-ratio 69/40, :bounding-period 2}
   {:ratio 161/90, :bounded-ratio 161/90, :bounding-period 2}
   {:ratio 1303/720, :bounded-ratio 1303/720, :bounding-period 2}
   {:ratio 2659/1440, :bounded-ratio 2659/1440, :bounding-period 2}
   {:ratio 901/480, :bounded-ratio 901/480, :bounding-period 2}
   {:ratio 343/180, :bounded-ratio 343/180, :bounding-period 2}
   {:ratio 229/120, :bounded-ratio 229/120, :bounding-period 2}])

(def scales
  (->> (careful-merge
        {:no1 {:meta {:scl/name "diffracted-meta-slendro_no1.scl"
                      :scl/description "A scale derived from some of the beating harmonics in Grady-Wilson Meta-Slendro[12]. The ratios where calculated by the acoustincs formula for calculating the perceived pitch of a beating pair: (f1+f2)/2."}
               :scale no1}})

       (into {})))
