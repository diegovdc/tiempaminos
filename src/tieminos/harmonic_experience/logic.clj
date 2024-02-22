(ns tieminos.harmonic-experience.logic
  (:require
   [erv.scale.core :refer [deg->freq]]
   [erv.utils.conversions :refer [midi->cps]]
   [erv.utils.core :refer [period-reduce]]))

;; What's necessary for a good visualization
;; 1. Easily load any scale
;; 2. Easily map midi notes to frequencies
;; 3. Easily map midi notes to ratios on the lattice
;; 3.1 If there is duplicate note being played and one key goes up, then the lattice should still show it
;; 4. (Bonus) Allow for the pedal effect in something like Pianoteq
;;
;;

(def ratios->scale
  (memoize (fn [ratios period]
             (->> ratios
                  ;; ensure something copied or read from a scala file will work fine i.e. 2/1 will be converted to 1
                  (map #(period-reduce period %))
                  (sort)
                  (map (fn [r] {:bounded-ratio r :bounding-period period}))))))

(defn deg->freq* [{:keys [ratios period root-freq]
                   :or {period 2
                        root-freq (midi->cps 48)}}
                  degree]
  (let [scale (ratios->scale ratios period)]
    (deg->freq scale root-freq degree)))

(comment
  (deg->freq* {:ratios [21/20 9/8 7/6 5/4 4/3 7/5 3/2 14/9 5/3 7/4 15/8 2/1]}
              0)
  (map
   #(deg->freq* {:ratios [21/20 9/8 7/6 5/4 4/3 7/5 3/2 14/9 5/3 7/4 15/8 2/1]}
                %)
   [-2 -1 0 1 2 12 24]))

;; impl 2.
;; TODO left here
