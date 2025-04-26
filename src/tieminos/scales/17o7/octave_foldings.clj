(ns tieminos.scales.17o7.octave-foldings
  (:require
   [erv.utils.ratios :refer [ratios->scale]]
   [erv.utils.scale :refer [dedupe-scale]]
   [tieminos.osc.surge :as surge]))

(comment
  ;; WIP
  ;; The original 17o7 scale can be conceived as two pentatonic scales a fifth appart
  [17/14 4/3 3/2 34/21 2]
  [68/63 4/3 34/21 16/9 2N]

  ;; They could be merged like this:
  (->> (concat
         ;; Of course this pentatonics could be rotated to make other versions of the scale
        (map #(/ % 1) [17/14 4/3 3/2 34/21 2])
        (map #(/ % 1) [68/63 4/3 34/21 16/9 2N]))
       ratios->scale
       dedupe-scale
       (map :ratio))

  (surge/set-scale
   {:scale {:meta {:scl/name "second child of 17/7"
                   :scl/description "An experiment"}
            :scale  (dedupe-scale (ratios->scale (concat
                                                  [17/14 4/3 3/2 34/21 2]
                                                  (map #(/ % 16/9) [68/63 4/3 34/21 16/9 2N]))))}
    :scale-name "dev/17o7-penta1"}))
