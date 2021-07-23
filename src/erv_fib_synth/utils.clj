(ns erv-fib-synth.utils
  (:require [erv.cps.core :as cps]
            [erv.scale.core :as scale]))


(defn period [seconds durs]
  (let [ratio (/ seconds (apply + durs))]
    (mapv #(* ratio %) durs)))

(defn periods [seconds & durs]
  (mapcat (partial period seconds) durs))


(defn gen-chord [scale fundamental [gens degs]]
  (let [scale** (->> scale (#(cps/filter-scale % gens)))]
    (map (partial scale/deg->freq scale** fundamental) degs)))


;; Range mapping https://github.com/supercollider/supercollider/blob/18c4aad363c49f29e866f884f5ac5bd35969d828/lang/LangSource/MiscInlineMath.h
(defn linexp
  "Maps a value from a linear range to an exponential range"
  [lin-min lin-max exp-min exp-max value]
  (cond
    (or (<= exp-min 0) (<= exp-max 0)) (throw (ex-info "Values for the exponential range must be larger than 0"
                                                       {:lin-min lin-min
                                                        :lin-max lin-max
                                                        :exp-min exp-min
                                                        :exp-max exp-max}))
    (<= value lin-min) exp-min
    (>= value lin-max) exp-max
    :else (* exp-min (Math/pow (/ exp-max exp-min)
                               (/ (- value lin-min)
                                  (- lin-max lin-min))))))
