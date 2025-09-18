(ns tieminos.scales.meru.fib
  (:require
   [erv.meru.core :as meru]
   [erv.utils.ratios :refer [ratios->scale]]
   [tieminos.osc.surge :as surge]
   [erv.mos.v3.core :as mos]))
(do
  (def fib
    (->> {:seed [1 1 2]
          :formula :fibonacci}
         (meru/recurrent-series)
         :series
         (drop 5)
         (take 36)
         (ratios->scale)))

  #_(surge/set-scale
     {:scale {:scale fib}
      :scale-name (:scl/name (:meta (format "dev/fib[%s]" (count fib))))}))

(comment
  (require '[clojure.string :as str])
  (-> fib)
  (->> (mos/gen->mos-ratios (rationalize 1.618) 2 100)

       (map (juxt (comp :size :meta)
                  (comp :mos/sL-ratio.float :meta))))
  (surge/init)
  (surge/set-scale
   {:scale {:scale fib}
    :scale-name (format "dev/fib[%s]" (count fib))})

  (surge/set-kbm
   (let [degrees (->> [0 6 12 16 22 26 31]
                      (mapcat (fn [%] [(mod (+ 5 %) (count fib))
                                       #_(mod (+ 9 %) (count fib))]))
                      set
                      sort)]
     {:kbm-name (format "dev/fib[%s]_%s" (count fib) (str/join "-" degrees))
      :scale-data {:scale fib}
      :degrees degrees})))
