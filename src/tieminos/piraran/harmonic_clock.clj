(ns tieminos.piraran.harmonic-clock
  (:require
   [erv.utils.core :refer [wrap-at]]
   [taoensso.timbre :as timbre]
   [tieminos.piraran.harmonic-form :as form]
   [tieminos.piraran.scale :refer [polydori]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain stop]]))

(defn +hexanies [moment]
  (assoc moment :hexanies
         (->> moment
              form/get-hexanies
              (map (comp :scale
                         (polydori :subcps)
                         (partial str "2)4 of 4)7 ")
                         :hex/name)))))

(def dek-seq
  (->> form/harmonic-form
       (map  form/get-scale)
       (map #(assoc (first %) :scale (second %)))
       (mapv +hexanies)))

(def momento (atom (first dek-seq)))

(defn get-scale
  "`0` returns the `moment`'s `:scale` (usually a dekany), `1` to `n` return
  a hexany or the `:scale` if there is no hexany."
  [i]
  (let [default-scale (:scale @momento)
        scale (cond (zero? i) default-scale
                    :else (wrap-at i (:hexanies @momento)))]
    (or scale default-scale)))

(comment
  ;; total different hexanies
  (->> dek-seq (mapcat :hexanies) set count))

(defn run [{:keys [moment-events]}]
  (ref-rain :id :harmonic-clock
            :durs (map :dur form/harmonic-form)
            :loop? false
            :on-event (on-event
                       (let [m (nth dek-seq index nil)
                             name* (cond (= 6 (:momento m)) "dórico"
                                         (= 0 (:momento m)) "anti"
                                         :else  (:name m))
                             event (moment-events (:momento m))]
                         (when m
                           (timbre/info
                            (format "Momento %s -  %s\nHexanies: %s\nAcción: %s\n\n"
                                    (:momento m) name*
                                    (count (:hexanies m))
                                    (:descripción m)))
                           (reset! momento m)
                           (when event (event)))))))

(comment
  (stop)
  (run))

