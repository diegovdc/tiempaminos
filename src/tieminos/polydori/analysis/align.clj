(ns tieminos.polydori.analysis.align
  "Analysis of the harmonies of the piece Align, or Alignment or 19Sept2023.

  All hexanies used in that piece, from polydori have the root at 12ET-D.

  What I want to know is how many different pitches are sound along the piece,
  or more precisely in the most dense moment...
  because the piece actually sounds very consonant."
  (:require
   [erv.utils.conversions :as conv]
   [tieminos.polydori.analysis.dorian-hexanies :refer [dorian-hexanies-in-polydori]]
   [tieminos.polydori.scale :refer [polydori-v2]]))

(defn get-note [deg] (-> polydori-v2 :scale (nth deg)))
(defn get-bounded-ratio [deg] (map (comp :bounded-ratio get-note) deg))
(defn root-scale [ratios]
  (let [lowest (first ratios)]
    (map #(/ % lowest) ratios)))

#_:clj-kondo/ignore
(defn ratio-cent-pairs
  [ratios]
  (map (fn [r] [r (conv/ratio->cents r)]) ratios))

(comment
  (def rooted-hexanies
    (->> dorian-hexanies-in-polydori
         (filter (comp #{"diat2v2"
                         "diat3v2"
                         "diat5v2"
                         "diat4v2"
                         "diat6v2"}
                       :name))
         (map (juxt :name
                    (comp
                     #_ratio-cent-pairs
                     root-scale
                     get-bounded-ratio
                     sort
                     :degrees)))))
  (-> rooted-hexanies)
  (->> rooted-hexanies
       (mapcat second)
       set
       sort
       ratio-cent-pairs))
