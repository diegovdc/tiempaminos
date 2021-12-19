(ns tieminos.afable-diablo.harmonic-form
  (:require
   [tieminos.afable-diablo.scale :refer [+cents polydori]]
   [tieminos.afable-diablo.dorian-scales :refer [anti-dorico-1v1]]))

(def harmonic-form
  "Momento 0:  antidórico diff ?
   Momento 1: (1 3 7 9 19) diff 1->15 . 3->21
   Momento 2: (7 9 15 19 21) diff 9->3
   Momento 3: (3 7 15 19 21) diff 3->1 . 15->9
   Momento 4: (1 7 9 19 21) diff  21->3
   Momento 5: (1 3 7 9 19) diff  (7)
   Momento 6: dorico-1v2 (7.15-1.3.9.19)"
  [{:momento 0
    :scale anti-dorico-1v1
    :antidorico :todo-lo-demas}
   {:momento 1
    :dek [1 3 7 9 19],
    :hexs [[1 3 9 19] [1 3 7 9]],
    :name "2)5 of 4)7 15.21-1.3.7.9.19"}
   {:momento 2
    :dek [7 9 15 19 21]
    :hexs [[7 9 19 21]]
    :name "3)5 of 4)7 1-7.9.15.19.21"}
   {:momento 3
    :dek [3 7 15 19 21]
    :hexs [[3 7 19 21]]
    :name "3)5 of 4)7 9-3.7.15.19.21"}
   {:momento 4
    :dek [1 7 9 19 21],
    :hexs [[7 9 19 21] [1 7 9 21]],
    :name "3)5 of 4)7 15-1.7.9.19.21"}
   {:momento 5
    :dek [1 3 7 9 19],
    :hexs [[1 3 9 19] [1 3 7 9]],
    :name "3)5 of 4)7 15-1.3.7.9.19"}])

(defn +sc-tun-note [scale]
  (map #(assoc % :sc/tun-note (/ (:cents %) 100))
       scale))
(do
  (defn get-scale [moment]
    (let [name* (moment :name)]
      (if name*
        (-> polydori :subcps (get name*) :scale +cents
            (->> +sc-tun-note))))
    )
  (->> harmonic-form
       (drop 1)
       (map get-scale)))
