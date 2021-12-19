(ns tieminos.afable-diablo.harmonic-form
  (:require
   [tieminos.afable-diablo.dorian-scales :refer [anti-dorico-1v1]]
   [tieminos.afable-diablo.scale :refer [+cents polydori]]
   [clojure.string :as str]
   [erv.cps.core :as cps]))

(def harmonic-form
  "Momento 0:  antidórico diff ?
   Momento 1: (1 3 7 9 19) diff 1->15 . 3->21
   Momento 2: (7 9 15 19 21) diff 9->3
   Momento 3: (3 7 15 19 21) diff 3->1 . 15->9
   Momento 4: (1 7 9 19 21) diff  21->3
   Momento 5: (1 3 7 9 19) diff  (7)
   Momento 6: dorico-1v2 (7.15-1.3.9.19)"
  [{:momento 0
    :note "Contiene la tónica"
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

(def tunings-dir
  (let [dir-path (-> (str (ns-name *ns*)) (str/split #"\.")
                     (->> (drop-last 1) (str/join "/"))
                     (str/replace #"-" "_"))]
    (-> (java.io.File. (str "src/" dir-path "/sc-tunings"))
        .getAbsolutePath)))


(defn +sc-tun-note [scale]
  (map #(assoc % :sc/tun-note (/ (:cents %) 100))
       scale))
(do
  (defn get-scale [moment]
    (let [name* (moment :name)]
      [moment
       (if (moment :scale)
         (+sc-tun-note (moment :scale))
         (-> polydori :subcps (get name*) :scale +cents
             (->> +sc-tun-note)))]))

  (defn make-sc-tuning [tuning-name-fn [moment scale]]
    (let [fmt #(format "Tuning.new(#[%s], \"%s\");"
                       % (tuning-name-fn moment))]
      (->> (mapv :sc/tun-note scale)
           (str/join ", " )
           fmt)))

  (defn make-file [file-name sc-tunings]
    (let [data (format "(\n%s\n);" (str/join "\n" sc-tunings))]
      (spit (str tunings-dir "/" file-name) data)))

  (defn get-hexanies [moment]
    (let [subcps-names (->> polydori :subcps keys)]

      (->> (moment :hexs)
           (mapcat (fn [h]
                     (filter #(and (str/includes? % (str/join "." h))
                                   (str/includes? % "2)4"))
                             subcps-names)))
           set
           (map #(assoc moment
                        :scale (-> polydori :subcps (get %) :scale +cents)
                        :hex/name (->  % (str/split #" ") last))))))

  (defn dek-tun-name [moment]
    (str "DekMoment " (moment :momento)))
  (defn hex-tun-name [moment]
    (format "HexMoment %s (%s)" (moment :momento) (moment :hex/name)))
  (def dekany-tunings
    (->> harmonic-form
         #_  (drop 1)
         (map get-scale)
         (map (partial make-sc-tuning dek-tun-name))
         (make-file "dekany-moments-tunings.scd")))
  (def hexany-tunings
    (->> harmonic-form
         (drop 1)
         (mapcat get-hexanies)
         (map get-scale)
         (map (partial make-sc-tuning hex-tun-name))
         (make-file "hexany-moments-tunings.scd")))
  #_dekany-tunings
  hexany-tunings)
