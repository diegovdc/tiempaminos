(ns tieminos.piraran.harmonic-form
  (:require
   [clojure.string :as str]
   [erv.scale.scl :refer [make-scl-file]]
   [tieminos.piraran.dorian-scales :refer [anti-dorico-1v1 dorico-1v2]]
   [tieminos.piraran.scale :refer [+cents polydori polydori-by-sets]]))

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
    :descripción "Alex y Diego"
    :dur 60
    :scale anti-dorico-1v1
    :act-seq [[5 0.1] [5 0.2] [10 0.3] [20 0.4] [10 0.5] [10 0.6]]
    :antidorico :todo-lo-demas}
   {:momento 1
    :descripción "Iván soleando"
    :dur 90
    :dek [1 3 7 9 19],
    :hexs [[1 3 9 19] [1 3 7 9]],
    :act-seq [[10 0.1] [10 0.3] [25 0.45] [20 0.6] [25 0.7] [20 0.5]]
    :name "2)5 of 4)7 15.21-1.3.7.9.19"}
   {:momento 2
    :descripción "Iván soleando"
    :dur 90
    :dek [7 9 15 19 21]
    :hexs [[7 9 19 21]]
    :act-seq [[25 0.45] [20 0.6] [25 0.7] [20 0.3]]
    :name "3)5 of 4)7 1-7.9.15.19.21"}
   {:momento 3
    :descripción "Solo de Diego"
    :dur 120
    :dek [3 7 15 19 21]
    :hexs [[3 7 19 21]]
    :act-seq [[30 0.6] [20 0.3] [10 0.5] [30 0.7] [10 0.4] [5 0.2] [5 0.4] [20 0.8]]
    :name "3)5 of 4)7 9-3.7.15.19.21"}
   {:momento 4
    :descripción "Tutti con exabruptos"
    :dur 90
    :dek [1 7 9 19 21],
    :hexs [[7 9 19 21] [1 7 9 21]],
    :act-seq [[5 1] [7 0] [3 0.7] [5 0] [2 0.3] [2 0.8] [5 0.1] [2 0]]
    :name "3)5 of 4)7 15-1.7.9.19.21"}
   {:momento 5
    :descripción "Tutti con exabruptos"
    :dur 60
    :dek [1 3 7 9 19],
    :hexs [[1 3 9 19] [1 3 7 9]],
    :act-seq [[3 1] [7 0] [3 0.7] [7 0] [2 0.3] [4 0.8] [5 0.1] [2 0]]
    :name "3)5 of 4)7 15-1.3.7.9.19"}
   {:momento 6
    :descripción "Tutti con exabruptos"
    :dur 90
    :act-seq [[3 1] [7 0] [3 0.7] [7 0] [2 0.3] [4 0.8] [4 0.1] [10 0.3] [10 0.5] [10 0.7] [10 0.5] [10 0.3] [10 0.2] [20 0]]
    :note "Escala de arribo"
    :scale dorico-1v2
    :dorico :el-complemento-de-antidorico}
   {:momento 7
    :descripción "Silencio"
    :dur 90
    :act-seq [[90 0]]
    :scale dorico-1v2
    :dorico :el-complemento-de-antidorico}])

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write tunings files
;;;;;;;;;;;;;;;;;;;;;;;;

(def tunings-dir
  (let [dir-path (-> (str (ns-name *ns*)) (str/split #"\.")
                     (->> (drop-last 1) (str/join "/"))
                     (str/replace #"-" "_"))]
    (-> (java.io.File. (str "src/" dir-path "/sc-tunings"))
        .getAbsolutePath)))

(defn +sc-tun-note [scale]
  (map #(assoc % :sc/tun-note (/ (:cents %) 100)
               :degree (:degree (first (polydori-by-sets (:set %)))))
       scale))

(defn get-scale [moment]
  (let [name* (moment :name)]
    [moment
     (if (moment :scale)
       (+sc-tun-note (moment :scale))
       (-> polydori :subcps (get name*) :scale +cents
           (->> +sc-tun-note)))]))

(defn make-sc-tuning [var-name tuning-name-fn [moment scale]]
  (let [fmt #(format "~%s = Tuning.new(#[%s], 2.0,  \"%s\");"
                     var-name % (tuning-name-fn moment))]
    (->> (mapv :sc/tun-note scale)
         (str/join ", ")
         fmt)))

(defn make-sc-scale [tuning-name-fn [moment scale]]
  (let [fmt #(format "Scale.new(#[%s], 35, ~polydori, \"%s\");"
                     % (tuning-name-fn moment))]
    (->> (mapv :degree scale)
         (str/join ", ")
         fmt)))

(defn make-file [file-name sc-tunings]
  (let [data (format "(\n%s\n)" (str/join "\n" sc-tunings))]
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

(comment
  ;; make-dekany-scales

  (->> harmonic-form
       #_(drop 1)
       (map get-scale)
       (map (partial make-sc-scale dek-tun-name))
       (make-file "dekany-moments-scales.scd"))

  ;; make-hexany-scales

  (->> harmonic-form
       (drop 1)
       (mapcat get-hexanies)
       (map get-scale)
       (map (partial make-sc-scale hex-tun-name))
       (make-file "hexany-moments-scales.scd"))

;; make-polydori tuning

  (make-file "polydori-tuning.scd"
             [(make-sc-tuning "polydori" (constantly "polydori") (get-scale polydori))])

  (make-file "dorian_scales.scd"
             [(make-sc-scale (constantly "dorian-1v2")
                             (get-scale {:scale dorico-1v2}))])

  (make-file "tuning-and-scales.scd"
             [(make-sc-tuning "polydori" (constantly "polydori") (get-scale polydori))

              (make-sc-scale (constantly "dorian-1v2")
                             (get-scale {:scale dorico-1v2}))
              (->> harmonic-form
                   #_(drop 1)
                   (mapv get-scale)
                   (mapv (partial make-sc-scale dek-tun-name))
                   (str/join "\n"))
              (->> harmonic-form
                   (drop 1)
                   (mapcat get-hexanies)
                   (mapv get-scale)
                   (mapv (partial make-sc-scale hex-tun-name))
                   (str/join "\n"))]))

(comment
;;;;;;;;;;;;;;;;;;;;;;
;;; make scl dekanies
;;;;;;;;;;;;;;;;;;;;;;

  (let [scl-files
        (map (fn [m]
               (let [scala-data
                     (if (:name m)
                       (-> polydori
                           (get-in [:subcps (:name m)])
                           make-scl-file)
                       (make-scl-file m))]
                 (assoc scala-data
                        :filename (format "momento_%s.scl"
                                          (:momento m)))))
             harmonic-form)]
    (doseq [{:keys [filename content]} scl-files]
      (spit (str tunings-dir "/" filename) content))))
