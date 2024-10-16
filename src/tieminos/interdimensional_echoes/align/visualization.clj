(ns tieminos.interdimensional-echoes.align.visualization
  (:require
   [erv.utils.core :refer [period-reduce]]
   [tieminos.lattice.v1.lattice :refer [add-played-ratio draw-lattice
                                        remove-all-played-ratios remove-played-ratio]]
   [tieminos.midi.core :refer [get-iac2! midi-in-event]]
   [tieminos.osc.reaper :as reaper]
   [tieminos.polydori.analysis.align :refer [rooted-hexanies]]
   [tieminos.polydori.analysis.dorian-hexanies :refer [dorian-hexanies-in-polydori]]
   [tieminos.polydori.scale :refer [polydori-v2]]))

(def iac2 (get-iac2!))

(defn get-hex-degrees
  [hex-name]
  (->> dorian-hexanies-in-polydori
       (filter #(= hex-name (:name % second)))
       first
       :degrees
       sort))

(def hex-degs (range 0 6))
(def polydori-degs (range (count (:scale polydori-v2))))

(def polydori-deg->ratio
  (->> polydori-v2
       :scale
       (map (juxt :degree :bounded-ratio))
       (into {})))

(def rooted-hexanies-name->deg->ratio
  (->> rooted-hexanies
       (map (fn [[k degs]] [k (into {} (map-indexed vector degs))]))
       (into {})))

(-> rooted-hexanies)

(do
  (def ^:private midi->ratio
    (memoize (fn [ref-note midi-note deg->ratio-map degs]
               (deg->ratio-map (nth degs (mod (- midi-note ref-note) (count degs)))))))
  #_(midi->ratio 60 57 polydori-deg->ratio hex-degs)
  #_(midi->ratio 60 59 (rooted-hexanies-name->deg->ratio "diat6v3") hex-degs)
  [(midi->ratio 60
                2
                polydori-deg->ratio
                polydori-degs)
   (midi->ratio 60
                14
                polydori-deg->ratio
                polydori-degs)])

(def rooted-hexanies-scale
  (->> rooted-hexanies
       (mapcat second)
       set
       (map period-reduce)
       sort))

(set
 (map
  #(midi->ratio 60
                %
                (rooted-hexanies-name->deg->ratio "diat4v2")
                hex-degs)
  (range 0 60)))
(count hex-degs)
(defn add-ratio
  [lattice-atom deg->ratio-map degs note channel color]
  (let [ratio (midi->ratio 60
                           note
                           deg->ratio-map
                           degs)]
    (println ratio color)
    (add-played-ratio lattice-atom
                      {:ratio ratio
                       :group-id channel
                       :color color})))
(defn remove-ratio
  [lattice-atom deg->ratio-map degs note channel]
  (let [ratio (midi->ratio 60
                           note
                           deg->ratio-map
                           degs)]
    (remove-played-ratio lattice-atom
                         {:ratio ratio
                          :group-id channel})))

(def guitarrish-polydori-ratios
  [1
   64/63
   896/855
   64/57
   112/95
   32/27
   128/105
   56/45
   24/19
   224/171
   4/3
   128/95
   64/45
   28/19
   256/171
   32/21
   14/9
   448/285
   8/5
   224/135
   32/19
   16/9
   512/285
   28/15
   256/135
   256/133
   112/57])

(comment
  (reaper/init)
  (let [size 125]
    (def lattice-atom (draw-lattice
                       {:ratios (into #{} (concat rooted-hexanies-scale
                                                  guitarrish-polydori-ratios))
                        :width (* 16 size)
                        :height (* 9 size)})))

  (-> @lattice-atom)
  (erv.utils.core/factors 57)
  (erv.utils.core/factors 49)

  (remove-all-played-ratios lattice-atom)
  (reaper/play)
  (reaper/stop)

  (midi-in-event
   {:midi-input iac2
    :note-on (fn [{:keys [note channel]}]
               (let [[hex-name color] (case channel
                                        0 ["diat6v3" [160, 21, 64]]
                                        1 ["diat4v2" [27, 162, 24]]
                                        2 ["diat3v2" [51, 62, 212]]
                                        3 ["diat5v2" [253, 1, 0]]
                                        4 ["diat2v2" [247, 105, 21]]
                                        5 ["diat2v2" [238, 222, 4]]
                                        nil)]
                 (if (= 6 channel) ;; guitarrish, uses polydori
                   (do
                     (println "PDN" note)
                     (add-ratio lattice-atom polydori-deg->ratio polydori-degs note channel [25 215 153]))
                   (add-ratio lattice-atom (rooted-hexanies-name->deg->ratio hex-name) hex-degs note channel color))))
    :note-off (fn [{:keys [note channel]}]
                (let [hex-name (case channel
                                 0 "diat6v3"
                                 1 "diat4v2"
                                 2 "diat3v2"
                                 3 "diat5v2"
                                 4 "diat2v2"
                                 5 "diat2v2"
                                 nil)]
                  (if hex-name
                    (remove-ratio lattice-atom (rooted-hexanies-name->deg->ratio hex-name) hex-degs note channel)
                    (remove-ratio lattice-atom polydori-deg->ratio polydori-degs note channel))))
    :auto-ctl? false}))
