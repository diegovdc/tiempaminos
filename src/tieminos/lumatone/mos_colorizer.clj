(ns tieminos.lumatone.mos-colorizer
  (:require
   [clojure.string :as str]
   [erv.mos.mos :as mos]
   [tieminos.learning.cps-lumatone.analysis.1-3-7-9-11-15 :refer [dek-3-5-a-b-c-d-e wilson-22-eik-midi-note->scale-note]]
   [erv.marwa.core :as marwa]
   [tieminos.lumatone.ltn :refer [get-keys make-ltn parse-ltn]]))

(def color-map
  {#{:a :d :e} "#6c7eac"
   #{:b :d :e} "#6ca0a1"
   #{:d :e :f} "#6c6edb"
   #{:a :d :f} "#aa759f"
   #{:a :b :c} "#b7a14d"
   #{:a :b :e} "#a2747e"
   #{:c :d :e} "#4ea590"
   #{:c :e :f} "#74688f"
   #{:a :e :f} "#a250ab"
   #{:a :b :d} "#aaaa75"
   #{:b :c :d} "#7adf62"
   #{:a :c :f} "#b76f68"
   #{:b :d :f} "#aa9495"
   #{:b :c :f} "#b78d61"
   #{:a :c :e} "#747870"
   #{:b :e :f} "#a265a0"
   #{:a :c :d} "#7ab068"
   #{:a :b :f} "#ff6b75"
   #{:b :c :e} "#749869"
   #{:c :d :f} "#7a9985"})

#_(defn color-fn [pred {:keys [key-val chan-val color-val]}]
    (let [midi-note (* key-val chan-val)
          archi-set (:archi-set (wilson-22-eik-midi-note->scale-note midi-note))]
      (if-not (pred archi-set)
        "111111"
        (-> archi-set
            color-map
            (str/replace "#" "")))))

(do
  (def mothra-6 [6 6 6 6 6 1])

  (defn mos->degs-seq [mos]
    (reduce (fn [acc n] (conj acc (+ n (or (last acc) 0))))
            [0]
            mos))

  (mos->degs-seq mothra-6))

(defn mos-note?
  [mos-degs-seq base-midi midi-note]
  (->> mos-degs-seq
       last
       (mod (- midi-note base-midi))
       ((set mos-degs-seq))
       boolean))

(defn color-fn [mos-degs-seq base-midi {:keys [key-val chan-val color-val]}]
  (let [midi-note (* key-val #_chan-val)]
    (if-not (mos-note? mos-degs-seq base-midi midi-note)
      color-val
      "FFFFFF")))

(defn change-key-color [color-fn key-data]
  (let [ks (get-keys key-data)]
    (if-not (:key ks)
      key-data
      (assoc key-data (:color ks) (color-fn ks)))))

(defn update-key-colors [color-fn parsed-ltn]
  (->> parsed-ltn
       (map (fn [section]
              [(first (keys section))
               (map #(change-key-color
                      color-fn %)
                    (first (vals section)))]))))

(comment
  (mos/make 31 12)
  (marwa/get-possible-generator-sequences [4 3 4 3 4 3 4 3 3])
  (marwa/mos-permutations 31 [7 7 7 7 7 7 7 7 6]))

(comment
  (def parsed-ltn
    (parse-ltn (slurp "/Users/diego/Music/diego/lumatone/31-ET-default.ltn")))
  (-> parsed-ltn)

  (->> parsed-ltn
       (update-key-colors (partial color-fn (mos->degs-seq mothra-6) 0))
       make-ltn
       (spit "/Users/diego/Music/diego/lumatone/31-ET-mothra6-test.ltn"))
  (->> parsed-ltn
       (update-key-colors (partial color-fn (mos->degs-seq [5 3 3 4 3 4 3 4 2]) 0))
       make-ltn
       (spit "/Users/diego/Music/diego/lumatone/31-ET-orwell9-marwa2-test.ltn"))
  (->> parsed-ltn
       (update-key-colors (partial color-fn (mos->degs-seq [5 5 2 5 5 2 5 2]) 0))
       make-ltn
       (spit "/Users/diego/Music/diego/lumatone/31-ET-oneirotonic-test2.ltn")))
