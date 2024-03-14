(ns tieminos.lumatone.mos-colorizer
  (:require
   [clojure.string :as str]
   [erv.mos.mos :as mos]
   [tieminos.learning.cps-lumatone.analysis.1-3-7-9-11-15 :refer [dek-3-5-a-b-c-d-e wilson-22-eik-midi-note->scale-note]]
   [erv.marwa.core :as marwa]
   [tieminos.lumatone.ltn :refer [get-keys make-ltn parse-ltn]]
   [thi.ng.color.core :as color]
   [thi.ng.color.gradients :as grad]
   [tieminos.lumatone.coord-system :as gral-kb]))

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

(defn color-fn2 [mos-degs-seq-vec base-midi
                 {:keys [key-type-val key-val chan-val color-val]}]
  (let [midi-note (* key-val #_chan-val)]
    (reduce
     (fn [default-color [mos-degs-seq color]]
       (cond
         (= "0" key-type-val) "111111"
         (mos-note? mos-degs-seq base-midi midi-note) (reduced color)
         :else "111111" #_default-color))
     color-val
     mos-degs-seq-vec)))

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
  (mos/make 31 6)
  (marwa/get-possible-generator-sequences [4 3 4 3 4 3 4 3 3])
  (marwa/mos-permutations 31 [7 7 7 7 7 7 7 7 6]))

(defn mos-degs-rings-with-gradient
  [gradient-scheme mos]
  (let [gradient (grad/cosine-gradient (count mos) gradient-scheme)]
    (map-indexed (fn [i mos*]
                   [(mos->degs-seq mos*)
                    (if (every? #(= 1 %) mos*)
                      "111111"
                      (str/replace @(color/as-css (color/as-int24 (gradient i)))
                                   #"#" ""))])
                 mos)))
(comment
  (mos-degs-rings-with-gradient
   (grad/cosine-schemes :red-blue)
   (mos/make 31 12)))

(comment
  @(color/as-css (color/as-int24 (color/rgba 1 0.5 1 0)))

  (def my-grad
    (->> :red-blue
         (grad/cosine-schemes)
         (grad/cosine-gradient 7)))
  (my-grad 0)

  (-> grad/cosine-schemes)
  ;; specifying cosine coefficients directly
  (def my-grad
    (grad/cosine-gradient 100 [0.5 0 0.5] [0.5 0 0.5] [0.5 0 0.5] [0 0 0.5]))

  (def try-grad [[0.210 0.710 0.710] [2.157 0.618 0.789] [0.088 0.333 0.500] [0.197 -0.872 -1.012]])
  (def parsed-ltn
    (parse-ltn (slurp "/Users/diego/Music/diego/lumatone/31-ET-default.ltn")))
  (-> parsed-ltn)

  (->> parsed-ltn
       (update-key-colors (partial color-fn2
                                   (mos-degs-rings-with-gradient
                                    (grad/cosine-schemes :yellow-green-blue)
                                    (drop-last 2 (drop 3 (mos/make 31 6))))
                                   0))
       make-ltn
       (spit "/Users/diego/Music/diego/lumatone/31-ET-mothra-test.ltn"))
  (->> parsed-ltn
       (update-key-colors (partial color-fn2
                                   (mos-degs-rings-with-gradient
                                    (grad/cosine-schemes :yellow-green-blue)
                                    [[6 6 6 6 7]
                                     [6 6 6 6 6 1]
                                     [5 1 4 2 4 2 4 2 4 2 1]
                                     [1 4 1 1 3 2 2 2 2 2 2 2 2 2 2 1]])
                                   0))
       make-ltn
       (spit "/Users/diego/Music/diego/lumatone/31-ET-mothra-marwa.ltn"))
  (gral-kb/generate-keyboard-types->xy-intervals 6 31)
  (let [gen 6 period 31]
    (->> (gral-kb/make-ltn-data
          {:offset 14
           :period period
           :xy-intervals ((gral-kb/generate-keyboard-types->xy-intervals gen period)
                          [4 21])})
         (map :key-value)
         (apply min)))
  (->> (gral-kb/ltn-data->ltn
        (let [gen 6 period 31]
          (gral-kb/make-ltn-data
           {:offset 14
            :period period
            :xy-intervals ((gral-kb/generate-keyboard-types->xy-intervals gen period)
                           [4 21])})))
       parse-ltn
       (update-key-colors (partial color-fn2
                                   (mos-degs-rings-with-gradient
                                    (grad/cosine-schemes :yellow-green-blue)
                                    [[6 6 6 6 7]
                                     [6 6 6 6 6 1]
                                     [5 1 4 2 4 2 4 2 4 2 1]
                                     [1 4 1 1 3 2 2 2 2 2 2 2 2 2 2 1]])
                                   0))
       make-ltn
       (spit "/Users/diego/Music/diego/lumatone/31-ET-mothra-marwa_4-21kb.ltn"))
  (let [generator (- 31 26)
        mos (->> (mos/make 31 generator) (drop 5) (take 4))]
    (->> parsed-ltn
         (update-key-colors (partial color-fn2
                                     (mos-degs-rings-with-gradient
                                      (grad/cosine-schemes :yellow-green-blue)
                                      mos)
                                     0))
         make-ltn
         (spit (format "/Users/diego/Music/diego/lumatone/31-ET-gen-%s.ltn" generator))))
  (mos 31 14 3 5)
  (mos-ltn 31 14 3 5)
  (->> parsed-ltn
       (update-key-colors (partial color-fn (mos->degs-seq [5 3 3 4 3 4 3 4 2]) 0))
       make-ltn
       (spit "/Users/diego/Music/diego/lumatone/31-ET-orwell9-marwa2-test.ltn"))
  (->> parsed-ltn
       (update-key-colors (partial color-fn2 (mos-degs-rings-with-gradient
                                              (grad/cosine-schemes :green-magenta)
                                              (mos/make 31 12))
                                   0))
       make-ltn
       (spit "/Users/diego/Music/diego/lumatone/31-ET-oneirotonic-test3.ltn")))



(comment
  (def parsed-ltn
    (parse-ltn (slurp "/Users/diego/Music/diego/lumatone/5_gen-11_kb4-9_template.ltn")))
  (-> parsed-ltn)

  (let [template-path "/Users/diego/Music/diego/lumatone/11_gen-5_kb2-5_template.ltn"]
    (->> template-path

      slurp
      parse-ltn
      (update-key-colors (partial color-fn2
                                  (mos-degs-rings-with-gradient
                                    (grad/cosine-schemes :green-magenta)
                                    (->> (mos/make 11 5)
                                         (drop 2)
                                         (drop-last 1)))
                                  0))
      make-ltn
      (spit (str/replace template-path #"_template" ""))))
  )
