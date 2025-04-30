(ns lumatone.colorizer
  (:require
   [clojure.string :as str]
   [erv.utils.scale :refer [scale-steps->degrees]]
   [lumatone.ltn :refer [get-keys]]
   [thi.ng.color.core :as color]
   [thi.ng.color.gradients :as grad]
   #_[tieminos.learning.cps-lumatone.analysis.1-3-7-9-11-15
      :refer
      [dek-3-5-a-b-c-d-e wilson-22-eik-midi-note->scale-note]]))

(defn note-in-degs-seq?
  [degs-seq base-midi midi-note]
  (->> degs-seq
       last
       (mod (- midi-note base-midi))
       ((set degs-seq))
       boolean))

(defn color-fn
  "Return the color for the given key"
  [degs-seqs-colors base-midi
   {:keys [key-type-val key-val _chan-val color-val]
    :as _lumatone-key-data}]

  (let [midi-note (* key-val #_chan-val)]
    (reduce
     (fn [_default-color [degs-seq color]]
       (cond
         (= "0" key-type-val) "111111"
         (note-in-degs-seq? degs-seq base-midi midi-note) (reduced color)
         :else "111111"))
     color-val
     degs-seqs-colors)))

(defn- change-key-color [color-fn key-data]
  (let [ks (get-keys key-data)]
    (if-not (:key ks)
      key-data
      (assoc key-data (:color ks) (color-fn ks)))))

(defn colorize-ltn
  "Updates the colors of a `parsed-ltn` file using the output of the `key-data->color` function"
  [key-data->color parsed-ltn]
  (->> parsed-ltn
       (map (fn [section]
              [(first (keys section))
               (map #(change-key-color key-data->color %)
                    (first (vals section)))]))))

(defn degs-rings-with-gradient
  "Get the colors for a collection of collections of degrees"
  [gradient-scheme degrees-coll]
  (let [gradient (grad/cosine-gradient (count degrees-coll) gradient-scheme)
        all-degrees (->> degrees-coll first last inc range)]
    (map-indexed (fn [i degrees]
                   [degrees
                    (if (= degrees all-degrees)
                      "111111"
                      (str/replace @(color/as-css (color/as-int24 (gradient i)))
                                   #"#" ""))])
                 degrees-coll)))

(defn ^:deprecated mos-degs-rings-with-gradient-old
  [gradient-scheme mos]
  (let [gradient (grad/cosine-gradient (count mos) gradient-scheme)]
    (map-indexed (fn [i mos*]
                   [(scale-steps->degrees mos* false)
                    (if (every? #(= 1 %) mos*)
                      "111111"
                      (str/replace @(color/as-css (color/as-int24 (gradient i)))
                                   #"#" ""))])
                 mos)))

(defn mos-degs-rings-with-gradient
  "Get the colors for a collection of collections of mos-rings (i.e. a whole mos)"
  [gradient-scheme mos]
  (degs-rings-with-gradient
   gradient-scheme (map #(scale-steps->degrees % false) mos)))
