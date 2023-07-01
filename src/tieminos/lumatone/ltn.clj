(ns tieminos.lumatone.ltn
  "Manipulate .ltn files"
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn- parse-int [x]
  (try (or (edn/read-string x) 1000)
       (catch Exception _ 1000)))

(defn parse-ltn [ltn]
  (->> (str/split ltn #"\[(.*?)\]")
       (drop 1)
       (map-indexed
        (fn [i board]
          {(format "[Board%s]" i)
           (->> (str/split board #"(\r\n|\r|\n)")
                (drop 1)
                (map (fn [kv]
                       (let [[k v] (str/split kv #"=")]
                         (if (or (str/includes? k "Key")
                                 (str/includes? k "Chan"))
                           {k (edn/read-string v)}
                           {k v}))))
                (group-by #(-> % keys first (str/split #"_") second))
                (map (fn [[k v]]
                       [k (apply merge v)]))
                (sort-by (comp parse-int first))
                (map second))}))))

(defn get-keys [key-data]
  (let [ks (keys key-data)
        key* (first (filter #(str/includes? % "Key_") ks))
        chan (first (filter #(str/includes? % "Chan_") ks))
        color (first (filter #(str/includes? % "Col_") ks))
        key-type  (first (filter #(str/includes? % "KTyp_") ks))]
    {:key key*
     :key-type key-type
     :key-type-val (get key-data key-type)
     :key-val (get key-data key*)
     :chan chan
     :chan-val (get key-data chan)
     :color color
     :color-val (get key-data color)}))

(defn maybe-join-vals
  [[k v]]
  (if v
    (str k "=" v)
    k))

(defn make-ltn [parsed-ltn]
  (str/join "\n"
            (mapcat (fn [[board keys*]]
                      [board
                       (str/join "\n" (map (comp #(str/join "\n" %) #(map maybe-join-vals %) seq) keys*))]) parsed-ltn)))
