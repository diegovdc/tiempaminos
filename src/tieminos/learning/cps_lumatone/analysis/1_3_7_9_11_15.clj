(ns tieminos.learning.cps-lumatone.analysis.1-3-7-9-11-15
  (:require
   [clojure.edn :as edn]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [overtone.midi :as midi]
   [taoensso.timbre :as timbre]
   [tieminos.midi.algo-note :as algo-note]
   [tieminos.midi.scl :refer [scl-note-off scl-note-on]]
   [tieminos.synths :as synths]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(def model
  "http://anaphoria.com/Wilson1-3-7-9-11-15x3_pascal_eikosany.scl"
  #{45/44
    35/33
    12/11
    9/8
    7/6
    105/88
    5/4
    14/11
    4/3
    15/11
    140/99
    35/24
    3/2
    14/9
    35/22
    5/3
    56/33
    7/4
    20/11
    15/8
    21/11
    1})

(comment
  (->> (cps/make 3 (map #(* 1 %) [1 3 7 9 11 15])
                 :norm-fac (* 1 9 11))
       :scale
       (map :bounded-ratio)
       set
       (set/difference model))
  (->> (cps/make 3 (map #(* 1 %) [1 3 7 9 11 15])
                 :norm-fac (* 1 9 11))
       :scale
       (map :bounded-ratio)
       set
       (set/difference model)))

(def added-tones [{:set #{4/3 7 15} :ratio 140/99 :bounded-ratio 140/99 :bounding-period 2}
                  {:set #{11 9} :set-vecs [[11 9 9] [11 9 3 3]] :ratio 9 :bounded-ratio 9/8 :bounding-period 2}])

(def eik-22 (-> (cps/make 3 (map #(* 1 %) [1 3 7 9 11 15])
                          :norm-fac (* 1 9 11))
                cps/+all-subcps
                (update :scale (comp
                                #(map-indexed (fn [i n] (assoc n :index i)) %)
                                #(sort-by :bounded-ratio %)
                                concat)
                        added-tones)))

(-> eik-22 :subcps (get "3)5 of 3)6 1.3.7.9.11")
    :scale
    (->> (mapv (juxt :bounded-ratio (comp #(str/join "." %) sort :set)))))

(def eik-22-by-set (reduce #(assoc %1 (:set %2) %2) {} (:scale eik-22)))
(-> eik-22-by-set)

(def scale
  (->> eik-22
       :scale))

(defn +index [scale]
  (sort-by :index (map (comp eik-22-by-set :set) scale)))

(defn subcps-scale
  [subcps-name]
  (-> eik-22 :subcps
      (get subcps-name)
      :scale
      +index))

(defn scale->intervals [scale]
  (let [start (:index (first scale))]
    {:start start
     :intervals (map #(-> % :index (- start)) scale)
     :scale scale}))

(scale->intervals (subcps-scale "1)4 of 3)6 3.11-1.7.9.15"))

(-> eik-22 :subcps keys sort)
(def eik-22-offset 9)

(comment
  (timbre/set-level! :info)
  (def sink (midi/midi-out "VirMIDI"))
  (reset! gp/refrains {})
  (let [deky (filter (comp dek-3-5-a-b-c-d-e :archi-set) scale)]
    (ref-rain
     :id ::intervals
     :durs (repeat (count deky) 1)
     :on-event
     (on-event
      (do
        #_(algo-note/algo-note :sink sink
                               :dur 1000 ;; milliseconds
                               :midi-note (+ 38 9 start (nth intervals index))
                               :vel 60
                               :note-on-fn scl-note-on
                               :note-off-fn scl-note-off)
        (synths/low
         :freq (scale/deg->freq
                deky
                440
                index
                :period -1))))
     :loop? false))
  (let [{:keys [start intervals]
         subscale :scale}
        (scale->intervals (subcps-scale "2)4 of 3)6 9-1.3.7.11"))
        midi-offset eik-22-offset ;; offset from midi control respect to the index 0 of out scale
        ]
    (println (+ start midi-offset) intervals)
    (ref-rain
     :id ::intervals
     :durs (repeat (count intervals) 1)
     :on-event
     (on-event
      (do
        (algo-note/algo-note :sink sink
                             :dur 1000 ;; milliseconds
                             :midi-note (+ 38 9 start (nth intervals index))
                             :vel 60
                             :note-on-fn scl-note-on
                             :note-off-fn scl-note-off)
        (synths/low
         :freq (scale/deg->freq
                scale
                440
                (+ (nth intervals index) start)
                :period -1))))
     :loop? false))
  (algo-note/algo-note :sink sink
                       :dur 1000 ;; milliseconds
                       :midi-note (+ 38 9)
                       :vel 60
                       :note-on-fn scl-note-on
                       :note-off-fn scl-note-off)
  (synths/low
   :freq (scale/deg->freq
          scale
          440
          0
          :period -1)
   :dcy 2)

  (color-map
   (:archi-set (nth scale 0))))

(do
  (defn- parse-int [x]
    (try (or (edn/read-string x) 1000)
         (catch Exception _ 1000))))

(defn ltn->edn [ltn]
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

(sort-by (comp parse-int first) [{nil {:a :b} 1 {:c :d}}])
(do
  (defn get-keys [key-data]
    (let [ks (keys key-data)
          key* (first (filter #(str/includes? % "Key_") ks))
          chan (first (filter #(str/includes? % "Chan_") ks))
          color (first (filter #(str/includes? % "Col_") ks))]
      {:key key*
       :key-val (get key-data key*)
       :chan chan
       :chan-val (get key-data chan)
       :color color
       :color-val (get key-data color)}))
  (get-keys {"Key_0" 100, "Chan_0" 1, "Col_0" "46D367", "CCInvert_0" nil}))
(-> scale)

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
(defn wilson-22-eik-midi-note->scale-note
  [midi-note]
  (nth scale (mod (- midi-note 16) 22)))
(comment
  (-> scale)
  (let [midi-note 1]
    (algo-note/algo-note :sink sink
                         :dur 1000 ;; milliseconds
                         :midi-note (+ (* 1 22) midi-note)
                         :vel 60
                         :note-on-fn scl-note-on
                         :note-off-fn scl-note-off)
    (:bounded-ratio
     (wilson-22-eik-midi-note->scale-note midi-note)))
  (do (synths/low
       :freq (scale/deg->freq
              scale
              440
              (- 39 16 eik-22-offset)
              :period -1)
       :dcy 5)
      (synths/low
       :freq (scale/deg->freq
              scale
              440
              (- 52 16 eik-22-offset)
              :period -1)
       :dcy 5))
  [(nth scale (mod (- 52 16 eik-22-offset) 22))
   (nth scale (mod (- 39 16 eik-22-offset) 22))])
(/  7/6 14/9)
(def archi-sets
  ;; FIXME uncomment when function is available in future commit
  #_(cps/archi-subcps-sets 3 6))
(-> scale)

(comment
  (map :archi-set
       (filter
        (comp
         #{14/9 45/44
           35/33 140/99
           9/8 21/11}
         :bounded-ratio)
        scale)))
(def dek-3-5-a-b-c-d-e #{#{:e :d :a} #{:e :b :d}
                         #{:c :b :a} #{:e :b :a}
                         #{:e :c :d} #{:b :d :a}
                         #{:c :b :d} #{:e :c :a}
                         #{:c :d :a} #{:e :c :b}})
(-> dek-3-5-a-b-c-d-e)
(defn color-fn [pred {:keys [key-val chan-val color-val]}]
  (let [midi-note (* key-val chan-val)
        archi-set (:archi-set (wilson-22-eik-midi-note->scale-note midi-note))]
    (if-not (pred archi-set)
      (do #_(println "Archiset not found" key-val chan-val)
       "111111" #_color-val)
      (-> archi-set
          color-map
          (str/replace "#" "")))))

(defn change-key-color [color-fn key-data]
  (let [ks (get-keys key-data)]
    #_(println ks)
    (if-not (:key ks)
      key-data
      (assoc key-data (:color ks) (color-fn ks)))))

(def parsed-ltn
  ;; FIXME uncomment when file is available
  #_(ltn->edn (slurp "/Users/diego/Music/diego/lumatone/default-22-et.ltn")))

(defn maybe-join-vals
  [[k v]]
  (if v
    (str k "=" v)
    k))

(defn update-ltn [color-fn parsed-ltn]
  (->> parsed-ltn
       (map (fn [section]
              [(first (keys section))
               (map #(change-key-color
                      color-fn %)
                    (first (vals section)))]))))

(defn make-ltn [parsed-ltn]
  (str/join "\n"
            (mapcat (fn [[board keys*]]
                      [board
                       (str/join "\n" (map (comp #(str/join "\n" %) #(map maybe-join-vals %) seq) keys*))]) parsed-ltn)))

(defn make-ltn-pack-config
  [archi-sets]
  (->> archi-sets
       (remove #(or (str/includes? % "1)")
                    (str/includes? % "2)2")
                    (str/includes? % "1)3")
                    (str/includes? % "2)3")
                    (str/includes? % "3)3")
                    (str/includes? % "4)4")))
       (map (fn [{:keys [name set]}]
              {:filename (format
                          "/Users/diego/Music/diego/lumatone/wilson-22-eikosany-pack/wilson-22-eik-subset %s.ltn"
                          (str/replace name #"of 3\)6 " ""))
               :pred set}))))
(filter (comp #(str/includes? % "2)5") :filename) (make-ltn-pack-config archi-sets))
(-> archi-sets)
(comment
  (doseq [{:keys [filename pred]} (make-ltn-pack-config archi-sets)]
    (spit filename
          (make-ltn (update-ltn (partial color-fn pred) parsed-ltn))))
  (spit "/Users/diego/Music/diego/lumatone/wilson-22-et-eikosany-mul-dek-3-5-a.b.c.d.e.ltn"
        (make-ltn (update-ltn (partial color-fn dek-3-5-a-b-c-d-e) parsed-ltn))))

