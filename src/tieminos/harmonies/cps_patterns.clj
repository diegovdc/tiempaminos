(ns tieminos.harmonies.cps-patterns
  (:require [clojure.math :refer [pow]]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [instaparse.core :as insta]
            [tieminos.utils :refer [wrap-at]]))

(def parser
  (insta/parser
   "sequence = items
    chord = <'['> <ws>? items? <ws>? <']'>
    items = (note | chord) (<ws> (note | chord | partial))*

    partial = num-partial | dot
    dot = <'.'> period?
    num-partial = transp period? | period transp

    note = side* letters side*
    <side> = period | transp | plus | minus
    plus  = <'+'>
    minus = <'-'>

    letters = #'[a-z]+'
    period  = #'[+-]?[0-9]+'
    transp  = #'>+' | #'<+'
    ws      = #'\\s+'"
   :start :sequence))

;;;;;;;;;;;;;;;;;;
;; * Transform
;;;;;;;;;;;;;;;;;;

(declare transform)

(defn- transp->int [s]
  (if (str/starts-with? s ">") (count s) (- (count s))))

(defn- find-child [tag tree]
  (first (filter #(and (vector? %) (= tag (first %))) (rest tree))))

(defn- extract [tag tree]
  (some-> (find-child tag tree) second))

(defn- items-vec [items-tree]
  (mapv transform (rest items-tree)))

(defn transform [tree]
  (let [tag (first tree)
        children (vec (rest tree))]
    (case tag
      :sequence {:sequence (items-vec (first children))}
      :chord    (if-let [it (find-child :items tree)]
                  {:chord (items-vec it)}
                  {:chord []})
      :partial  (transform (first children))
      :dot      (let [p (extract :period tree)]
                  (cond-> {:transp 0}
                    p (assoc :period (Integer/parseInt p))))
      :num-partial
      (let [t (extract :transp tree)
            p (extract :period tree)]
        (cond-> {:transp (transp->int t)}
          p (assoc :period (Integer/parseInt p))))
      :note
      (let [letters-seq (filter #(= :letters (first %)) children)
            periods     (filter #(= :period  (first %)) children)
            transps     (filter #(= :transp  (first %)) children)
            pluses      (filter #(= :plus    (first %)) children)
            minuses     (filter #(= :minus   (first %)) children)]
        (when (or (not= 1 (count letters-seq))
                  (> (count periods) 1)
                  (> (count transps) 1)
                  (> (count pluses) 1)
                  (> (count minuses) 1)
                  (and (seq pluses) (seq minuses)))
          (throw (ex-info "Invalid note" {:tree tree})))
        (let [letters (second (first letters-seq))]
          (cond-> {:set    (set (map (comp keyword str) letters))
                   :period (if (seq periods)
                             (Integer/parseInt (second (first periods)))
                             0)}
            (seq transps) (assoc :transp (transp->int (second (first transps))))
            (seq pluses)  (assoc :position :above-prev)
            (seq minuses) (assoc :position :below-prev)))))))

(defn parse [s]
  (let [s (str/trim s)]
    (if (empty? s)
      {:sequence []}
      (let [tree (parser s)]
        (if (insta/failure? tree)
          (throw (ex-info "Parse failed" {:input s :result tree}))
          (transform tree))))))

(comment

  (parse "abc")
  ;; {:sequence [{:set #{:a :b :c} :period 0}]}

  (parse "abc1")
  ;; {:sequence [{:set #{:a :b :c} :period 1}]}

  (parse "abc-1")
  ;; {:sequence [{:set #{:a :b :c} :period -1}]}

  (parse ">abc")
  ;; {:sequence [{:set #{:a :b :c} :period 0 :transp 1}]}

  (parse "<<abc1")
  ;; {:sequence [{:set #{:a :b :c} :period 1 :transp -2}]}

  (parse "abc>1")
  ;; {:sequence [{:set #{:a :b :c} :period 1 :transp 1}]}

  (parse "[abc bcd cda]")
  ;; {:sequence [{:chord [{:set #{:a :b :c} :period 0}
  ;;                      {:set #{:b :c :d} :period 0}
  ;;                      {:set #{:c :d :a} :period 0}]}]}

  (parse "abc bcd cda")
  ;; {:sequence [note note note]}

  (parse "abc [bcd cda]")
  ;; {:sequence [note {:chord [note note]}]}

  (parse "abc > cda <")
  ;; {:sequence [{:set #{:a :b :c} :period 0}
  ;;             {:transp 1}
  ;;             {:set #{:c :d :a} :period 0}
  ;;             {:transp -1}]}

  (parse "abc . cda <")
  ;; {:sequence [{:set #{:a :b :c} :period 0}
  ;;             {:transp 0}
  ;;             {:set #{:c :d :a} :period 0}
  ;;             {:transp -1}]}

  (parse "[abc > bcd]")
  ;; {:sequence [{:chord [note {:transp 1} note]}]}

  (parse "[abc [bcd cda]]")
  ;; {:sequence [{:chord [note {:chord [note note]}]}]}

  (parse "[]")
  ;; {:sequence [{:chord []}]}

  (parse "abc >1 . <<-2")
  ;; {:sequence [note {:transp 1 :period 1}
  ;;             {:transp 0}
  ;;             {:transp -2 :period -2}]}

  (parse "> abc")                       ; leading partial => parse failure
  (parse ">")                           ; bare partial    => parse failure
  (parse "1")                           ; bare number     => parse failure
  (parse ""))                            ; empty           => {:sequence []}

;;;;;;;;;;;;;;;;;;;
;; * CPS Transform
;;;;;;;;;;;;;;;;;;;
(defn- calc-note-position
  [note period]
  (-> note :bounded-ratio (* (pow (:bounding-period note) period))))

(defn- position-note
  [position note period prev-note-data]
  (let [prev-note-pos (calc-note-position (:note prev-note-data) (:period prev-note-data))]
    (loop [period period]
      (case position
        :above-prev (if (> (calc-note-position note period) prev-note-pos)
                      period
                      (recur (inc period)))
        :below-prev (if (< (calc-note-position note period) prev-note-pos)
                      period
                      (recur (dec period)))
        period))))

(defn- notes->cps-notes
  [scale-transp-maps transformed-notes]
  (reduce
   (fn [acc {:keys [chord set transp period position] :or {transp 0 period 0} :as data}]
     (if chord data
         (let [{:keys [archiset->note deg->note scale]} scale-transp-maps
               note (archiset->note set)
               [note* period*] (if (zero? transp)
                                 [note period]
                                 (let [degree (+ transp (:degree note))
                                       scale-degree (mod degree (count scale))
                                       period-transp (quot degree (count scale))]
                                   [(deg->note scale-degree)
                                    (+ period period-transp)]))
               prev-note-data (last acc)
               period** (if-not prev-note-data period
                                (position-note position note period* prev-note-data))]
           (when-not note (throw (ex-info "Unknown set:" data)))
           (conj acc
                 (assoc data
                        :note note*
                        :period period**)))))
   []
   transformed-notes))

(defn- note-transform
  [scale-transp-maps items-vec-data]
  (->> items-vec-data
       (reduce
        (fn [acc {:keys [chord set transp period position] :as data}]
          (let [prev-note (last acc)]
            (cond
              chord (conj acc data)
              set (conj acc data)
              (not prev-note) (throw (ex-info "Can't have a transp without a prev-note" data))
              transp (conj acc (-> data
                                   (assoc :set (:set prev-note)
                                          :period (or period (:period prev-note 0)))
                                   (update :transp + (:transp prev-note 0)))))))
        [])
       (notes->cps-notes scale-transp-maps)))

(defn make-shift-maps
  [archi-factors]
  (->> (map
        (fn [xs ys] (->> (interleave xs ys)

                         (apply hash-map)))
        (repeat archi-factors)
        (combo/permutations archi-factors))
       (reduce (fn [{:keys [perms variations] :as  acc} m]
                 (let [im (set/map-invert m)]
                   (if (variations im)
                     acc
                     {:perms (conj perms m)
                      :variations (conj variations m im)})))
               {:perms []
                :variations #{}})
       :perms))

(def make-scale-transp-maps
  (memoize (fn [cps]
             (let [cps-scale (:scale cps)
                   archi-set (->> cps :scale (mapcat :archi-set) set sort)
                   shift-maps (make-shift-maps archi-set)]
               {:scale cps-scale
                :archiset->note (reduce (fn [m {:keys [archi-set] :as note}]
                                          (assoc m archi-set note))
                                        {}
                                        cps-scale)
                :deg->note (reduce (fn [m {:keys [degree] :as note}]
                                     (assoc m degree note))
                                   {}
                                   cps-scale)
                :shift-maps shift-maps}))))

(defn- shift-set*
  [{:keys [shift-maps]} shift-index set*]
  (let [shift-map (wrap-at shift-index shift-maps)]
    (->> set*
         (map shift-map)
         set)))

(comment (shift-set* {:shift-maps (make-shift-maps [:a :b :c :d])} 5 #{:a :b}))

(defn- in-walk-shift
  [shift-index scale-transp-maps x]
  (if (:set x)
    (assoc x :set
           (shift-set*
            scale-transp-maps
            shift-index
            (:set x)))
    x))

(defn shift
  [shift-index cps-pattern]
  {:process (partial in-walk-shift shift-index)
   :pattern cps-pattern})

(defn- in-walk-prefix
  [archiset-items-set _scale-transp-maps x]
  (if (:set x)
    (assoc x :set
           (set/union (:set x) archiset-items-set))
    x))

(defn prefix
  "Add one or more archi-factors to the archi-factors of a cps-pattern"
  ;; eg. (prefix "a" "c b d")
  [archi-set-items cps-pattern]
  {:process (partial in-walk-prefix (-> archi-set-items (str/split #"") (->> (map keyword) set)))
   :pattern cps-pattern})

(defn readable [cps-seq]
  (->> cps-seq
       :sequence
       (walk/postwalk (fn [x] (if (:note x)
                                (->> (:archi-set (:note x))
                                     sort
                                     (map name)
                                     (str/join ""))
                                x)))))

(defn ->freqs
  [root-freq {:keys [sequence cps] :as _cps-seq}]
  (->> sequence
       (walk/postwalk (fn [x] (if (:note x)
                                (scale/deg->freq (:scale cps) root-freq (-> x :note :degree)
                                                 {:period (:period x 0)})

                                x)))))

(do
  (defn- preprocess-pattern
    [scale-transp-maps cps-pattern]
    (cond
      (string? cps-pattern) {:parse-tree (parse cps-pattern)
                             :process identity}
      (:process cps-pattern) {:parse-tree (parse (:pattern cps-pattern))
                              :process (partial (:process cps-pattern) scale-transp-maps)}))

  (defn cps-seq
    [cps cps-pattern]
    (let [scale-transp-maps (make-scale-transp-maps cps)
          {:keys [process parse-tree]} (preprocess-pattern scale-transp-maps cps-pattern)
          note-transform-fn (fn [x]
                              (if (and (vector? x) (map? (first x)))
                                (note-transform scale-transp-maps x)
                                x))
          ;; _ (def pt parse-tree)
          result (walk/postwalk
                  (comp note-transform-fn
                        process)
                  parse-tree)]
      (assoc result :cps cps)))

  (->> (cps-seq (cps/make 2 [1 3 5 7])
                (shift 1 "ab [bc > . cd]")
                #_(prefix "a" "c b d"))
       readable)

  #_(->> (cps-seq (cps/make 2 [1 3 5 7])
                  (rot 1 "ab [bc >1 .2 cd]")
                  #_(prefix "a" "c b d"))
         (freqs 1)))

(parse "[bc > . cd]")

(comment
  ;; test
  (->> (cps-seq (cps/make 2 [1 3 5 7])
                (shift 1 "ab [bc > . cd]"))
       :sequence
       (walk/postwalk (fn [x] (if (:note x)
                                (->> (:archi-set (:note x))
                                     sort
                                     (map name)
                                     (str/join ""))
                                x))))
  (->> (cps-seq
        (cps/make 2 [1 3 5 7]) ;; 2)4 1.3.5.7          
        "ab ac ad")
       (->freqs 1))

  (->> (cps-seq
        (cps/make 2 [1 3 5 7]) ;; 2)4 1.3.5.7          
        "ab ac cb")
       (->freqs 1)))



