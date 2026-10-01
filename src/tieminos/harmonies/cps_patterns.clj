(ns tieminos.harmonies.cps-patterns
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [instaparse.core :as insta]))

(def parser
  (insta/parser
   "sequence = items
     chord = <'['> <ws>? items? <ws>? <']'>
     items = (note | chord) (<ws> (note | chord | partial))*

     partial = num-partial | dot
     dot = '.'
     num-partial = transp | transp period | period transp

     note = letters
          | letters period        | period letters
          | letters transp        | transp letters
          | letters period transp | letters transp period
          | period letters transp | period transp letters
          | transp letters period | transp period letters

     letters = #'[a-z]+'
     period  = #'-?[0-9]+'
     transp  = #'>+' | #'<+'
     ws      = #'\\s+'"
   :start :sequence))

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
      :dot      {:transp 0}
      :num-partial
      (let [t (extract :transp tree)
            p (extract :period tree)]
        (cond-> {:transp (transp->int t)}
          p (assoc :period (Integer/parseInt p))))
      :note
      (let [l (extract :letters tree)
            p (extract :period tree)
            t (extract :transp tree)]
        (cond-> {:set (set (map (comp keyword str) l))
                 :period (if p (Integer/parseInt p) 0)}
          t (assoc :transp (transp->int t)))))))

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

(def cps-scale (-> (cps/make 2 [1 3 5 7])
                   :scale))

(do
  (def archiset->note

    (reduce (fn [m {:keys [archi-set] :as note}]
              (assoc m archi-set note))
            {}
            cps-scale))
  (def deg->note

    (reduce (fn [m {:keys [degree] :as note}]
              (assoc m degree note))
            {}
            cps-scale))
  (archiset->note
   (-> (cps/make 2 [1 3 5 7])
       :scale)))

(def scale-transp-maps
  {:scale cps-scale
   :archiset->note archiset->note
   :deg->note deg->note})

(defn- notes->cps-notes
  [scale-transp-maps transformed-notes]
  (mapv
   (fn [{:keys [chord set transp period] :or {transp 0 period 0} :as data}]
     (if chord data
         (let [{:keys [archiset->note deg->note scale]} scale-transp-maps
               note (archiset->note set)
               [note* period*] (if (zero? transp)
                                 [note period]
                                 (let [degree (+ transp (:degree note))
                                       scale-degree (mod degree (count scale))
                                       period-transp (quot degree (count scale))]
                                   [(deg->note scale-degree)
                                    (+ period period-transp)]))]
           (when-not note (throw (ex-info "Unknown set:" data)))
           (assoc data
                  :note note*
                  :period period*))))
   transformed-notes))

(defn- note-transform
  [scale-transp-maps items-vec-data]
  (->> items-vec-data
       (reduce
        (fn [acc {:keys [chord set transp period] :as data}]
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

(comment

  (->> (cps-seq (cps/make 2 [1 3 5 7])
                "bc > >  ")
       (freqs 1)))

(def make-scale-transp-maps
  (memoize (fn [cps]
             (let [cps-scale (:scale cps)
                   archi-set-kws->index (->> cps :scale (mapcat :archi-set) set sort (map-indexed (fn [i k] [k i])) (into {}))]
               {:scale cps-scale
                :archiset->note (reduce (fn [m {:keys [archi-set] :as note}]
                                          (assoc m archi-set note))
                                        {}
                                        cps-scale)
                :deg->note (reduce (fn [m {:keys [degree] :as note}]
                                     (assoc m degree note))
                                   {}
                                   cps-scale)
                :archi-set-kws->index archi-set-kws->index
                :archi-set-index->kw (set/map-invert archi-set-kws->index)}))))

(let [m (make-scale-transp-maps (cps/make 2 [1 3 5 7]))]
  (m :archi-set-kws->index))

(defn- rotate-set*
  [{:keys [archi-set-kws->index archi-set-index->kw]} rot-amount set*]
  (->> set*
       (map (comp
             archi-set-index->kw
             #(mod (+ rot-amount %) (count archi-set-kws->index))
             archi-set-kws->index))
       set))

(rotate-set*
 (make-scale-transp-maps (cps/make 2 [1 3 5 7]))
 1
 #{:a :b})

(defn- in-walk-rotation
  [amount scale-transp-maps x]
  (if (:set x)
    (assoc x :set
           (rotate-set*
            scale-transp-maps
            amount
            (:set x)))
    x))

(defn rot
  [amount cps-pattern]
  {:process (partial in-walk-rotation amount)
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

(defn freqs
  [root-freq {:keys [sequence cps] :as _cps-seq}]
  (->> sequence
       (walk/postwalk (fn [x] (if (:note x)
                                (scale/deg->freq (:scale cps) root-freq (-> x :note :degree)
                                                 {:period (:period x 0)})

                                x)))))

(scale/deg->freq (:scale (cps/make 2 [1 3 5 7])) 1 0 :period 0)
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
          result (walk/postwalk
                  (comp note-transform-fn
                        process)
                  parse-tree)]
      (assoc result :cps cps)))

  (->> (cps-seq (cps/make 2 [1 3 5 7])
                (rot 1 "ab [bc > . cd]")
                #_(prefix "a" "c b d"))
       readable)

  (->> (cps-seq (cps/make 2 [1 3 5 7])
                (rot 1 "ab [bc >1 . cd]")
                #_(prefix "a" "c b d"))
       (freqs 1)))
(parse "[bc > . cd]")

;; test
(->> (cps-seq (cps/make 2 [1 3 5 7])
              (rot 1 "ab [bc > . cd]"))
     :sequence
     (walk/postwalk (fn [x] (if (:note x)
                              (->> (:archi-set (:note x))
                                   sort
                                   (map name)
                                   (str/join ""))
                              x))))
