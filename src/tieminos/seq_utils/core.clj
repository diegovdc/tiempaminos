(ns tieminos.seq-utils.core
  "Experimental isorhythymic sequencing library. The main function is `mseq`, but also contains a simple implementation of xo logic."
  (:refer-clojure :exclude [rand + - * /])
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [tieminos.utils :refer [wrap-at]]
   [time-time.standard :refer [rotate]]))

;;;;;;;
;; XO
;;;;;;;

(defn- parse-xo
  [xo-str]
  (-> xo-str
      (str/replace #" " "")
      (str/split #"")
      (->> (map-indexed (fn [i x]
                          (if (= x "x") i nil)))
           (remove nil?)
           set)))

(defn xo
  ([xo-str]
   (let [index-set (parse-xo xo-str)]
     (fn [index]
       (when-not (zero? (count xo-str))
         (index-set (mod index (count xo-str)))))))
  ([xo-str index]
   (when-not (zero? (count xo-str))
     (let [index-set (parse-xo xo-str)]
       (index-set (mod index (count xo-str)))))))

;;;;;;;;;;;;;;;;;;;;;;
;; melodic sequencing
;;;;;;;;;;;;;;;;;;;;;;

(defn- concat-chars
  [chars]
  (apply str chars))

(defn rot [n xs]
  (let [meta* (meta xs)]
    (cond-> (rotate xs n)
      (string? xs) concat-chars
      ;; preserve meta for lin/rand/etc.
      meta* (with-meta meta*))))

(defonce linear-state (atom {}))

(defn lin
  "Ensures items are returned in the order they appear in the arge list.
  This is done by having an internal call count in the `linear-state` atom which is what keeps track of the returned value.

  NOTE: Therefore `lin` is stateful, so there are some caveats:

  1. If an `:id/<something>` is provided as the first value different `lin` calls will share a common count, even if the provided collections are different.

  2. Else the id that will be used is the collection itself. So calls to a `lin` with the same args will share the same call count.

  The second conditions allows `lin` to be useful (i.e. instantiated) within a `refrain` without having to specify an id key. This requirement defines the first condition, i.e.  how to have the same collection behave independently."
  ([& coll]
   (let [id? (try (= "id" (namespace (first coll)))
                  (catch Exception _ false))
         id (if id? (first coll) coll)
         coll* (into [] (if id? (rest coll) coll))]
     (with-meta  coll*
       {::linear? true :linear/id (or id coll)}))))

(defn- lin? [x] (::linear? (meta x)))

(defn choose
  [& coll]
  (with-meta (into [] coll) {::rand? true}))

(defn choose?
  [coll]
  (::rand?  (meta coll)))

;; operations
(defn op [f] (fn [& xs] (with-meta xs  {::op? true :op/fn f})))

(def plus (op clojure.core/+))

(def + plus)
(def ++ plus)

(def minus (op clojure.core/-))
(def - minus)
(def -- minus)

(def mult (op clojure.core/*))
(def * mult)
(def ** mult)
(def div (op clojure.core//))
(def / div)

(declare mseq)
(defn- do-op [index op]
  (let [f (-> op meta :op/fn)
        values (mapv (fn [x]
                       (if (number? x)
                         x
                         (mseq index x))) op)]
    (apply f values)))

(defn- ensure-vector
  [coll]
  (if (vector? coll) coll (into [] coll)))

(defn- port-meta
  [original-coll target-coll]
  (let [meta* (meta original-coll)]
    (if-not meta*
      target-coll
      (with-meta
        target-coll
        (cond-> meta*
          (-> meta* :linear/id (= original-coll)) (assoc :linear/id target-coll))))))

;; palindrome
(defn mirror [coll]
  (if-not (sequential? coll)
    (throw (ex-info "`palindrome` requires a `sequential?`" {:coll coll}))
    (if (choose? coll)
      coll
      (let [coll* (into (ensure-vector coll)
                        (->> coll
                             reverse
                             (drop 1)
                             (drop-last 1)))]

        (port-meta coll coll*)))))

(defn mirror2 [coll]
  (let [coll* (walk/postwalk
               (fn [x] (if (sequential? x) (mirror x) x))
               coll)]
    (port-meta coll coll*)))

(defn rev [coll]
  (if-not (sequential? coll)
    (throw (ex-info "`rev` requires a `sequential?`" {:coll coll}))
    (let [coll* (into [] (reverse coll))]
      (port-meta coll coll*))))

(defn rev2
  "Deep reverse"
  [coll]
  (let [coll* (-> (walk/postwalk
                   (fn [x] (if (sequential? x) (rev x) x))
                   coll))]
    (port-meta coll coll*)))

(def ^:private inc* (fnil inc -1))

(defn- get-next-special-index!
  [coll]
  (let [seq-meta (meta coll)
        id (:linear/id seq-meta)]
    (cond
      (::linear? seq-meta) (get (swap! linear-state update id inc*) id)
      (::rand? seq-meta) (rand-int (count coll)))))

(defn- get-next-item [index coll]
  (let [seq-meta (meta coll)
        id (:linear/id seq-meta)
        index* (cond
                 (::linear? seq-meta) (get (swap! linear-state update id inc*) id)
                 (::rand? seq-meta) (rand-int (count coll))
                 :else index)]
    (cond
      (map? coll)      (weighted coll)
      (::op? seq-meta) (do-op index coll)
      :else (wrap-at index* coll))))

(defn mseq
  ([index melodic-sequence] (mseq nil index melodic-sequence))
  ([id index melodic-sequence]
   (loop [sequence* melodic-sequence]
     (let [item (get-next-item index sequence*)]
       (if (or (sequential? item) (map? item))
         (recur item)
         item)))))

(comment

  ;; TODO  seqcat
  (defn seqcat
    "Append a sequence to another sequence"
    [coll]
    (with-meta coll
      {::seqcat? true :seqcat/length (count coll)}))

  ;; basic usage
  [1 2 3 (seqcat (lin 4 5))] ;; 1 2 3 4 5 1 2 3 4 5 - 0,0 1,0 2,0 3,-1 4=3,-1, 5=4,-1...
  [1 2 3 (seqcat (lin 4 (lin 5 6)))]          ;; 1 2 3 4 5 1 2 3 4 6
  [1 2 3 (seqcat (lin 4 (seqcat (lin 5 6))))] ;; 1 2 3 4 5 6

  ;; seqcat would need to be able to manipulate how mseq maps an index as well a keeping some sort of id of it's own
  ;; seems fairly complex

  ;; more relevant use-case
  [1 2 3 (every 4 (seqcat (lin 4 5)))] ;; 1 2 3 1 2 3 1 2 3 1 2 3 4 5
  (concat (flatten (repeat 4 [1 2 3])) [(lin 4 5)]))

(comment
  ;; TODO convert into test

  (defn at-i-degs [at-i] (at-i [0 2 0 (at-i [2 3]) (at-i [7 4]) (at-i [4 5]) 3]))
  (def degs [0 2 0 [2 3] [7 4] [4 5] 3])

  (= (map
      (fn [i] (at-i-degs (partial wrap-at i)))
      (range 1000))
     (map
      (fn [i] (mseq i degs))
      (range 1000))))

(comment
  ;; WIP
  ;; TODO memoize mseqs inside refrains, by saving then into atoms
  (def id->seqs (atom {}))
  (def known-seqs (atom {}))

  (defmacro memoize-seqs
    [x]
    (let [s*# (@known-seqs x)
          s# (or s*# (random-uuid))]
      (when-not s*#
        (swap! id->seqs assoc s# (eval x))
        (swap! known-seqs assoc x s#))
      `(println ((deref id->seqs) ~s#))))

  (macroexpand-1 `(memoize-seqs 5))
  (memoize-seqs [1 (lin 3 4)])
  (mseq 1 (@id->seqs  #uuid "96945ed6-e59f-429c-be66-38ec452c0479"))

  (-> @known-seqs))
