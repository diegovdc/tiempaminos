(ns tieminos.seq-utils.core
  "Experimental isorhythymic sequencing library. The main function is `mseq`, but also contains a simple implementation of xo logic."
  (:refer-clojure :exclude [rand + - * /])
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [taoensso.timbre :as timbre]
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

(defn ret
  "Returns the contained items. Useful for returning chords."
  [& x]
  (with-meta x {::return? true}))

(defn- graph*
  [{:keys [id auto? start-node]
    :as config
    :or {auto? true}} m]
  (let [g (->> m
               (map (fn [[k v]]
                      (when-not (seq v)
                        (throw (ex-info "A graph should not have terminal nodes."
                                        {:graph m
                                         :empty-node k})))
                      [k (into #{} v)]))
               (into {}))]
    (with-meta g {::graph? true
                  :graph/id (or id {:config config :graph g})
                  :graph/start-node (when (m start-node) start-node)
                  :graph/autonomous? (boolean auto?)})))

(defn- dispatch-graph
  ([_m] :graph-only)
  ([x _m] (cond (keyword? x) :id
                (map? x) :config
                :else :unknown)))

(defmulti graph #'dispatch-graph)

(defmethod graph :graph-only
  [m]
  (graph* {} m))

(defmethod graph :id
  [id m]
  (graph* {:id id} m))

(defmethod graph :config
  [config m]
  (graph* config m))

(defonce graph-state (atom {}))

(defn- get-next-graph-node!
  [prev-val graph]
  (let [{:keys [graph/id graph/autonomous? graph/start-node]} (meta graph)
        graph-in-state? (@graph-state id)
        next-node (if (and (not graph-in-state?) start-node)
                    start-node
                    (let [prev-node* (when (and (not autonomous?) (graph prev-val))
                                       prev-val)
                          prev-node (or prev-node*
                                        (get @graph-state id)
                                        (-> graph keys rand-nth))]
                      (rand-nth (seq (get graph prev-node)))))]
    (swap! graph-state assoc id next-node)
    next-node))

(defn- mancha*
  [{:keys [id
           start-node
           max-mem
           drop%
           get-prev-node-fn
           get-next-node-fn
           get-start-node-fn]
    :or {max-mem 5
         drop% 0.5
         get-prev-node-fn (fn [prev-nodes] (rand-nth prev-nodes))
         get-next-node-fn (fn [prev-node mancha] (rand-nth (seq (get mancha prev-node))))
         get-start-node-fn (fn [mancha] (rand-nth (keys mancha)))}
    :as config} m]
  (let [g (->> m
               (map (fn [[k v]]
                      (when-not (seq v)
                        (throw (ex-info "A mancha-graph should not have terminal nodes."
                                        {:mancha-graph m
                                         :empty-node k})))
                      [k (into #{} v)]))
               (into {}))]
    (with-meta g {::mancha? true
                  :mancha/id (or id {:config config :mancha g})
                  :mancha/max-mem max-mem
                  :mancha/drop% drop%
                  :mancha/start-node (when (m start-node) start-node)
                  :mancha/get-prev-node-fn get-prev-node-fn
                  :mancha/get-next-node-fn get-next-node-fn
                  :mancha/get-start-node-fn get-start-node-fn})))

(defn- dispatch-mancha
  ([_m] :mancha-only)
  ([x _m] (cond (keyword? x) :id
                (map? x) :config
                :else :unknown)))

(defmulti mancha #'dispatch-mancha)

(defmethod mancha :mancha-only
  [m]
  (mancha* {} m))

(defmethod mancha :id
  [id m]
  (mancha* {:id id} m))

(defmethod mancha :config
  [config m]
  (mancha* config m))
(def mancha-state (atom {}))

;; TODO add tests
(defn- get-next-mancha-node!
  [mancha]
  (let [{:keys [mancha/id
                mancha/start-node
                mancha/max-mem
                mancha/drop%
                mancha/get-prev-node-fn
                mancha/get-next-node-fn
                mancha/get-start-node-fn]} (meta mancha)
        {prev-nodes :nodes
         :as current-mancha} (@mancha-state id)
        next-node (cond
                    (seq prev-nodes) (let [prev-node (get-prev-node-fn prev-nodes)]
                                       (get-next-node-fn prev-node mancha))
                    (and (not (seq prev-nodes)) start-node) start-node
                    :else (get-start-node-fn mancha))
        nodes* (conj prev-nodes next-node)
        nodes (take max-mem
                    (if (and (> (count nodes*) 1)
                             (> drop% (clojure.core/rand)))
                      (drop-last 1 nodes*)
                      nodes*))
        current-mancha* (assoc current-mancha :nodes nodes)]

    (swap! mancha-state assoc id current-mancha*)
    next-node))

;;;;;;;;;;;;;;;;;;;;
;; Transformations
;;;;;;;;;;;;;;;;;;;

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
                         (mseq index x)))
                     op)]
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

(defn mirror2
  "Deep mirror (mirrors all nested sequences as well)"
  [coll]
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
  "Deep reverse (reverses all nested sequences as well)"
  [coll]
  (let [coll* (-> (walk/postwalk
                   (fn [x] (if (sequential? x) (rev x) x))
                   coll))]
    (port-meta coll coll*)))

(def ^:private inc* (fnil inc -1))

(defn- get-next-item
  [prev-item index coll]
  (let [seq-meta (meta coll)
        id (:linear/id seq-meta)
        index* (cond
                 (::linear? seq-meta) (get (swap! linear-state update id inc*) id)
                 (::rand? seq-meta) (rand-int (count coll))
                 :else index)]
    (cond
      (::graph? seq-meta) (get-next-graph-node! prev-item coll)
      (::mancha? seq-meta) (get-next-mancha-node! coll)
      (::op? seq-meta)    (do-op index coll)
      (map? coll)         (weighted coll)
      :else (wrap-at index* coll))))

(def mseq-state (atom {}))

(defn mseq
  ([index item-seq] (mseq item-seq index item-seq))
  ([id index melodic-sequence]
   (if (::return? (meta melodic-sequence))
     melodic-sequence
     (let [{:keys [prev-item]} (@mseq-state id)]
       (loop [sequence* melodic-sequence]
         (let [item (get-next-item prev-item index sequence*)]
           (cond
             (::return? (meta item)) item
             (or (sequential? item) (map? item)) (recur item)
             :else (do
                     (swap! mseq-state assoc-in [id :prev-item] item)
                     item))))))))

(defn gen-seq [len item-seq]
  (mapv #(mseq % item-seq) (range len)))

(comment
  (let [g (graph {1 [2 3], 2 [3], 3 [1]})
        my-seq [6 g g g]]
    (gen-seq 20 my-seq)))

;;;;;;;;;;;;;;;;;
;; rainseq
;;;;;;;;;;;;;;;;;

(defonce id->seqs (atom {}))
(defonce known-seqs (atom {}))
(defn memoize-seq
  "Eval and save the seq to `know-seqs`, and returns a function that when called returns the seq."
  [item-seq]
  #_(timbre/debug "memoize-seq called...")
  (let [s* (@known-seqs item-seq)
        seq-id (or s* (random-uuid))]
    (when-not s*
      (timbre/debug "memoizing sequence:" item-seq "as" seq-id)
      (swap! id->seqs assoc seq-id (eval item-seq))
      (swap! known-seqs assoc item-seq seq-id))
    seq-id))

(comment
  (timbre/set-level! :debug)
  (-> @id->seqs)
  (-> @known-seqs)
  (do (reset! id->seqs {})
      (reset! known-seqs {})))

(defmacro rainseq
  "Within a `ref-rain`, evaluate an `item-seq` only once and memoize it to the `known-seqs` atom.
  Must use the `on-event` macro. When called the function, then derefernces the `item-seq`, calls `mseq` with it and passes in the current `index`.

  NOTE: Due to macro limitations, the `item-seq` or parts of it cannot be defined in a `let` or a `def`. If that is necessary, use `mseq`. With some changes values could be defed, but then this requires the form to be evaled twice, and `memoize-seq` to be updated without the `when-not*` conditional. That could work but is neither ergonomic nor a predictable behaviour."
  [item-seq]
  `(mseq ~'i (@id->seqs ~(memoize-seq item-seq))))

;;;;;;;
;; WIP
;;;;;;;

(comment
  ;; IDEA
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
