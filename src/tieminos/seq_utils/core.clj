(ns tieminos.seq-utils.core
  (:refer-clojure :exclude [rand])
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.string :as str]
   [tieminos.utils :refer [wrap-at]]))

;; XO
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
  ([xo-str index]
   (when-not (zero? (count xo-str))
     (let [index-set (parse-xo xo-str)]
       (index-set (mod index (count xo-str)))))))

;; melodic sequencing
(defonce linear-state (atom {}))

(defn lin
  ([& coll]
   (let [id (try (= "id" (namespace (first coll)))
                 (catch Exception _ nil))
         coll* (if id (rest coll) coll)]
     (with-meta coll*
       {::linear? true :linear/id (or id coll)}))))

(defn lin* [coll] (apply lin coll))

(defn rand
  [& coll]
  (with-meta coll {::rand? true}))

(defn rand* [coll] (apply rand coll))

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
    (if (map? coll)
      (weighted coll)
      (wrap-at index* coll))))

(defn mseq
  [index melodic-sequence]
  (loop [sequence*  melodic-sequence]
    (let [item (get-next-item index sequence*)]
      (if (or (sequential? item) (map? item))
        (recur item)
        item))))

(comment
  ;; TODO add tests
  (reset! linear-state {})
  #_(map
     #(mseq % [0 1 [:a :b :c] (lin [2 3 4 5 6 7])])
     (range 20))
  (map
   #(mseq % [[-1 -2 -3]
             {(lin :A :B) 3, (rand 5 6 7) 1}])
   (range 20))

  (let [my-sequence
        [0 -1 [-2 -3] {(lin 1 2 3 4) 5, (rand :a :b :c) 2}]]
    (map #(mseq % my-sequence)
         (range 20)))

  (let [my-sequence [:a (lin :id/a :c :C)]
        my-sequence2 [:b :d (lin :id/a :c :C)]]
    (map (fn [i] [(mseq i my-sequence)
                  (mseq i my-sequence2)])
         (range 5))))

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
