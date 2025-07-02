(ns tieminos.seq-utils.qwerty-velocity
  "Midi velocity, amplitude and decibel mappings for a US qwerty keyboard."
  (:require
   [clojure.edn :as edn]
   [clojure.math :refer [round]]
   [clojure.math :as math]
   [clojure.string :as str]
   [instaparse.core :as insta]
   [overtone.core :as o]))

(def ^:private default-key-levels [0 1 2])

(def config (atom {:key-levels default-key-levels}))

(def key-levels
  ["zxcvbnm"
   "asdfghjkl"
   "qwertyuiop"])

(def ^:private grammar (slurp "src/tieminos/seq_utils/parsers/velocity.grammar"))

(def ^:private parser (insta/parser grammar))

(defn- parse
  [str]
  (let [result (parser str)]
    (when (insta/failure? result)
      (println (insta/get-failure result))
      (throw (ex-info "Velocity parser error"
                      {:string str
                       :error (insta/get-failure result)})))
    result))

(defn make-key-map*
  [step-val-fn min* max* levels]
  (let [levels* (remove nil? (map #(nth key-levels % nil) (sort (set levels))))
        qwerty-str  (apply str levels*)
        total-steps (count qwerty-str)
        step-size (/ (- max* min*) (float total-steps))
        steps (map #(step-val-fn min* max* %)
                   (reverse (range  max* min* (* -1 step-size))))
        key-map (into {} (map (fn [ch step] (vector (str ch) step)) qwerty-str steps))]
    (fn [x] (or (key-map x)
                x))))

(def get-key-map (memoize make-key-map*))

(def midi-step (fn [_min* _max* val] (-> val round (min 127))))

#_(make-key-map* midi-step 1 127 [1])

(def amp-step (fn [min* max* val] (-> val (min max*) (max min*))))

#_(make-key-map* amp-step 0 1 [1])

(def db-step (fn [min* max* val] (-> val (min max*) (max min*) (o/db->amp))))

#_(->> (make-key-map* db-step -24 12 [1 2])
       (sort-by second))

(defn- do-interpolations
  [parsed-data]
  (let [parts (partition-by number? parsed-data)]
    (if (= 1 (count parts))
      parsed-data
      (->> parts
           (partition 3 2)
           ((fn [groups]
              (let [total-groups (count groups)]
                (map-indexed (fn [i group]
                               (let [[a [[_ inter]] b] group
                                     last-a (last a)
                                     first-b (first b)
                                     inter-len (inc (edn/read-string inter))
                                     step-size (/ (- first-b last-a)
                                                  inter-len)]

                                 (concat
                                  a
                                  (range (+ last-a step-size) first-b step-size)
                                  (if (= (inc i) total-groups) b ()))))
                             groups))))
           (apply concat)))))
(comment
  (db* (parse "al"))
  (db* (parse "a7l")))

(defn midi*
  ([key-str] (midi* 1 128 key-str))
  ([min* max* key-str] (midi* (@config :key-levels default-key-levels) min* max* key-str))
  ([levels min* max*  key-str]
   (->> key-str
        (map (get-key-map midi-step (int min*) (int max*) levels))
        do-interpolations
        (map math/round))))

(defmacro midi
  ([key-str] `(midi 1 128 ~key-str))
  ([min* max* key-str] `(midi ~(@config :key-levels default-key-levels) ~min* ~max* ~key-str))
  ([levels min* max* key-str]
   (into [] (midi* (eval levels) min* max* (parse (eval key-str))))))

(defn amp*
  ([key-str] (amp* 0 1 key-str))
  ([min* max* key-str] (amp* (@config :key-levels default-key-levels) min* max* key-str))
  ([levels min* max*  key-str]
   (->> key-str
        (map (get-key-map amp-step min* max* levels))
        do-interpolations)))

(defmacro amp
  ([key-str] `(amp 0 1 ~key-str))
  ([min* max* key-str] `(amp ~(@config :key-levels default-key-levels) ~min* ~max* ~key-str))
  ([levels min* max*  key-str]
   (into [] (amp* (eval levels) min* max* (parse (eval key-str))))))

(defn db*
  "Returns a list of amp values. So no mapping from db->amp is necessary."
  ([key-str] (db* -24 12 key-str))
  ([min* max* key-str] (db* (@config :key-levels default-key-levels) min* max* key-str))
  ([levels min* max*  key-str]
   (->> key-str
        (map (get-key-map db-step min* max* levels))
        do-interpolations)))

(defmacro db
  "Returns a list of amp values. So no mapping from db->amp is necessary."
  ([key-str] `(db -24 12 ~key-str))
  ([min* max* key-str] `(db ~(@config :key-levels default-key-levels) ~min* ~max* ~key-str))
  ([levels min* max*  key-str]
   (into [] (db* (eval levels) min* max* (parse (eval key-str))))))

(comment
  (amp "a7l")
  (amp "asdfghjkl")
  (db* (parse "aasdfl"))
  (interleave (midi "qwertyuiop")
              (midi "asdfhjkl")
              (midi "zxcvbnm")))

(comment
  (require '[time-time.dynacan.players.refrain.v2 :as rain.v2])
  (rain.v2/stop)
  (rain.v2/ref-rain
   :id :a
   :durs [1]
   :on-event (rain.v2/on-event
              (println "-----")
              (println (at-index (amp "a3g"))) ;; con interpolación de 2
              (println (at-index (amp "asdfg"))))))
