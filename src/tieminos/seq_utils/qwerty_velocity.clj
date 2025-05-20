(ns tieminos.seq-utils.qwerty-velocity
  "Midi velocity, amplitude and decibel mappings for a US qwerty keyboard."
  (:require
   [clojure.math :refer [round]]
   [overtone.core :as o]))

(def ^:private default-key-levels [0 1 2])

(def config (atom {:key-levels default-key-levels}))

(def key-levels
  ["zxcvbnm,./"
   "asdfghjkl;'"
   "qwertyuiop"
   "`1234567890-="])

(defn make-key-map*
  [step-val-fn min* max* levels]
  (let [levels* (remove nil? (map #(nth key-levels % nil) (sort (set levels))))
        qwerty-str  (apply str levels*)
        total-steps (count qwerty-str)
        step-size (/ (- max* min*) (float total-steps))
        steps (map #(step-val-fn min* max* %)
                   (reverse (range  max* min* (* -1 step-size))))]
    (into {} (map vector qwerty-str steps))))

(def get-key-map (memoize make-key-map*))

(def midi-step (fn [_min* _max* val] (-> val round (min 127))))

#_(make-key-map* midi-step 1 127 [1])

(def amp-step (fn [min* max* val] (-> val (min max*) (max min*))))

#_(make-key-map* amp-step 0 1 [1])

(def db-step (fn [min* max* val] (-> val (min max*) (max min*) (o/db->amp))))

(->> (make-key-map* db-step -24 12 [1 2])
     (sort-by second))

(defn midi
  ([key-str] (midi 1 128 key-str))
  ([min* max* key-str] (midi (@config :key-levels default-key-levels) min* max* key-str))
  ([levels min* max*  key-str]
   (->> key-str
        (map (get-key-map midi-step (int min*) (int max*) levels))
        (remove nil?))))

(defn amp
  ([key-str] (amp 0 1 key-str))
  ([min* max* key-str] (amp (@config :key-levels default-key-levels) min* max* key-str))
  ([levels min* max*  key-str]
   (->> key-str
        (map (get-key-map amp-step min* max* levels))
        (remove nil?))))

(defn db
  "Returns a list of amp values. So no mapping from db->amp is necessary."
  ([key-str] (db -24 12 key-str))
  ([min* max* key-str] (db (@config :key-levels default-key-levels) min* max* key-str))
  ([levels min* max*  key-str]
   (->> key-str
        (map (get-key-map db-step min* max* levels))
        (remove nil?))))

(comment
  (amp "asdfghjkl")
  (db "asdfghjkl")
  (interleave (midi "qwertyuiop")
              (midi "asdfhjkl")
              (midi "zxcvbnm")))
