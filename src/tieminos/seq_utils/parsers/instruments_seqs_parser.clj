(ns tieminos.seq-utils.parsers.instruments-seqs-parser
  (:require
   [clojure.edn :as edn]
   [instaparse.core :as insta]
   [tieminos.seq-utils.core :refer [lin rainseq]]
   [tieminos.utils :refer [wrap-at]]))

(do

  (def grammar (slurp "src/tieminos/seq_utils/parsers/instruments_seqs_parsers.grammar"))
  (def parser
    (insta/parser
     grammar))

  (parser "a*2!2")
  (parser "ab [ab]!2 [qqq]*2"))

(do
  (defn post-process-parsed-seq
    [parsed-seq]
    (mapcat (fn [x]
              (if (= :repeat (and (sequential? x) (first x)))
                (let [[_ sub-event _ [_ n]] x
                      n* (edn/read-string n)]
                  (repeat n* sub-event))
                [x]))

            parsed-seq))
  (post-process-parsed-seq ["a"
                            "b"
                            [:repeat [:chord "a" "b"] "!" [:posint "2"]]
                            [:ratchet [:chord "q" "q" "q"] [:op-ratchet "*"] [:posint "2"]]]))
(do
  (defn make-fn-map
    [key-fns]
    (when (not (even? (count key-fns)))
      (throw (ex-info "`key-fns` must be pairs of char and function" {:key-fns key-fns})))
    (->> key-fns
         (partition 2 2)
         (map (fn [[k f]]
                [(str k) f]))
         (into {})))
  ((get (make-fn-map ["a" #(println "hola")])
        "a")))

(declare build-play-fn)
(defn build-ratchet-play-fn
  [[_ sub-event [_ op] [_ times]] fns-map]

  (println "will play ratched" op times)
  (build-play-fn sub-event fns-map))

(defn build-chord-play-fn
  [event fns-map]
  (fn [i] (doseq [sub-event (rest event)]
            ((build-play-fn sub-event fns-map) i))))

(defn noop [_])
(defn build-element-play-fn
  [event fns-map]
  (get fns-map event noop))

(do
  (defn build-play-fn
    [event fns-map]
    (cond
      (= :ratchet (first event)) (build-ratchet-play-fn event fns-map)
      (= :chord (first event))   (build-chord-play-fn event fns-map)
      (char? (first event))      (build-element-play-fn event fns-map)
      :else                      noop))

  #_((build-play-fn
      [:ratchet [:chord "q" "q" "q"] [:op-ratchet "*"] [:posint "2"]]))
  #_((build-play-fn
      [:chord "q" "q" "q"])))

#_((build-play-fn  [:chord "c"] {"c" #(println "cccc")}))

(defn play-event
  [fns-map events-seq {:keys [index] :as data}]
  (let [ev (wrap-at index events-seq)
        f (build-play-fn ev fns-map)]
    (when f (f data))))

(defonce ev-players (atom {}))
(defonce ev-player-ids (atom {}))
(-> ev-players)
#_(reset! ev-players {})

(defmacro evseq
  [pattern & key-fns]
  (let [player (partial
                play-event
                (->> (make-fn-map key-fns)
                     (map (fn [[k f]]
                            (let [[_ _ f*] f]
                              [k (eval (list 'fn ['data]
                                             ;; TODO: rainseq should use `data` as well
                                             (list 'let '[i (:index data)] f*)))])))
                     (into {}))
                (into [] (post-process-parsed-seq (parser pattern))))
        ev-player-id [pattern key-fns]
        id (get @ev-player-ids ev-player-id (random-uuid))]
    (swap! ev-player-ids assoc ev-player-id id)
    (swap! ev-players assoc id player)
    `((get @ev-players ~id) ~'data)))

(-> @ev-players)
(macroexpand-1
 '(evseq "ab[cd]"
         "a" #(println "a" (rainseq [1 2 3]))
         "b" #(println "adios")
         "c" #(println "chachacha")))

(comment
  (require '[time-time.dynacan.players.refrain.v2 :as rain.v2])
  ((eval 'map) inc [1 2 3])
  (doseq [i (range 6)]
    (let [data {:index i}]
      (evseq "ab[cd]"
             "a" #(println "a" (rainseq (lin 1 2 3)))
             "b" #(println "adios")
             "c" #(println "chachacha")
             "d" #(println "dhachacha"))))

  (rain.v2/stop)
  (rain.v2/ref-rain
   :id :hola
   :durs [1]
   :on-event (rain.v2/on-event
              (evseq "ab[cd] e!3"
                     "a" #(println "a" (rainseq (lin 1 2 3)))
                     "b" #(println "b")
                     "c" #(println "c")
                     "d" #(println "d")
                     "e" #(println "e")))))
