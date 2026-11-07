(ns tieminos.seq-utils.parsers.instruments-seqs-parser
  (:require
   [clojure.edn :as edn]
   [instaparse.core :as insta]
   [tieminos.utils :refer [wrap-at]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

(def grammar (slurp "src/tieminos/seq_utils/parsers/instruments_seqs_parsers.grammar"))
(def parser (insta/parser grammar))
(comment
  (parser "a*2!2")
  (parser "a*(1, 3)")
  (parser "ab [ab]!2 <qqq>*2")
  (parser "ab*3 [cd] ddd  e/2 e/4 e/5")
  (post-process-parsed-seq (parser "a*(3/2 3)"))
  (post-process-parsed-seq (parser "a<a[c*2]d>"))
  (post-process-parsed-seq (parser "a<ad>")))

(defn post-process-parsed-seq
  [parsed-seq]
  (mapcat (fn [x]
            (if (= :repeat (and (sequential? x) (first x)))
              (let [[_ sub-event _ [_ n]] x
                    n* (edn/read-string n)]
                (repeat n* sub-event))
              [x]))

          parsed-seq))
#_(post-process-parsed-seq ["a"
                            "b"
                            [:repeat [:chord "a" "b"] "!" [:posint "2"]]
                            [:ratchet [:chord "q" "q" "q"] [:op-ratchet "*"] [:posint "2"]]])

(declare build-play-fn)

;; TODO move the following processing functions to the parsing phase (post-process-parsed-seq)
(defn- dispatch-process-ratchet-data
  [ratchet-data]
  (first ratchet-data))

(defn- process-ratio-or-posint
  [ratio]
  (if (= :ratio (first ratio))
    (let [[_ [_ num] [_ denom]] ratio]
      (/ (edn/read-string num)
         (edn/read-string denom)))
    (edn/read-string (second ratio))))

(defmulti ^:private process-ratchet-data #'dispatch-process-ratchet-data)

(defmethod process-ratchet-data :ratchet-config
  [[_ ratio [_ times]]]
  (let [ratio* (process-ratio-or-posint ratio)]
    {:times (edn/read-string times)
     :ratio ratio*}))

(defmethod process-ratchet-data :posint
  [[_ times]]
  (let [times* (edn/read-string times)]
    {:times times*
     :ratio times*}))

(defn- build-ratchet-play-fn
  [rain-data [_ sub-event [_ op] ratchet-data] player-fn]
  #_(println [rain-data [_ sub-event [_ op] [_ times]] player-fn])
  (let [{:keys [times ratio]} (process-ratchet-data ratchet-data)
        op* (if (= op "/") * /)
        durs (repeat times (op* (:dur-s rain-data) ratio))]
    (fn [] (rain.v2/ref-rain
            :id (random-uuid)
            :durs durs
            :loop? false
            :on-event (fn [data]
                        ((build-play-fn (:event data) sub-event player-fn)))))))

(defn- build-subseq-play-fn
  [rain-data [_ & subevents] player-fn]
  (let [durs (repeat (count subevents)
                     (/ (:dur-s rain-data) (count subevents)))]

    (fn []
      (rain.v2/ref-rain
       :id (random-uuid)
       :durs durs
       :loop? false
       :on-event (fn [data]
                   ((build-play-fn (:event data)
                                   (nth subevents (:index (:event data)))
                                   player-fn)))))))

(defn- build-chord-play-fn
  [rain-data event player-fn]
  (fn [] (doseq [sub-event (rest event)]
           ((build-play-fn rain-data sub-event player-fn)))))

(defn- noop [_])
(defn- build-element-play-fn
  [_rain-data event player]
  (fn [] (player event)))

(defn build-play-fn
  [rain-data event player-fn]
  (cond
    (= :ratchet (first event)) (build-ratchet-play-fn rain-data event player-fn)
    (= :subseq (first event))  (build-subseq-play-fn rain-data event player-fn)
    (= :chord (first event))   (build-chord-play-fn rain-data event player-fn)
    (char? (first event))      (build-element-play-fn rain-data event player-fn)
    :else                      noop))

(defn- parse
  [str]
  (let [result (parser str)]
    (when (insta/failure? result)
      (println (insta/get-failure result))
      (throw (ex-info "evseq pattern parser error"
                      {:string str
                       :error (insta/get-failure result)})))
    result))

(defmacro evseq [pattern & body]
  (let [body*      (into body nil)
        parsed-pattern (parse (eval pattern))
        events-seq (into [] (post-process-parsed-seq parsed-pattern))]
    #_(println parsed-pattern)
    `(let [event# (wrap-at (:index ~'data) ~events-seq)
           player# (fn [case*] (case case*  ~@body* nil))]
       ((build-play-fn ~'data event# player#)))))

(comment
  (macroexpand-1 '(evseq (str "ab" "c")
                         "a" (println "hola" (rainseq (lin 1 2 3))))))
(defmacro evseqs [body-map & patterns]
  (let [body (->> body-map seq (apply concat))
        body* (into body nil)]
    (mapv
     (fn [pattern] `(evseq ~pattern ~@body*))
     patterns)))
(macroexpand-1 '(evseqs {"a" (println "hola")
                         "b" (println "adios")}
                        "abb"))

(comment
  (require '[tieminos.seq-utils.core :refer [** choose lin rainseq] :rename {rainseq rseq}]
           '[tieminos.seq-utils.qwerty-velocity :as qwerty]
           '[tieminos.synths :as s])

  (rain.v2/stop)

  (rain.v2/ref-rain
   :id :ex1
   :durs [1]
   :loop? false
   :on-event (rain.v2/on-event
              (evseq "<b>*2"
                     "a" (s/low :freq (* 100 (rseq (lin 1 9/8 5/4))))
                     "b" (s/sharp-plate :freq (* 100 (rseq (lin 1 2 3)))))))
  (rain.v2/ref-rain
   :id :hola
   :durs [1/4]
   :on-event (rain.v2/on-event
              (evseq "ab*3 [cd] a/3b e/2 e/4 e/5"
                     "a" (s/low :freq (* 100 (rseq (lin 1 2))))
                     "b" (s/sharp-plate :freq (* 100 (rseq (lin 1 2 5 1))))
                     "c" (s/noise-tone :freq (* 800 (rseq (choose 1 7/4 2)))
                                       :amp 0.7
                                       :dcy (rseq (** 0.5 (lin 1 2 3))))
                     "d" (s/sharp-plate :freq (* 100 (rseq (lin 4 3 2 7 7/4)))
                                        :amp (rseq (qwerty/db 0.5 2 "cirkgdamnbcx"))
                                        :atk 0 :dcy 4)
                     "e" (do (s/low :freq 80)
                             (s/low :freq (* 80
                                             (rseq (lin 1 2 4))
                                             (rseq (lin 2 5 2 7)))
                                    :dcy (* 1/4 (rseq [2 5 2 7 1]))
                                    :amp (rseq (qwerty/amp "afjvlvkpv")))))))
  (rain.v2/ref-rain
   :id :evseqs-test
   :durs [1 1]
   :loop? true
   :on-event (rain.v2/on-event
              (evseqs {"a" (println "hola")
                       "b" (println "adios")
                       "c" (println "cccccccccc")
                       "d" (println "d")}
                      "a<b*2cc>"
                      "d--"))))
