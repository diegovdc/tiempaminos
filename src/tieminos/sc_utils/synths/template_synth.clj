(ns tieminos.sc-utils.synths.template-synth
  (:require
   [clojure.walk :as walk]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]))

(defn ns-kw?
  [kw-ns-str kw]
  (and (keyword? kw) (= kw-ns-str (namespace kw))))

(defn modify-body [params synth-body]
  #_(println "MB" params synth-body)
  (let [params-map (->> params
                        (map (fn [[k v]]
                               (cond
                                 (number? v) [(symbol k) (symbol k)]
                                 (sequential? v) [(symbol k) (into [] (map-indexed (fn [i v*]
                                                                                     (symbol (str (name k) i)))
                                                                                   v))])))
                        (into {}))]

    #_(timbre/spy :info)
    (walk/postwalk (fn [x]
                     (if-let [mapping (params-map x)]
                       mapping
                       (cond
                         (ns-kw? "fx" x) (params x identity)
                         (ns-kw? "dyn" x) (list 'as-> 'sig (params x identity))
                         :else x)))
                   synth-body)))

(defn modify-params
  "Used when creating the synth's params vector"
  [params]
  #_(println "MP" params)
  (->> params
       (keep (fn [[k v]]
               (cond
                 (number? v) [(symbol (name k)) v]
                 (sequential? v) (map-indexed (fn [i v*]
                                                [(symbol (str (name k) i)) v*])
                                              v))))
       flatten
       (into [])))

(defn modify-params2
  "Used when calling the synth"
  [params]
  (println "MP2" params)
  (->> params
       (mapv (fn [[k v]]
               (cond
                 (number? v) {k v}
                 (vector? v) (map-indexed (fn [i v*]
                                            {(keyword (str (name k) i)) v*})
                                          v))))
       flatten
       (apply merge)))

#_(defmacro make-synth [params-map synth-body]
    (let [[s-name# params ugen-form]
          (let [body (eval (modify-body params-map synth-body))]
            (println "==============" body)
            (o/synth-form 's-name [(modify-params params-map) body]))]
      `(o/synth ~s-name# ~params ~ugen-form)))

(defn analyze-arg
  [k arg]
  (cond
    (number? arg) [:number]
    (sequential? arg) [:seq (count arg)]
    (ns-kw? "fx" k) [:fx/fn (str arg)]
    (ns-kw? "dyn" k) [:dyn/fn (str arg)]
    :else (throw (ex-info "Don't know how to analyze arg:" {:key k
                                                            :arg arg}))))

(defn analyze-ds-args [synth-symbol m]
  (->> m
       (mapv (fn [[k arg]]
               [k (analyze-arg k arg)]))
       (into [(str synth-symbol)])))

(defn make-synth-form
  [synth-symbol params-map synth-body]
  (let [body (modify-body params-map synth-body)]
    (o/synth-form synth-symbol [(modify-params params-map) body])))

(defonce variations-data (atom {}))
(defonce synths-cache (atom {}))

(defn instance-symbol [synth-symbol]
  (println " instance-symbol" synth-symbol)
  (symbol (str synth-symbol (inc (get-in @variations-data [synth-symbol :count] -1)))))

(do

  #_(reset! variations-data {})
  #_(reset! synths-cache {})

  ;; NOTE IMPORTANT ths is promising, no macros!
  (defn make-synth-fn
    [synth-symbol params-map synth-body]
    ;; TODO should analyze args and memoize synths

    (let [analyzed-args (analyze-ds-args synth-symbol params-map)
          _ (println analyzed-args)
          cached-synth (get-in @synths-cache [analyzed-args])]
      (if cached-synth
        (do
          (println "CS" cached-synth)
          cached-synth)
        (let [[_s-name params ugen-form] (make-synth-form synth-symbol params-map synth-body)
              _ (println "before" ugen-form)
              synth (eval (list 'overtone.core/synth
                                (instance-symbol synth-symbol)
                                params ugen-form))
              _ (println "after")]
          (swap! synths-cache assoc analyzed-args synth)
          ;; TODO add getter
          #_(eval (list 'defn synth-symbol [] (list println "getting the synth:" synth-symbol)))
          ;; TODO prevent overwritting a synth from another namespace (i.e. namespace the symbol)
          (swap! variations-data (fn [data]
                                   (let [synth-data (get data synth-symbol {:default-params params-map :synth-body synth-body})]
                                     (assoc data synth-symbol
                                            (update synth-data :count (fnil inc 0))))))
          synth))))

  ((make-synth-fn
    'sini
    (merge {:freq [200 500] :out 0} {:amp 1})
    '(o/out out (* (o/env-gen (o/env-perc) :action o/FREE) (o/sin-osc freq)))))
  #_(println "res:")
  #_(println @variations-data)
  #_(println @synths-cache))

(defn get-instance
  [synth-symbol params-map]
  (let [{:keys [default-params synth-body]} (get @variations-data synth-symbol)
        merged-params (merge default-params params-map)
        analyzed-args (analyze-ds-args synth-symbol merged-params)
        synth (or (get-in @synths-cache [analyzed-args])
                  (make-synth-fn synth-symbol merged-params synth-body))]
    synth))

(defn call-synth
  [synth-symbol params-map]
  (let [synth (get-instance synth-symbol params-map)]
    (println synth)
    (synth (modify-params2 params-map))))

(call-synth 'sini {:freq [200 600 700 900]})
