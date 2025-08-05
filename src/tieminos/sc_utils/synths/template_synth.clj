(ns tieminos.sc-utils.synths.template-synth
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list]))

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
    (walk/prewalk (fn [x]
                    (if-let [mapping (params-map x)]
                      mapping
                      (cond
                        (ns-kw? "fx" x) (params x identity)
                        (ns-kw? "ugen" x) (params x identity)
                        (ns-kw? "dyn" x) (list 'as-> 'sig (params x identity))
                        :else x)))
                  synth-body)))
(comment
  ;; TODO levels and env-durs should ideally only belong to this particular instance(?)
  ;; TODO try parametrizing panning as will be needed, probably the most important thing to work on atm.
  (modify-body
   {:freq [1 2]
    :levels [0 1 1 0]
    :env-durs [1 1 1]
    :ugen/env '(o/env-gen (o/envelope levels env-durs))
    :shaper-limit 0.5
    :ugen/rev '(o/sine-shaper shaper-limit)
    :ugen/fx1 '(o/dist)
    :out 0}
   (qualify-body '(o/out 0
                         (o/sin-osc freq)
                         :ugen/rev
                         :ugen/fx1
                         (* :ugen/env)))))

(do
  (defn modify-params
    "Used when creating the synth's params vector"
    [params]
    #_(println "MP" params)
    (->> params
         (keep (fn [[k v]]
                 (cond
                   (or (ns-kw? "fx" k)
                       (ns-kw? "dyn" k)
                       (ns-kw? "ugen" k)) nil
                   (number? v) [(symbol (name k)) v]
                   (sequential? v) (map-indexed (fn [i v*]
                                                  [(symbol (str (name k) i)) v*])
                                                v))))
         flatten
         (into [])))

  (modify-params {:freq [1 2]
                  :levels [0 1 1 0]
                  :env-durs [1 1 1]
                  :ugen/env '(o/env-gen (o/envelope levels env-durs))
                  :shaper-limit 0.5
                  :ugen/rev '(o/sine-shaper shaper-limit)
                  :ugen/fx1 '(o/dist)
                  :out 0}))

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
    (ns-kw? "ugen" k) [:ugen (str arg)]
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
    (println body)
    (o/synth-form synth-symbol [(modify-params params-map) body])))

(defonce variations-data (atom {}))
(defonce synths-cache (atom {}))

(defn instance-symbol [synth-symbol]
  (println " instance-symbol" synth-symbol)
  (symbol (str synth-symbol (inc (get-in @variations-data [synth-symbol :count] -1)))))

(defn qualify-body
  [synth-body]
  (let [collides (ns-interns 'overtone.sc.ugen-collide-list)]
    (walk/postwalk (fn [x]
                     (if (symbol? x)
                       (if-let [res (resolve x)]
                         (cond
                           (number? (var-get res)) (var-get res) ;; constants
                           (collides x) (collides x)
                           :else res)
                         x)
                       x))
                   synth-body)))
(comment
  (qualify-body '(o/out out
                        (* (o/env-gen (o/env-perc) :action o/FREE)
                           (o/sin-osc freq)))))

(declare call-synth define-synth)

(defn get-synth-ns-string
  [synth-symbol]
  (str/replace (ns-resolve *ns* synth-symbol)
               #"#'" ""))

(defn add-variation-data!
  [namespaced-synth-string
   synth-body
   params-map
   analyzed-args]
  (swap! variations-data
         (fn [data]
           (let [synth-data (get data namespaced-synth-string
                                 {:default-params params-map
                                  :synth-body (qualify-body synth-body)})]
             (assoc data
                    namespaced-synth-string
                    (-> synth-data
                        (update :count (fnil inc 0))
                        (assoc-in [:variants analyzed-args] {:default-params params-map
                                                             :synth-body synth-body})))))))
(-> @variations-data)
(do
  (reset! variations-data {})
  (reset! synths-cache {})
  ;; NOTE IMPORTANT ths is promising, no macros!
  (defn make-synth-fn
    [synth-symbol params-map synth-body]
    ;; TODO should analyze args and memoize synths

    (when-not (ns-resolve *ns* synth-symbol)
      ;; TODO add getter
      (define-synth *ns* synth-symbol))

    (let [namespaced-synth-string (get-synth-ns-string synth-symbol)
          analyzed-args (analyze-ds-args namespaced-synth-string params-map)
          cached-synth (get-in @synths-cache [analyzed-args])]
      (if cached-synth
        cached-synth
        (let [synth-body* (qualify-body synth-body)
              [_s-name params ugen-form] (make-synth-form
                                          synth-symbol
                                          params-map
                                          synth-body*)
              _ (println "AS" params)
              synth (eval (list 'overtone.core/synth
                                (instance-symbol synth-symbol)
                                params
                                ugen-form))
              _ (println "AS1")]
          (swap! synths-cache assoc analyzed-args synth)
          ;; TODO prevent overwritting a synth from another namespace (i.e. namespace the symbol)
          (add-variation-data!
           namespaced-synth-string
           synth-body*
           params-map
           analyzed-args)
          synth))))
  (o/stop)
  (defn map-to-outs-seq
    [outs-seq sig]
    (map (fn [i sig] (o/out i sig))
         outs-seq
         sig))
  (make-synth-fn
    ;; synth name
   'sinpan
    ;; default args
   {:freq [500 900]
    :levels [0 1 0.1 1 0]
    :env-durs [1 3 1 1]
    :ugen/env '(o/env-gen (o/envelope levels env-durs) :action o/FREE)
    :shaper-limit 0.1
    :rev-mix 0
    :rev-room 2
    :ugen/rev '(o/free-verb rev-mix rev-room 0.3) ;; '(o/sine-shaper shaper-limit)
    :ugen/pan '(#(o/pan-az 4 % (o/lf-saw 0.3)))
    :ugen/fx1 '(o/distort)
    :outs [0]}
    ;; synth
   '(map-to-outs-seq
     outs
     (-> (o/saw freq)
         (o/mix)
         :ugen/fx1
         :ugen/pan
         :ugen/rev
         (* :ugen/env))))

  ;; synth call
  (let [outs [0 1]]
    (sinpan {:freq [599 800 900]
             :rev-mix 0.2
             :rev-room 0.5
             :outs outs
             :ugen/pan '(#(o/pan-az (count (set outs)) % (o/lf-saw 0.3)
                                    :width 1))
             :levels [0 1 0.5 0]
             :env-durs [1 1 10]}))
  #_((make-synth-fn
      'sini
      (merge {:freq [200 500] :out 0} {:amp 1})
      '(o/out out (* (o/env-gen (o/env-perc)
                                :action o/FREE) (o/sin-osc freq)))))
  #_(println "res:")
  #_(println @variations-data)
  #_(println @synths-cache))

(defn get-instance-data
  [synth-symbol params-map]
  (let [namespaced-synth-string (get-synth-ns-string synth-symbol)
        {:keys [default-params synth-body]} (get @variations-data namespaced-synth-string)
        merged-params (merge default-params params-map)
        analyzed-args (analyze-ds-args namespaced-synth-string merged-params)
        synth (or (get-in @synths-cache [analyzed-args])
                  (make-synth-fn synth-symbol merged-params synth-body))]
    {:synth synth :analyzed-args analyzed-args :merged-params merged-params}))

#_(get-instance-data 'sini {})

(defn call-synth
  [synth-symbol params-map]
  (println "SS" synth-symbol params-map)
  (let [{:keys [synth merged-params]} (get-instance-data (symbol synth-symbol) params-map)]
    (synth (modify-params2 merged-params))))
(comment

  (resolve 'sini)
  (get @variations-data 'sini)
  (call-synth 'sini {:freq [500]})
  (sini))

(defn define-synth
  [ns synth-symbol]
  (intern ns synth-symbol
          (fn
            ([] (call-synth synth-symbol {}))
            ([params-map] (call-synth synth-symbol params-map)))))

(comment
  (ns-unmap  *ns* 'sini)

  (intern *ns* 'hola 6)
  (def hola "hola")
  (-> hola))

(comment
  ((make-synth-fn
    'sin2i
    {:freq [1 2]
     :levels [0 1 1 0]
     :env-durs [1 1 1]
     :ugen/env '(o/env-gen (o/envelope levels env-durs))
     :shaper-limit 0.5
     :ugen/rev '(o/sine-shaper shaper-limit)
     :ugen/fx1 '(o/dist)
     :out 0}
    '(o/out 0
            (o/sin-osc freq)
            :ugen/rev
            :ugen/fx1
            (* :ugen/env)))))
