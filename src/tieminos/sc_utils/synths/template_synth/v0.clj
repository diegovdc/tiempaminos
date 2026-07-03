(ns tieminos.sc-utils.synths.template-synth.v0
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list]
   [taoensso.timbre :as timbre]))

(defn ns-kw?
  [kw-ns-str kw]
  (and (keyword? kw) (= kw-ns-str (namespace kw))))

(defn resolve-frag [x]
  (if (-> x meta :fragment)
    (var-get x)
    x))

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

    (walk/prewalk (fn [x]
                    (if-let [mapping (params-map x)]
                      mapping
                      (resolve-frag
                       (cond
                         (ns-kw? "fx" x) (params x identity)
                         (ns-kw? "ugen" x) (params x identity)
                         (ns-kw? "dyn" x) (list 'as-> 'sig (params x identity))
                         :else x))))
                  synth-body)))
(comment
  ;; TODO levels and env-durs should ideally only belong to this particular instance(?)
  ;; TODO try parametrizing panning as will be needed, probably the most important thing to work on atm.
  (let [params {:freq [1 2]
                :levels [0 1 1 0]
                :env-durs [1 1 1]
                :ugen/env '(o/env-gen (o/envelope levels env-durs))
                :shaper-limit 0.5
                :ugen/pan #'panny
                :ugen/rev '(o/sine-shaper shaper-limit)
                :ugen/fx1 '(o/dist)
                :out 0}]
    (modify-body
     params
     (qualify-body params '(o/out 0
                                  (o/sin-osc freq)
                                  :ugen/pan
                                  :ugen/rev
                                  :ugen/fx1
                                  (* :ugen/env))))))

(def freq 5432)

(def ^:private SC-IDABLES
  #{overtone.sc.sample.Sample
    overtone.sc.buffer.Buffer
    overtone.sc.bus.AudioBus})

(defn sc-idable?
  [x]
  (SC-IDABLES (type x)))

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
                   (sc-idable? v) [(symbol (name k)) (:id v)]
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

(defn coll-of-numbers? [coll]
  (and (sequential? coll)
       (every? number? coll)))

;; TODO optimize to not use merge
(defn modify-params2
  "Used when calling the synth"
  [params]
  #_(println "MP2" params)
  (->> params
       (mapv (fn [[k v]]
               (cond
                 (number? v) {k v}
                 (sc-idable? v) {k (:id v)}
                 (or (vector? v)
                     (coll-of-numbers? v)) (map-indexed (fn [i v*]
                                                          {(keyword (str (name k) i)) v*})
                                                        v))))
       flatten
       (apply merge)))
(comment
  (modify-params2 (select-keys merged-params [:buf])))
#_(defmacro make-synth [params-map synth-body]
    (let [[s-name# params ugen-form]
          (let [body (eval (modify-body params-map synth-body))]
            (println "==============" body)
            (o/synth-form 's-name [(modify-params params-map) body]))]
      `(o/synth ~s-name# ~params ~ugen-form)))

(defn analyze-arg
  [k arg]
  (cond
    (or (number? arg) (sc-idable? arg)) [:number]
    (-> arg meta :fragment) [:ugen (resolve-frag arg)]
    (ns-kw? "ugen" k) [:ugen arg]
    (sequential? arg) [:seq (count arg)]
    :else (throw (ex-info "Don't know how to analyze arg:" {:key k
                                                            :arg arg}))))

(defn analyze-ds-args [synth-symbol m]
  (->> m
       (mapv (fn [[k arg]]
               [k (analyze-arg k arg)]))
       (into [(str synth-symbol)])))

(comment

  (analyze-ds-args
   'hola
   {:freq [500]
    :rev-mix 0.2
    :rev-room 0.5
    :width 1.5
    :outs [0]
    :ugen/pan #'panny
    :ugen/mix '((fn [%] (if (> (count freq) 1)
                          (o/mix %)
                          %)))
    :levels [0 1 1 1 0]
    :env-durs [1 5 5 1]}
   #_{:freq [500 900]
      :levels [0 1 0.1 1 0]
      :env-durs [1 3 1 1]
      :ugen/env '(o/env-gen (o/envelope levels env-durs) :action o/FREE)
      :shaper-limit 0.1
      :rev-mix 0
      :rev-room 2
      :ugen/rev '(o/free-verb rev-mix rev-room 0.3) ;; '(o/sine-shaper shaper-limit)
      :ugen/pan '((fn [sig] (o/pan-az 4 sig (o/lf-saw 0.3))))
      :ugen/fx1 '(o/distort)
      :ugen/mix '((fn [sig] (if (> (count freq) 1)
                              (o/mix sig)
                              sig)))
      :outs [0]}))

(defn make-synth-form
  [synth-symbol params-map synth-body]
  (let [body (modify-body params-map synth-body)]
    (timbre/debug :make-synth-form/body body)
    (o/synth-form synth-symbol [(modify-params params-map) body])))

(defonce variations-data (atom {}))
(defonce synths-cache (atom {}))

(defn instance-symbol [namespaced-synth-string synth-symbol]
  (let [sym (symbol (str synth-symbol (inc (get-in @variations-data [namespaced-synth-string :count] -1))))]
    (timbre/info "Synth symbol created: "  sym "for" synth-symbol)
    sym))

(comment
  (-> @variations-data))
(defn qualify-body
  [params-map synth-body]
  (let [collides (ns-interns 'overtone.sc.ugen-collide-list)]
    (walk/postwalk (fn [x]
                     (if (and (symbol? x)
                              (not (contains? params-map (keyword x))))
                       (if-let [res (resolve x)]
                         (cond
                           (number? (var-get res)) (var-get res) ;; constants
                           (collides x) (collides x)
                           :else res)
                         x)
                       x))
                   synth-body)))
(comment
  (require '[tieminos.sc-utils.synths.v1 :refer [lfo-kr]])
  lfo-kr
  (qualify-body #{:out} '((fn [sig] (o/out (map-outs out) (o/sin-osc (lfo-kr 1 0 1)))))))
(contains? #{:hola} :hola)
(def PLUG_NS "ugen")

(defn plug*
  [external-params-set body]
  (qualify-body external-params-set body))

(defmacro plugm
  [external-params-set body]
  `(plug* ~external-params-set ~body))

(defn qualify-plug-map
  [external-params-set plug-map]
  (let [plug-keys (->> plug-map
                       keys
                       (filter #(= PLUG_NS (namespace %))))
        param-keys (->> plug-map
                        keys
                        (remove #(= PLUG_NS (namespace %))))
        params-set (set/join (set external-params-set)
                             (set param-keys))]
    (reduce
     (fn [pm k]
       (assoc pm k (qualify-body params-set (plug-map k))))
     plug-map
     plug-keys)))

(comment
  (def plug-map {:out 0
                 :ugen/out '((fn [sig] (o/out (map-outs out) (o/sin-osc 1))))})
  (plugm #{:out} '((fn [sig] (o/out (map-outs out) (o/sin-osc 1))))))
(defn qualify-plug-map
  [external-params-set plug-map]
  (let [plug-keys (->> plug-map
                       keys
                       (filter #(= PLUG_NS (namespace %))))
        param-keys (->> plug-map
                        keys
                        (remove #(= PLUG_NS (namespace %))))
        params-set (set/union (set external-params-set)
                              (set param-keys))]
    (reduce
     (fn [pm k]
       (assoc pm k (qualify-body params-set (plug-map k))))
     plug-map
     plug-keys)))

#_(qualify-plug-map #{} plug-map)
#_(qualify-plug-map #{:outs} plug-map)

(defn- plug-map->arg-opts
  [plug-map]
  (let [args (remove #(= PLUG_NS (namespace (first %))) plug-map)]
    {:keys (vec (map (comp symbol first) args))
     :or (into {} (map (juxt (comp symbol first) second) args))}))
#_(plug-map->arg-opts plug-map)

(defn- plug-map->assoc-args
  [external-params plug-map]
  (let [qualified-plug-map (qualify-plug-map external-params plug-map)]
    (mapcat (fn [[k v]]
              [k (if (not= PLUG_NS (namespace k))
                   (symbol k)
                   v)])

            qualified-plug-map)))
#_(plug-map->assoc-args #{} plug-map)
#_(qualify-plug-map  #{:outs} plug-map)
(defmacro defplug
  ([sym plug-map] `(defplug ~sym #{} ~plug-map))
  ([sym external-params plug-map]
   `(defn ~sym [~'params
                & ~(plug-map->arg-opts plug-map)]
      (assoc ~'params ~@(plug-map->assoc-args external-params plug-map)))))

(comment

  (macroexpand-1
   (macroexpand-1
    '(defplug +outs
       {:out 0
        :ugen/out '((fn [sig] (o/out (map-outs out) (o/sin-osc 1))))}
       #{:outs}))))

(comment
  ;; Here freq shouldn't overwrite the symbol in the qualified body
  (def freq 23456)
  (qualify-body
   {:freq 222}
   '(o/out out
           (* (o/env-gen (o/env-perc) :action o/FREE)
              (o/sin-osc freq))))
  ;; Here out should be 0 in the qualified body
  (def out-var 0)
  (qualify-body
   {:freq 222}
   '(o/out out-var
           (* (o/env-gen (o/env-perc) :action o/FREE)
              (o/sin-osc freq)))))

(declare call-synth define-synth)

(defn get-synth-ns-string
  ([synth-symbol] (get-synth-ns-string *ns* synth-symbol))
  ([ns synth-symbol]
   (str/replace (ns-resolve ns synth-symbol)
                #"#'" "")))
(comment
  (with-meta 'hola {:ns "my-ns"})
  (namespace 'hola))
#_(get-synth-ns-string synth-symbol)
(defn add-variation-data!
  [namespaced-synth-string
   synth-body
   params-map
   analyzed-args]
  (swap! variations-data
         (fn [data]
           (let [synth-data (get data namespaced-synth-string
                                 {:default-params params-map
                                  :synth-body (qualify-body params-map synth-body)})]
             (assoc data
                    namespaced-synth-string
                    (-> synth-data
                        (update :count (fnil inc 0))
                        (assoc-in [:variants analyzed-args] {:default-params params-map
                                                             :synth-body synth-body})))))))
(comment (-> @variations-data))

(defn get-variations
  [synth-var-str]
  (-> (get @variations-data synth-var-str)
      :variants
      keys))

(defn remove-synth
  [synth-var-str]
  (when-let [variations (get-variations synth-var-str)]
    (swap! variations-data dissoc synth-var-str)
    (swap! synths-cache #(apply dissoc % variations))))

;; NOTE IMPORTANT ths is promising, no macros!
(defn make-synth-fn
  [synth-symbol params-map synth-body & {:keys [reset? ns]
                                         :or {ns *ns*}}]
  ;; TODO should analyze args and memoize synths

  (when-not (ns-resolve ns synth-symbol)
    ;; TODO add getter
    (define-synth ns synth-symbol))

  (let [namespaced-synth-string (get-synth-ns-string ns synth-symbol)
        _ (when reset? (remove-synth namespaced-synth-string))
        analyzed-args (analyze-ds-args namespaced-synth-string params-map)
        cached-synth (get-in @synths-cache [analyzed-args])]
    (if cached-synth
      cached-synth
      (let [params-map* (qualify-plug-map #{} params-map)
            _ (timbre/debug :make-synth-fn/params-map* params-map*)
            synth-body* (qualify-body params-map synth-body)
            _ (timbre/debug :make-synth-fn/synth-body* synth-body*)
            [_s-name params ugen-form] (make-synth-form
                                        synth-symbol
                                        params-map
                                        synth-body*)
            _ (timbre/debug "[make-synth] params" params)
            synth (eval (list 'overtone.core/synth
                              (instance-symbol namespaced-synth-string synth-symbol)
                              params
                              ugen-form))]
        (swap! synths-cache assoc analyzed-args synth)
        ;; TODO prevent overwritting a synth from another namespace (i.e. namespace the symbol)
        (add-variation-data!
         namespaced-synth-string
         synth-body*
         params-map
         analyzed-args)
        synth))))

(defn get-instance-data
  [ns synth-symbol params-map]
  (def synth-symbol synth-symbol)
  (timbre/debug :get-instance-data/synth-symbol synth-symbol)
  (let [namespaced-synth-string (get-synth-ns-string ns synth-symbol)
        {:keys [default-params synth-body]} (get @variations-data namespaced-synth-string)
        merged-params (merge default-params params-map)
        analyzed-args (analyze-ds-args namespaced-synth-string merged-params)
        _ (timbre/debug :get-instance-data/analyzed-args analyzed-args)
        _ (def analyzed-args analyzed-args)
        synth (or (get-in @synths-cache [analyzed-args])
                  (make-synth-fn synth-symbol merged-params synth-body {:ns ns}))]
    {:synth synth
     :analyzed-args analyzed-args
     :merged-params merged-params
     :synth-body synth-body}))

(comment
  (def buf (o/load-sample "samples/habitat_samples/take-1-gusano-cuantico-2.2.9.2-algo-2-2-9-mic-2-bus-43.wav"))
  ((:synth (get-instance-data (:ns csp)
                              (symbol (:synth-symbol csp))
                              (dissoc (:params-map csp)
                                      :group)))
   :buf buf))

(comment
  (-> @variations-data)
  (get @variations-data namespaced-synth-string)
  (-> @synths-cache)
  (get-in @synths-cache [analyzed-args]))
#_(get-instance-data 'sini {})

(defn call-synth
  [ns synth-symbol params-map]
  (def csp {:ns ns :synth-symbol synth-symbol :params-map params-map})
  (timbre/debug "call synth" synth-symbol)
  (let [group (:group params-map)
        params-map (dissoc params-map :group)
        {:keys [synth merged-params]} (get-instance-data ns (symbol synth-symbol) params-map)
        params (modify-params2 merged-params)]
    (timbre/debug "[call-synth] synth" synth)
    (timbre/debug "[call-synth] params" params)
    (timbre/debug "[call-synth] group" group)
    (def merged-params merged-params)
    (def synth synth)
    (def  group group)
    (def  params params)
    (if group
      (apply synth group (flatten (seq params)))
      (synth params))))

(comment
  (-> @variations-data)
  (-> csp)
  (:ns csp)
  (symbol (:synth-symbol csp))
  (dissoc (:params-map csp) :group)
  (:synth-body (get-instance-data (:ns csp)
                                  (symbol (:synth-symbol csp))
                                  (dissoc (:params-map csp)
                                          :group)))
  ((:synth (get-instance-data (:ns csp)
                              (symbol (:synth-symbol csp))
                              (dissoc (:params-map csp)
                                      :group)))
   :rate 2)
  (-> merged-params)
  (-> params)
  (-> group)
  (-> synth)
  (synth params)
  (apply synth group (flatten (seq (assoc params :rate 2))))
  (apply synth2  (flatten {:freq 100}))
  (o/stop)
  (resolve 'sini)
  (get @variations-data 'sini)
  (call-synth 'sini {:freq [500]})
  (sini))

(defn define-synth
  "Returns a synth calling funciton"
  [ns synth-symbol]
  (timbre/info "defining synth" ns synth-symbol)
  (intern ns synth-symbol
          (fn
            ([] (#'call-synth ns synth-symbol {}))
            ([params-map] (#'call-synth ns synth-symbol params-map)))))

#_(comment
    (ns-unmap  *ns* 'sini)

    (intern *ns* 'hola 6)
    (def hola "hola")
    (-> hola))

#_(comment
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

(comment
  (reset! variations-data {})
  (reset! synths-cache {})
  (-> @variations-data)
  (-> @variations-data
      (get "tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths/cristal-liquidizado-2")
      :synth-body)
  (-> @synths-cache keys)
  (get-variations "tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths/cristal-liquidizado-2")
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
    :ugen/pan '((fn [sig] (o/pan-az 4 sig (o/lf-saw 0.3))))
    :ugen/fx1 '(o/distort)
    :ugen/mix '((fn [sig] (if (> (count freq) 1)
                            (o/mix sig)
                            sig)))
    :outs [0]}
    ;; synth
   '(map-to-outs-seq
     outs
     (-> (o/sin-osc freq)
         :ugen/mix
         :ugen/fx1
         :ugen/pan
         :ugen/rev
         (* :ugen/env)))
   {:reset? true})

  ;; synth call
  (def ^:fragment panny '((fn [%] (do
                                    (println "com")
                                    (o/pan-az (count (set outs))
                                              %
                                              (o/line:kr -0.4  4 (apply + env-durs))
                                              :width width)))))
  (meta #'panny)
  (let [outs [0 1 3 2]]
    (sinpan {:freq [500]
             :rev-mix 0.2
             :rev-room 0.5
             :width 1.5
             :outs outs
             :ugen/pan #'panny
             :ugen/mix '((fn [%] (if (> (count freq) 1)
                                   (o/mix %)
                                   %)))
             :levels [0 1 1 1 0]
             :env-durs [1 5 5 1]}))
  #_((make-synth-fn
      'sini
      (merge {:freq [200 500] :out 0} {:amp 1})
      '(o/out out (* (o/env-gen (o/env-perc)
                                :action o/FREE) (o/sin-osc freq)))))
  #_(println "res:")
  #_(println @variations-data)
  #_(println @synths-cache))
