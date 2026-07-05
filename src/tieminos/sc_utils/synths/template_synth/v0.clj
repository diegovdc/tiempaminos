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

(defn safe-get-ugen-param
  "Prevent a param from being `nil`, if that value is in the map."
  [params x]
  (let [v (params x)]
    (or v identity)))

(defn modify-body
  "Modifies the body of a template to insert the ugens and return a `ugen-form`. If an ugen is missing in the params and has no default (like `:ugen/pan`) or if it is `nil` (like :ugen/nilly`) it it will be substituted by `identity`. Also if the parameters come in a vector they will use serial keys as that is what Overtone expects."
  [params synth-body]
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
                         (ns-kw? "fx" x) (safe-get-ugen-param params x)
                         (ns-kw? "ugen" x) (safe-get-ugen-param params x)
                         (ns-kw? "dyn" x) (list 'as-> 'sig (safe-get-ugen-param params x))
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
                ;; :ugen/pan #'panny
                :ugen/rev '(o/sine-shaper shaper-limit)
                :ugen/fx1 '(o/dist)
                :out 0}]
    (modify-body
     params
     (qualify-body params '(o/out 0
                                  (-> (o/sin-osc freq)
                                      :ugen/pan
                                      :ugen/rev
                                      :ugen/fx1
                                      :ugen/nilly
                                      (* :ugen/env)))))))

(def freq 5432)

(def ^:private SC-IDABLES
  #{overtone.sc.sample.Sample
    overtone.sc.buffer.Buffer
    overtone.sc.bus.AudioBus})

(defn sc-idable?
  [x]
  (SC-IDABLES (type x)))

(defn modify-params
  "Creates the params vector with the defaults of an Overtone synth. It takes a map of params which may contain a vector of values. Such vectors will produced serially named symbols with the corresponding values as their defaults.
  Used when creating the synth's params vector"
  [params]
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

(defn coll-of-numbers? [coll]
  (and (sequential? coll)
       (every? number? coll)))

;; TODO optimize to not use merge
(defn modify-params2
  "When calling a synth, processes the params map coming into the synth into params that an overtone synth understands."
  [params]
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

(defn analyze-args
  "Analyze the arguments in a params map so that the output serves to index the cache for a synth variation."
  [synth-symbol m]
  (->> m
       (mapv (fn [[k arg]]
               [k (analyze-arg k arg)]))
       (into [(str synth-symbol)])))

(defn make-synth-form
  "Creates and Overtone synth form."
  [synth-symbol params-map synth-body]
  (let [body (modify-body params-map synth-body)]
    (timbre/debug :make-synth-form/body body)
    (o/synth-form synth-symbol [(modify-params params-map) body])))

(defonce variations-data (atom {}))

(defonce synths-cache (atom {}))

(defn instance-symbol
  [namespaced-synth-string synth-symbol]
  (let [sym (symbol (str synth-symbol (get-in @variations-data [namespaced-synth-string :count] 0)))]
    (timbre/info "Synth symbol created: "  sym "for" synth-symbol)
    sym))

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

(defn add-variation-data!
  [namespaced-synth-string
   synth-body
   params-map
   analyzed-args
   synth-symbol
   ugen-form]
  (swap! variations-data
         (fn [data]
           (let [synth-data (get data namespaced-synth-string
                                 {:default-params params-map
                                  :synth-body (qualify-body params-map synth-body)})]
             (assoc data
                    namespaced-synth-string
                    (-> synth-data
                        (update :count (fnil inc 0))
                        (assoc-in [:variants analyzed-args] {:synth-symbol synth-symbol
                                                             :default-params params-map
                                                             :synth-body synth-body
                                                             :ugen-form ugen-form})))))))
(defn get-variants*
  [synth-var-str]
  (-> (get @variations-data synth-var-str)
      :variants
      keys))

(defn get-variants-dispatcher
  [synth-caller-or-namespaced-synth-string]
  (cond
    (= ::synth-caller (-> synth-caller-or-namespaced-synth-string meta :type))
    :synth-caller
    (string? synth-caller-or-namespaced-synth-string)
    :namespaced-synth-string))

(defmulti get-variants
  "args:
   - [synth-caller-or-namespaced-synth-string]
  Returns the variants give a `synth-caller` or a `namespaced-synth-string`"
  #'get-variants-dispatcher)

(defmethod get-variants :synth-caller
  [synth-caller]
  (let [{:keys [ns synth-symbol]} (meta synth-caller)]
    (get-variants* (get-synth-ns-string ns synth-symbol))))

(defmethod get-variants :namespaced-synth-string
  [namespaced-synth-string]
  (get-variants* namespaced-synth-string))

(defn remove-synth
  [synth-var-str]
  (when-let [variations (get-variants synth-var-str)]
    (swap! variations-data dissoc synth-var-str)
    (swap! synths-cache #(apply dissoc % variations))))

(defn make-synth-fn
  [synth-symbol params-map synth-body
   & {:keys [reset? ns]
      :or {reset? true
           ns *ns*}}]

  (when-not (ns-resolve ns synth-symbol)
    ;; TODO add getter
    (define-synth ns synth-symbol))

  (let [namespaced-synth-string (get-synth-ns-string ns synth-symbol)
        _ (when reset? (remove-synth namespaced-synth-string))
        analyzed-args (analyze-args namespaced-synth-string params-map)
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
            synth-symbol (instance-symbol namespaced-synth-string synth-symbol)
            synth (eval (list 'overtone.core/synth
                              synth-symbol
                              params
                              ugen-form))]
        (swap! synths-cache assoc analyzed-args synth)
        ;; TODO prevent overwritting a synth from another namespace (i.e. namespace the symbol)
        (add-variation-data! namespaced-synth-string
                             synth-body*
                             params-map
                             analyzed-args
                             synth-symbol
                             ugen-form)
        synth))))

(defn get-variant-data-dispatcher
  ([_ns _synth-symbol _params-map] :ns+synth-sym+params-map)
  ([synth-caller _params-map]
   (if  (= ::synth-caller
           (-> synth-caller meta :type))
     :synth-caller+params-map
     (throw (ex-info "Can't process this:" {:synth-caller synth-caller})))))

(defn- synth-id
  [overtone-synth]
  (-> overtone-synth :sdef :name))

(defn get-variant-data*
  [ns synth-symbol params-map]
  (timbre/debug :get-variant-data/synth-symbol synth-symbol)
  (let [namespaced-synth-string (get-synth-ns-string ns synth-symbol)
        {:keys [default-params synth-body]} (get @variations-data namespaced-synth-string)
        merged-params (merge default-params params-map)
        analyzed-args (analyze-args namespaced-synth-string merged-params)
        _ (timbre/debug :get-variant-data/analyzed-args analyzed-args)
        synth (or (get-in @synths-cache [analyzed-args])
                  (make-synth-fn synth-symbol
                                 merged-params
                                 synth-body
                                 {:reset? false :ns ns}))
        variant-data (get-in @variations-data [namespaced-synth-string :variants analyzed-args])]
    (-> {:variant-id (synth-id synth)
         :template/namespaced-synth-string namespaced-synth-string
         :analyzed-args analyzed-args}
        (merge variant-data)
        (assoc :overtone/synth synth))))

(defmulti get-variant-data
  "args:
    - [ns synth-symbol params-map]
    - [synth-caller params-map]
  
  Get data describing the variant (including an Overtone synth on the `:synth` key.
  Includes the following keys:
  :variant-id 
  :template/namespaced-synth-string
  :analyzed-args
  :synth-symbol
  :default-params
  :synth-body
  :ugen-form
  :overtone/synth

  `synth-caller` is the var generated by `make-synth-fn` 
  
  The `params-map` defines the variant returned. Passing an empty map returns the original (default) variant."
  #'get-variant-data-dispatcher)

(defmethod get-variant-data :ns+synth-sym+params-map
  [ns synth-symbol params-map]
  (get-variant-data* ns synth-symbol params-map))

(defmethod get-variant-data :synth-caller+params-map
  [synth-caller params-map]
  (let [{:keys [ns synth-symbol]} (meta synth-caller)]
    (get-variant-data* ns synth-symbol params-map)))

(defn call-synth
  [ns synth-symbol params-map]
  (let [group (:group params-map)
        params-map (dissoc params-map :group)
        {:keys [overtone/synth default-params]} (get-variant-data* ns (symbol synth-symbol) params-map)
        params (modify-params2 default-params)]
    (if group
      (apply synth group (flatten (seq params)))
      (synth params))))

(defn params->synth-variant*
  "Returns a synth variant that works for the given a sample params-map (it can contain ugens).
  NOTE: current ns is `*ns*` other ns can be retrived by (find-ns 'my.ns)"
  [ns synth-symbol params-map]
  (let [params-map (dissoc params-map :group)
        {:keys [overtone/synth] :as variant-data} (get-variant-data* ns (symbol synth-symbol) params-map)
        data (-> variant-data
                 (dissoc :overtone/synth)
                 (assoc :params (->> variant-data :overtone/synth :params (sort-by first))))]
    (with-meta synth data)))

(defmulti get-synth-variant
  "Returns a synth variant that works for the given a sample params-map (it can contain ugens)."
  #'get-variant-data-dispatcher)

(defmethod get-synth-variant :ns+synth-sym+params-map
  [ns synth-symbol params-map]
  (params->synth-variant* ns synth-symbol params-map))

(defmethod get-synth-variant :synth-caller+params-map
  [synth-caller params-map]
  (let [{:keys [ns synth-symbol]} (meta synth-caller)]
    (params->synth-variant* ns synth-symbol params-map)))

(defn variant-id*
  [ns synth-symbol params-map]
  (-> (params->synth-variant* ns synth-symbol params-map)
      meta
      :variant-id))

(defmulti variant-id #'get-variant-data-dispatcher)

(defmethod variant-id :ns+synth-sym+params-map
  [ns synth-symbol params-map]
  (variant-id* ns synth-symbol params-map))

(defmethod variant-id :synth-caller+params-map
  [synth-caller params-map]
  (let [{:keys [ns synth-symbol]} (meta synth-caller)]
    (variant-id* ns synth-symbol params-map)))

(defn describe-synth
  "Takes an synth-variant as generated by `get-synth-variant` and shows a summary of it's params and other data"
  [synth-variant]
  (let [{:keys [variant-id template/namespaced-synth-string params ugen-form]} (meta synth-variant)]
    {:variant-id variant-id
     :template namespaced-synth-string
     :params (map (juxt (comp keyword :name) :default) params)
     :ugen-form ugen-form}))

(comment
  (sawy)
  (def t (get-synth-variant (find-ns 'user) 'sawy {:freq [400 500 600]}))
  (meta t)
  (type t)
  (describe-synth t)
  (def t (get-synth-variant sawy {:freq [400 500 900 600]}))
  (def t (get-synth-variant sawy {:freq [900 600]}))
  (get-variant-data sawy {:freq [400 500 900 600]})
  (keys (get-variant-data sawy {:freq [400 500 900 600]}))
  (-> sawy meta)
  (variant-id sawy {}))

(comment
  (make-synth-fn
  ;; synth name: a quoted symbol
   'sawy
  ;; default args
   {:freq [500 900]
    :env-levels [0 1 0.1 1 0]
    :env-durs [1 3 1 1]
    :amp 1
   ;; template-able ugens, they are "passed in" dynamically to the synth (new synth versions are cached). They can access other arguments in the synth.
    :ugen/env '(o/env-gen (o/envelope env-levels env-durs) :action o/FREE)
    :ugen/fx1 '(o/distort)
    :ugen/mix '((fn [sig]
                 ;; if sig is a single sin-osc (not a vector) then don't call `o/mix`.
                 ;; NOTE: even if a single freq is passed in a vector, the sig will still come in not in a vector.
                  (if (vector? freq)
                    (o/mix sig)
                    sig)))
   ;; a default can be just a number
    :ugen/pan-pos 0}
  ;; ugen graph template: a quoted list containing a graph of ugens or keywords with the `ugen` namespace. They are subtituted by the passed in ugens. Otherwise the syntax is the same as the body of a `defsynth`.
   '(o/out 0
           (-> (o/sin-osc freq)
               :ugen/mix
               :ugen/fx1
               (o/pan2 :ugen/pan-pos)
               (o/free-verb)
               (* amp :ugen/env)))
  ;; remove all variations of the synth when `make-synth-fn` is called.
   {:reset? true}))

(defn define-synth
  "Returns a synth calling function"
  [ns synth-symbol]
  (timbre/info (format "Defining synth: %s/%s" ns synth-symbol))
  (intern ns synth-symbol
          (with-meta (fn
                       ([] (#'call-synth ns synth-symbol {}))
                       ([params-map] (#'call-synth ns synth-symbol params-map)))
            {:type ::synth-caller
             :ns ns
             :synth-symbol synth-symbol})))

