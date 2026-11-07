(ns hooks.make-synth-fn
  (:require
   [clj-kondo.hooks-api :as api]
   [clojure.walk :as walk]))

(def PLUG_NS "ugen")

(defn bindings-map->vec
  [m]
  (->> m
       :children
       (partition 2 2)
       (remove (fn [[{k :k} _v]] (= PLUG_NS (namespace k))))
       (mapcat (fn [[{k :k} v]]
                 [(api/token-node (symbol k)) v]))))

(defn find-params-map
  "The params map may be the argument itself, or nested somewhere inside a
  form threading it through plugins."
  [node]
  (if (= :map (:tag node))
    node
    (some find-params-map (:children node))))

(defn ugen-kw?
  [x]
  (some-> x :k namespace (= PLUG_NS)))

(defn ignore-ugen-kw-errors
  [body]
  (walk/postwalk
   (fn [x]
     (if (and (:tag x)
              (some ugen-kw? (:children x)))
       (vary-meta x assoc :clj-kondo/ignore [:type-mismatch :invalid-arity])
       x))
   body))

(defn make-synth-fn [{:keys [node]}]
  (let [[_ synth-name params-map body _opts] (:children node)
        body* [params-map
               (-> body :children first ignore-ugen-kw-errors)]
        binding-vec (bindings-map->vec (find-params-map params-map))
        new-node (api/list-node
                  (list*
                   (api/token-node 'def)
                   (-> synth-name :children first :value api/token-node)

                   [(api/list-node
                     (list*
                      (api/token-node 'let)
                      (api/vector-node binding-vec)
                      body*))]))]
    {:node new-node}))

(comment

  (do
    (def test-str "
(make-synth-fn
         'siny
         (-> {:freq [500 900]
              :amp 1
              :ugen/pan (something)}
             (env1)
             (freq-mixer))
         '(o/out 0
                 (-> (o/sin-osc freq)
                     (:ugen/freq-mixer freq amp) 
                     (o/pan2 0)
                     (* amp :ugen/env)
                     (:ugen/outs)))
         {:reset? true})")
    (def res (make-synth-fn {:node (api/parse-string test-str)}))
    res)

  (def test-str "
(make-synth-fn
   'siny
   (-> {:freq [500 900]
        :amp 1})
   '(let [sig (o/sin-osc freq)]
      (-> sig
          (:ugen/freq-mixer freq amp)
          (:ugen/outs))))")
  (def res (make-synth-fn {:node (api/parse-string test-str)}))
  (-> res :node :children (nth 2) :children (nth 3) :children (nth 2) :children (nth 2) meta)
  (-> res :node :children (nth 2) :children (nth 3) :children (nth 2) :children (nth 3) #_meta)

  (def code
    (str "(require '[clj-kondo.impl.utils :as u])
(let [something (fn [])
        env1 (fn [_] _)
        freq-mixer (fn [_] _)]
    (u/make-synth-fn
      'siny
      (-> {:freq [500 900]
           :amp 1
           :ugen/pan (something)}
          (env1)
          (freq-mixer))
      '(o/out 0
              (-> (o/sin-osc freq)
                  (:ugen/freq-mixer freq amp) 
                  (o/pan2 0)
                  (* amp :ugen/env)))
      {:reset? true}))"))

  (require '[clj-kondo.core :as clj-kondo])
  (:findings (with-in-str code (clj-kondo/run! {:lint ["-"]}))))





