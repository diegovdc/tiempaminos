(ns hooks.make-synth-fn
  (:require
   [clj-kondo.hooks-api :as api]
   [clojure.walk :as walk]))

(def PLUG_NS "ugen")

(defn bindings-map->vec
  [m]
  (->> m
       (into {})
       :children
       (partition 2 2)
       (remove (fn [[{k :k} _v]] (= PLUG_NS (namespace k))))
       (mapcat (fn [[{k :k} v]]
                 [(api/token-node (symbol k)) v]))))

(defn ignore-ugen-kw-errors
  [body]
  (let [new-ch
        (->> body
             :children
             (walk/postwalk
              (fn [x]
                (if-let [ch (:children x)]
                  (let [ch* (->> ch
                                 (mapv (fn [c]
                                         (if (some-> c :children  first :k (namespace) (= PLUG_NS))
                                           (vary-meta c assoc :clj-kondo/ignore [:type-mismatch :invalid-arity])
                                           c))))]
                    (assoc x :children ch*))

                  x))))]
    (assoc body :children new-ch)))

(defn make-synth-fn [{:keys [node]}]
  (let [[_ synth-name params-map body _opts] (:children node)
        body* [params-map
               (-> body :children first ignore-ugen-kw-errors)]
        binding-vec (->> params-map
                         :children
                         (filter #(= :map (:tag %)))
                         first
                         bindings-map->vec)
        new-node (api/list-node
                  (list*
                   (api/token-node 'def)
                   (-> synth-name :children first :value  api/token-node)

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
    (make-synth-fn {:node (api/parse-string test-str)}))

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





