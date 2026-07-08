(ns hooks.defplug
  (:require [clj-kondo.hooks-api :as api]))

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
(defn external-bindings->vec
  [set*]
  (->> set*
       :children
       (mapcat (fn [t]
                 [t (api/token-node nil)]))))
(defn bindings-map->body
  [m]

  (->> m
       (into {})
       :children
       (partition 2 2)
       (filter (fn [[{k :k} _v]] (= PLUG_NS (namespace k))))
       (map (fn [[{k :k} v]]
              v))))

(defn defplug [{:keys [node]}]
  (let [[_ synth-name a b] (:children node)
        bindings-map (or b a)
        external-bindings (if b a #{})
        binding-vec (bindings-map->vec bindings-map)
        external-bindings-vec (external-bindings->vec external-bindings)
        body (bindings-map->body bindings-map)
        new-node (api/list-node
                  (list*
                   (api/token-node 'def)
                   synth-name
                   [(api/list-node
                     (list*
                      (api/token-node 'let)
                      (api/vector-node (concat binding-vec
                                               external-bindings-vec))
                      body))]))]
    {:node new-node}))

(comment
  (-> {:node
       (api/parse-string
        "
(defplug moog-ladder
  {:lpf 20000
   :reso 0.1
   :ugen/filter (fn [sig] (o/moog-ladder sig (o/clip lpf 30 20000) reso))})")}
      defplug)
  (-> {:node
       (api/parse-string
        "
(defplug moog-ladder
#{outs}
  {:lpf 20000
   :reso 0.1
   :ugen/filter (fn [sig] (o/moog-ladder sig (o/clip lpf 30 20000) reso))})")}
      defplug))
