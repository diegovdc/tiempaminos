(ns tieminos.seq-utils.utils
  (:require
   [clojure.set :as set]
   [taoensso.timbre :as timbre]))

(defn invert-graph
  "Invert the direction of a graph"
  [graph]
  (reduce (fn [g [node edges]]
            (reduce (fn [g e]
                      (update g e (fnil conj #{}) node))
                    g
                    edges))
          {}
          graph))

(defn bigraph
  "Returns a graph with bidirectional edges."
  [graph]
  (merge-with set/union
              graph
              (invert-graph graph)))

(defn subgraph
  "Select a subset from a graph data structure."
  [graph node-set]
  (let [node-set*  (into #{} node-set)]
    (->> node-set*
         (select-keys graph)
         (map (fn [[node edges]]
                (let [edges* (->> edges
                                  (filter (fn [e] (node-set* e)))
                                  set)]
                  (when (and (seq edges) (not (seq edges*)))
                    (timbre/warn (format "Node with no edges found: %s. Consider using a bidirectional graph."
                                         node)))
                  [node edges*])))
         (into {}))))

(defn seq->graph
  ;; TODO improve to handle other pattern types
  [xs]
  (->> xs
       (partition 2 1 (take 1 xs))
       (reduce
        (fn [graph [node edge]]
          (update graph node (fnil conj #{}) edge))
        {})))
