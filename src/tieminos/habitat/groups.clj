(ns tieminos.habitat.groups
  (:require
   [overtone.core :as o]))

(defonce groups (atom {}))

(defn early
  ([] (early :head))
  ([pos] [pos (:early @groups)]))
(defn mid
  ([] (mid :head))
  ([pos] [pos (:mid @groups)]))
(defn panners
  ([] (panners :head))
  ([pos] [pos (:panners @groups)]))
(defn preouts
  ([] (preouts :head))
  ([pos] [pos (:preouts @groups)]))
(defn fx [] [:tail (:fx @groups)])

(defn init-groups! []
  (let [main (o/group "habitat main")
        early (o/group :head main)
        mid (o/group :after early)
        panners (o/group :after mid)
        preouts (o/group :after panners)
        fx (o/group "fx" :after preouts)]
    (reset! groups
            {:main main
             :early early
             :mid mid
             :panners panners
             :preouts preouts
             :fx fx})))

(comment
  (init-groups!))
