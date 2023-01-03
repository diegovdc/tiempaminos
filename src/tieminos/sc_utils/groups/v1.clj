(ns tieminos.sc-utils.groups.v1
  (:require
   [overtone.core :as o]))

(defonce groups (atom {}))

(defn early [] [:head (:early @groups)])
(defn mid [] [:head (:mid @groups)])
(defn late [] [:head (:late @groups)])
(defn fx [] [:tail (:fx @groups)])

(defn init-groups! []
  (let [main (o/group "get-on-the-bus main")
        early (o/group :head main)
        mid (o/group :after early)
        late (o/group :after mid)
        fx (o/group "fx" :after late)]
    (reset! groups
            {:main main
             :early early
             :mid mid
             :late late
             :fx fx})))

(comment
  (init-groups!))

