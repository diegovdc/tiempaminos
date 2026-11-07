(ns tieminos.sc-utils.groups.v1
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]))

(defonce groups (atom {}))

(defn early
  ([] (early :head))
  ([pos] [pos (:early @groups)]))

(defn mid
  ([] (mid :head))
  ([pos] [pos (:mid @groups)]))

(defn late
  ([] (late :head))
  ([pos] [pos (:late @groups)]))

(defn fx
  ([] (fx :tail))
  ([pos] [pos (:fx @groups)]))

(defn post-fx
  ([] (post-fx :tail))
  ([pos] [pos (:post-fx @groups)]))

(defn output-rec
  ([] (output-rec :tail))
  ([pos] [pos (:output-rec @groups)]))

(defn init-groups! []
  (if (seq @groups)
    (timbre/warn "sc-utils.groups.v1 already initialized")
    (let [main (o/group "get-on-the-bus main")
          early (o/group :head main)
          mid (o/group :after early)
          late (o/group :after mid)
          fx (o/group "fx" :after late)
          post-fx (o/group "post-fx" :after fx)
          output-rec (o/group "output-rec" :after late)]
      (timbre/info "Initializing groups")
      (reset! groups
              {:main main
               :early early
               :mid mid
               :late late
               :fx fx
               :post-fx post-fx
               :output-rec output-rec}))))

(comment
  (reset! groups {})
  (init-groups!))

