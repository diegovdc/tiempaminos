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
(defn mid-midi-synths
  ([] (mid-midi-synths :head))
  ([pos] [pos (:mid-midi-synths @groups)]))
(defn panners
  ([] (panners :head))
  ([pos] [pos (:panners @groups)]))
(defn preouts
  ([] (preouts :head))
  ([pos] [pos (:preouts @groups)]))
(defn fx [] [:tail (:fx @groups)])

(defn init-groups!
  "`add-custom-groups-fn` (optional) takes the default groups map and returns a new groups map"
  [& {:keys [add-custom-groups-fn]}]
  (let [main (o/group :main)
        early (o/group :early :head main)
        mid (o/group :mid :after early)
        mid-midi-synths (o/group :mid-midi-synths :after mid)
        panners (o/group :panners :after mid)
        preouts (o/group :preouts :after panners)
        fx (o/group :fx :after preouts)]
    (reset! groups
            (cond-> {:main main
                     :early early
                     :mid mid
                     :mid-midi-synths mid-midi-synths
                     :panners panners
                     :preouts preouts
                     :fx fx}
              (fn? add-custom-groups-fn) add-custom-groups-fn))))

(comment
  (init-groups!)
  (:group (second (early))))
