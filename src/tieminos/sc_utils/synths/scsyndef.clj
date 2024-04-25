(ns tieminos.sc-utils.synths.scsyndef
  "Generate synths from compiled SuperCollider scsyndef files."
  (:require [overtone.core :as o]
   [overtone.sc.machinery.synthdef :refer [load-synthdef synthdef-read]]))

(comment
  ;; Example
  (def sini (make-synth
              {:synthdef-path "/Users/diego/Music/code/tieminos/src/tieminos/sc_utils/synths/sini.scsyndef"
               :name "sini"
               :params {:freq 200}}))
  (sini :freq 800)
  (o/stop))


(defn params->synth-params
  [params]
  (map (fn [[name* value]]
         (let [value* (float value)]
           {:name (name name*) :default value* :rate :kr :value (atom value*)}))
       params))

(defn synth-params->args
  [synth-params]
  (map :name synth-params))

(defn make-synth
  [{:keys [name
           synthdef-path
           params]}]
  (let [sdef (synthdef-read synthdef-path)
        params* (params->synth-params params)
        args (synth-params->args params*)
        smap (with-meta
               (o/map->Synth
                 {:name name
                  :sdef sdef
                  :args args
                  :params params*
                  :instance-fn identity})
               {:overtone.live/to-string #(str (name (:type %)) ":" (:name %))})]
    (load-synthdef sdef)
    (o/event :new-synth :synth smap)
    smap))


(comment
  (synthdef-read "/Users/diego/Music/code/tieminos/src/tieminos/habitat/extended_sections/hacia_un_nuevo_universo/scsyndefs/amp-regulator-replier.scsyndef")
  )
