(ns tieminos.tierra-mar.v1.state
  (:require
   [clojure.string :as str]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.base :refer [subcps]]
   [tieminos.utils :refer [wrap-at]]))

(defonce state (atom {}))

(def synths-key :synth)

(defn- synth-path
  [k]
  [synths-key k])

(defn add-synth!
  "Keep a synth on the state"
  [k synth]
  (swap! state assoc-in (synth-path k) synth))

(defn get-synth
  [k]
  (if-let [synth (get-in @state (synth-path k))]
    synth
    (timbre/warn "Synth not found:" k)))

(defn set-arp-pattern!
  [state pattern]
  (swap! state assoc
         :arp/pattern-index (:index pattern)
         :arp/pattern pattern))

(defn update-arp-pattern
  [{:keys [arp/pattern-index section] :as state} arp-patterns]
  (let [index (inc (or pattern-index 0))
        pattern (->> arp-patterns
                     (filter #(#{nil section} (:section %)))
                     (wrap-at index))]
    (assoc state
           :arp/pattern-index index
           :arp/pattern pattern)))

(defn set-arp-scale!
  [state {:as _arp-subcps-data
          :keys [name index]}]
  (let [scale (subcps name)]
    (swap! state assoc
           :arp/cps-index  index
           :arp/subcps-name name
           :arp/harmony-strs [(str/replace name #"of 3\)6" "")
                              (str/join " " (map (comp :class :pitch) scale))]
           :arp/scale scale)))
(defn update-arp-scale-data
  [{:keys [arp/cps-index section] :as state} arp-subcps]
  (let [index (inc (or cps-index 0))
        subcps-name (->> arp-subcps
                         (wrap-at section)
                         (wrap-at index))
        scale (subcps subcps-name)]
    (assoc state
           :arp/cps-index  index
           :arp/subcps-name subcps-name
           :arp/harmony-strs [(str/replace subcps-name #"of 3\)6" "")
                              (str/join " " (map (comp :class :pitch) scale))]
           :arp/scale scale)))
