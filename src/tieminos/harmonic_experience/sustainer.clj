(ns tieminos.harmonic-experience.sustainer
  "Call a synthdef and sustain it until it's no longer needed.
  Sustaining is controlled by an `id`, if the `params` change, then `o/ctl` and update.
  All used `inst` (instruments) should have a `:gate`.
  TODO: If `inst` changes retrigger the synth.
  NOTE: this does not (yet) work with `template-synth`s (see `do-play!`'s `o/ctl` call)."
  (:require
   [clojure.set :as set]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]))

(comment
  (require '[tieminos.overtone-extensions :as oe]
           '[tieminos.harmonic-experience.drones.sounds :refer [drone]])
  (def root 200)
  (play! :id1 {:inst drone :params {:freq (* root 1)}})
  (stop! :id1)
  (multi :my-chord
         {1 {:inst drone :params {:freq (* root 1)}}
          3/2 {:inst drone :params {:freq (* root 3/2)}}})
  (multi :my-chord {}))

(defonce ^:private state (atom {}))
(declare stop! play!)
(:synth (get @state 1))

(defn- do-play!
  [id]
  (let [{:keys [synth inst params prev-params] :as play-data} (get @state id)
        inst* (-> synth meta :inst)]
    (cond
      ;; start a synth
      (nil? synth) (let [synth (with-meta (inst params)
                                 {:inst inst})]
                     (swap! state #(-> %
                                       (assoc-in [id :synth] synth)
                                       (assoc-in [id :prev-params] params))))
      ;; restart a synth
      (and synth (not= inst* inst))
      (do (stop! id) (play! id play-data))
      ;; update a synth
      synth (when-not (= params prev-params)
              (swap! state assoc-in [id :prev-params] params)
              (doseq [[p v] params]
                (when (number? v)
                  (o/ctl synth p v))))
      :else (timbre/warn "Unknown do-play! state" id))))

(defn play!
  [id {:keys [_inst _params] :as play-data}]
  (swap! state update-in [id] merge play-data)
  (do-play! id))
(update-in {} [1] merge {:hola :mundo})

(update-in {1 {:hola :mundo}} [1] merge {:adios :mundo})

(defn stop!
  [id]
  (let [synth (get-in @state [id :synth])]
    (swap! state dissoc id)
    (when synth
      (o/ctl synth :gate 0))))

(defn multi
  "`play-map` is a map of `{id {inst params}`"
  ([play-map] (multi ::default-chord play-map))
  ([chord-id play-map]
   (let [path [::chords chord-id]
         ids (set (keys play-map))
         prev-ids (get-in @state path #{})
         ids-to-stop (set/difference prev-ids ids)]
     (swap! state assoc-in path ids)
     (doseq [id ids-to-stop] (stop! id))
     (doseq [[id play-data] play-map] (play! id play-data)))))
