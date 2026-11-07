(ns tieminos.midi.sustainer.v1
  (:require
   [overtone.midi :as midi]))

(defonce state (atom {}))

(defn- notes-on-data->state-data
  [sink notes-on-data]
  (->> notes-on-data
       (mapv (fn [[note vel chan]] [[sink note chan] vel]))
       (into {})))
#_(notes-on-data->state-data :sink [[60 30 0]
                                    [65 30 0]])

(defn- actions
  [prev-state next-state]
  (concat (mapv (fn [data]
                  (let [[note-chan vel] data
                        prev-vel (prev-state note-chan)]
                    (cond
                      (not prev-vel) {:action :add :data data}
                      (and prev-vel
                           (not= prev-vel vel)) {:action :restart :data data}
                      :else {:action :keep :data data})))
                next-state)
          (keep
           (fn [data]
             (let [[note-chan _] data]
               (when-not (next-state note-chan)
                 {:action :remove :data data})))
           prev-state)))

#_(let [prev-state {[:sink 60 0] 30,
                    [:sink 61 0] 30,
                    [:sink 65 0] 30}
        next-state (notes-on-data->state-data :sink [[60 30 0]
                                                     [61 45 0]
                                                     [67 30 0]])]
    (actions prev-state next-state))

(defn- do-actions [actions]
  (doseq [{:keys [action data]} actions]
    (let [[[sink note chan] vel] data]
      (case action
        :keep nil
        :add (midi/midi-note-on sink note vel chan)
        :remove (midi/midi-note-off sink note chan)
        :restart (do (midi/midi-note-off sink note chan)
                     (midi/midi-note-on sink note vel chan))))))

(defn- get-state [id]
  (get @state id {}))

(defn play
  "Sustains the notes in notes-on-data.
  If a note is removed from the structure, it will trigger a note-off event.
  - sink: a Sink
  - id: used to separate different notes-on-data vectors,
        so that they don't interfere with each other
        (i.e. don't trigger note-off events on each other.)
  - notes-on-data: [[note velocity chan]].
  "
  ([sink notes-on-data] (play :default sink notes-on-data))
  ([sink id notes-on-data]
   (let [prev-state (get-state id)
         next-state (notes-on-data->state-data sink notes-on-data)
         actions* (actions prev-state next-state)]
     (do-actions actions*)
     (swap! state assoc id next-state))))

(defn stop
  "Stops all notes of a given id"
  ([id]
   (let [prev-state (get-state id)
         next-state {}
         actions* (actions prev-state next-state)]
     (do-actions actions*)
     (swap! state assoc id next-state))))

(comment
  (def sink (midi/midi-out "VirMIDI"))
  (reset! state {})
  (-> state)
  (play sink :pad [[60 30 2]
                   [65 30 2]
                   [50 30 2]])
  (stop :pad)
  (stop :drums)
  (play sink :drums [[60 30 0]
                     #_[60 20 0]]))
