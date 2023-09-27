(ns tieminos.impros.sept252023
  (:require
   [clojure.string :as str]
   [overtone.midi :as midi]
   [tieminos.midi.core :refer [midi-in-event]]
   [tieminos.piraran.scale :refer [polydori-set->deg polydori-v2]]
   [tieminos.utils :refer [map-subscale-degs]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

;; Today I explored:
;;- sending midi from keyboad to reaper
;;- mapping midi notes for a subcps
;;- saving chords

(def sink (midi/midi-out "VirMIDI"))

(def current-sets (atom {}))

(defonce fav-chords (atom {}))

(def chord-id (atom nil))

(defn save-chord
  [id midi-note data]
  (swap! fav-chords assoc-in [id midi-note] data))

(defn set-save-chord!
  []
  (reset! chord-id (random-uuid))
  (println "START - SAVING CHORD ")
  (ref-rain :id ::save-chord
            :loop? false
            :durs [7 1]
            :on-event (on-event
                       (when (= 1 dur)
                         (println "END - SAVING CHORD ")
                         (reset! chord-id nil)))))

(comment
  (-> @fav-chords)
  (reset! fav-chords {})
  (-> @chord-id)
  (set-save-chord!)

  (add-watch current-sets
             :chord
             (fn [_ _ _ sets]
               (let [[notes ratios sets]
                     (->> sets
                          (sort-by first)
                          ((juxt #(map first %)
                                 (fn [notes] (->> notes
                                                  (map (comp :bounded-ratio second))))

                                 (fn [notes] (->> notes
                                                  (map (comp :sets second))
                                                  (map
                                                    (comp
                                                      (partial map (comp
                                                                     #(str/join "." %)
                                                                     sort)))))))))]

                 (when (seq notes)
                   (println notes)
                   (println ratios)
                   (println sets)
                   (println "=====================")))))

  (let [deg-fn (fn [deg]
                 (let [deg* (- (map-subscale-degs
                                 29
                                 dek-3_1-7-9-15-19
                                 deg)
                               60)]
                   deg*))
        deg->set (fn [deg]
                   (-> polydori-v2 :scale
                       (nth (mod deg 29))))]
    (midi-in-event
      :note-on (fn [{:keys [note velocity] :as ev}]
                 (let [note* (deg-fn note)]
                   #_(println note* (deg->set note*))
                   (when @chord-id
                     (save-chord @chord-id note* (deg->set note*)))
                   (swap! current-sets assoc note* (deg->set note*))
                   (midi/midi-note-on sink note* velocity)))
      :note-off (fn [{:keys [note] :as ev}]
                  (let [note* (deg-fn note)]
                    (swap! current-sets dissoc note*)
                    (midi/midi-note-off sink note*)))))

  (-> polydori-v2)
  (-> polydori-v2 :subcps
      keys
      sort)
  (def dek-3_1-7-9-15-19
    (-> polydori-v2 :subcps
        (get "3)5 of 4)7 3-7.9.15.19.21")
        :scale
        (->> (map :set)
             (map polydori-set->deg))))

  (-> polydori-v2 :subcps
      (get "3)5 of 4)7 3-1.7.9.15.19")
      :scale
      (->> (map (comp sort :set))
           (map-indexed vector)))

  (gp/stop)
  (map-subscale-degs
    29
    dek-3_1-7-9-15-19
    0)
  :rcf)
