(ns tieminos.midi.plain-algo-note
  "Send MIDI events to sink, with preprogammed note-off.
  This is a WIP.

  For MPE supported preprogrammed messages use `tieminos.midi.algo-note`"
  (:require
   [overtone.midi :as midi]
   [tieminos.utils :refer [map-subscale-degs]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(comment
  ;; USAGE
  (def sink (midi/midi-out "VirMIDI"))
  (algo-note {:sink sink
              :dur  0.5
              :note 79
              :vel 80})
  ;; try with a 64 degrees scale
  (malgo-note {:sink sink
               :dur  10
               :vel 80
               :chan 6
               :scale-size 64
               :base-midi-deg 69
               :base-midi-chan 6
               :deg -64})

  ;; a useful pattern
  (def my-malgo #(malgo-note (merge {:sink sink
                                     :scale-size 64
                                     :base-midi-deg 69
                                     :base-midi-chan 6}
                                    %)))

  (my-malgo {:deg -64
             :dur  10
             :vel 80}))

(defn- algo-note-fn
  [sink dur note vel chan tempo offset]
  (let [note* (+ offset note)]
    (ref-rain
     :id (random-uuid)
     :durs [dur 0.1]
     :tempo 60
     :loop? false
     :on-event (on-event
                (if (zero? index)
                  (midi/midi-note-on sink note* vel chan)
                  (midi/midi-note-off sink note* chan))))))

(defn algo-note
  [{:keys [sink dur note vel chan tempo offset]
    :or {chan 0
         tempo 60
         offset 0}}]
  (if (sequential? note)
    (doseq [n note]
      (algo-note-fn sink dur n vel chan tempo offset))
    (algo-note-fn sink dur note vel chan tempo offset)))

(quot (- 210 127) 29)
(- 210 (* 3 29))
(do
  (defn midi-mapper
    "This returns a midi mapping function that takes a `scale-deg` and returns the corresponding midi note
  The `base-midi-deg` must be a midi note that correspondes to the first degree of the provided scale.
  The `base-midi-chan` is the channel that refers to the given `base-midi-deg`."
    [scale-size base-midi-deg base-midi-chan scale-deg]
    #_(println scale-size base-midi-deg base-midi-chan scale-deg)

    (let [new-deg  (+ base-midi-deg scale-deg)
          chan (cond (neg? (- new-deg 127)) 0
                     :else (inc (quot (max 0 (- new-deg 127)) scale-size)))
          note (- new-deg (* chan scale-size))]
      #_(println {:new-deg new-deg
                :note note
                :chan chan})
      {:new-deg new-deg
       :note note
       :chan chan}))
  [(midi-mapper 29 60 0 15)
   (midi-mapper 29 60 0 70)
   (midi-mapper 29 60 0 150)])

(defn malgo-note
  "`algo-note` with `midi-mapper`.
  Notice that `:deg` substitutes `:note` and `:base-midi-chan` substitutes `:chan`

  NOTE that `base-midi-chan` is zero indexed while midi channels are 1 indexed"
  ;; TODO add/test subscale-degs
  [{:keys [sink dur vel tempo offset
           deg scale-size base-midi-deg base-midi-chan
           subscale]
    :or {dur 1
         vel 64
         tempo 60
         offset 0
         base-midi-chan 0}}]
  (algo-note
   (merge {:sink sink
           :dur dur
           :vel vel
           :tempo tempo
           :offset offset}
          (midi-mapper scale-size
                       base-midi-deg
                       base-midi-chan
                       (if-not (seq subscale)
                         deg
                         (map-subscale-degs scale-size
                                            subscale
                                            deg))))))

(comment
  (require '[time-time.dynacan.players.gen-poly :as gp])
  (let [subscale [0 2 4 5 7 9 11]]
    (gp/ref-rain
     :id :test
     :loop? false
     :durs (repeat 12 1)
     :on-event (gp/on-event
                (println index)
                (malgo-note
                 {:sink sink
                  :dur  1
                  :scale-size 12
                  :base-midi-deg 69
                  :base-midi-chan 6
                  :subscale subscale
                  :deg (* -1 index)})))))
