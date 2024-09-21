(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.clouds
  (:require
   [clojure.data.generators :refer [weighted]]
   [tieminos.compositions.garden-earth.synths.recording :as rec]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.recording :refer [norm-amp silence?]]
   [tieminos.habitat.routing :refer [main-returns]]
   [tieminos.habitat.scratch.sample-rec2 :refer [rand-latest-buf]]
   [tieminos.habitat.synths.granular :refer [amanecer*guitar-clouds]]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(defn clouds-refrain
  "This version can handle rate chords (as a vector of rates)"
  [{:keys [id
           buf-fn
           durs-fn
           rates-fn
           amp-fn ;; optional, takes the index and returns an amp value, if present `amp` will be overriden
           d-level-weights room-weights
           out-bus silence-thresh
           on-play]
    :or {id :clouds-refrain
         buf-fn rand-latest-buf
         amp-fn (fn [_] 1)
         d-level-weights {0.3 1}
         room-weights {0.2 2, 2 1/2 4 1/2}
         out-bus (main-returns :non-recordable)
         silence-thresh 0.05}}]

  (ref-rain
    :id id
    :durs durs-fn
    :on-event (on-event
                (when-let [buf (buf-fn {:index index})]
                  (when-not (silence? silence-thresh buf)
                    (let [rates (rates-fn {:index index})
                          _ (println rates)
                          rates* (if (sequential? rates) rates [rates])
                          amp* (amp-fn [{:index index}])]
                      (when on-play (println "ONPLAY"))
                      (doseq [r rates*]
                        (let [trig-rate (+ 90 (rand-int 20))
                              config {:group (groups/mid)
                                      :buf buf
                                      :d-level (weighted d-level-weights)
                                      :rev-room (weighted room-weights)
                                      :trig-rate 100
                                      :grain-dur (/ 1 (/ trig-rate 2))
                                      :amp-lfo (rrange 0.1 0.4)
                                      :amp-lfo-min 0.95
                                      :lpf-max (rrange 2000 10000)
                                      :start 0
                                      :end 1
                                      :interp (rand-nth [1 2 4])
                                      :out out-bus
                                      :pan (rrange -1 1)}]
                          (if on-play
                            (on-play (assoc config
                                            :amp amp*
                                            :rate r
                                            :index i))
                            (do
                              (amanecer*guitar-clouds (assoc config
                                                             :rate (float r)
                                                             :interp (rand-nth [1 2 4])
                                                             :amp (* amp* (rrange 0.2 1) (norm-amp buf))))
                              (amanecer*guitar-clouds (assoc config
                                                             :rate (* (rand-nth [2 3/2 5/4 7/4 1/2 1 1 1 1]) r)
                                                             :interp (rand-nth [4])
                                                             :amp (* amp* (rrange 0 0.7) (norm-amp buf))))))))))))))


(comment
  (clouds-refrain
    {:id :my-clouds
     :durs-fn (fn [_] (rand-nth [1 2 3]))
     :buf-fn (fn [_] (last rec/bufs))
     :rates-fn (fn [{:keys [index]}] (wrap-at index [1 2 [3 4]]))
     :amp-fn (fn [_] (* 0.1 (rand-nth [1 2 3])))
     :on-play println}))
