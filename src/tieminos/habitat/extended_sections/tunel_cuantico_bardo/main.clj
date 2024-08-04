(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.main
  "The code from the recorded versions of `2.2.9"
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.set :as set]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.fib-meru :refer [fib-chord-seq
                                                        transpose-chord]]
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo.main-4ch :as hunu.4ch]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :as habitat]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec :refer [norm-amp silence?]]
   [tieminos.habitat.routing :as habitat.route :refer [main-returns]]
   [tieminos.habitat.scratch.sample-rec2 :refer [hacia-un-nuevo-universo-perc-refrain-v1p2 periodize-durs
                                                 quad-router-2o rand-latest-buf rev-filter start-rec-loop3!]]
   [tieminos.habitat.synths.granular :refer [amanecer*guitar-clouds
                                             clouds2-4ch]]
   [tieminos.math.bezier-samples :as bzs]
   [tieminos.osc.reaper :as reaper]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(comment
  (fib-chord-seq (transpose-chord [0 6 12 18] (range 21))) ;; acorde bonito, muy liso
  (fib-chord-seq (transpose-chord [0 4 8 12 16 20 24 28] [0 1])) ;; calido con un poco de disonancia
  (fib-chord-seq (transpose-chord [11 15 19] (range 21))) ;; estable claro (segmento de arriba: 4-4)
  (fib-chord-seq (transpose-chord [10 15 20] (range 21))) ;; nocturno (5-5)
  )

(defn clouds-refrain
  "This version can handle rate chords (as a vector of rates)"
  [{:keys [buf-fn period durs rates amp id
           amp-fn ;; optional, takes the index and returns an amp value, if present `amp` will be overriden
           d-weights d-level-weights a-weights room-weights out-bus silence-thresh
           on-play]
    :or {id ::clouds-refrain
         buf-fn rand-latest-buf
         period 2.5
         durs (bzs/fsf 20 0.1 1)
         rates (range 1 10)
         amp 1
         a-weights {(rrange 0.01 0.1) 10
                    (rrange 2 5) 1/2}
         d-weights {(rrange 0.2 0.3) 5
                    (rrange 0.3 0.5) 3
                    (rrange 0.5 1) 1
                    (rrange 1 5) 1/2}
         d-level-weights {0.3 1}
         room-weights {0.2 2, 2 1/2 4 1/2}
         out-bus (main-returns :non-recordable)
         silence-thresh 0.05}}]
  (let [rates* (map (fn [r] (if (sequential? r) r [r])) rates)]
    (ref-rain
     :id id
     :durs (periodize-durs period durs)
     :on-event (on-event
                (when-let [buf (buf-fn {:index index})]
                  (when-not (silence? silence-thresh buf) ;; allow us to control silences by not playing
                    (let [rate (at-i rates*)
                          amp* (if amp-fn (amp-fn index) amp)]
                      (doseq [r rate]
                        (let [start 0 #_(rrange (rrange 0 0.5) 0.7)
                              end 1 #_(+ start (rrange 0.05 0.3))
                              a (weighted a-weights)
                              trig-rate (+ 90 (rand-int 20))
                              config {:group (groups/mid)
                                      :buf buf
                                      :a a
                                      :d (/ (+ (/ a 2) (weighted d-weights))
                                            2)
                                      :r (+ (/ a 2) (weighted d-weights))
                                      :d-level (weighted d-level-weights)
                                      :rev-room (weighted room-weights)
                                      :trig-rate 100
                                      :grain-dur (/ 1 (/ trig-rate 2))
                                      :amp-lfo (rrange 0.1 0.4)
                                      :amp-lfo-min 0.95
                                      :lpf-max (rrange 2000 10000)
                                      :start start
                                      :end end
                                      :out out-bus
                                      :pan (rrange -1 1)}]
                          (when on-play
                            (println "ONPLAY")
                            (on-play (assoc config
                                            :amp amp*
                                            :rate r)))
                          (amanecer*guitar-clouds (assoc config
                                                         :rate (float r)
                                                         :interp (rand-nth [1 2 4])
                                                         :amp (* amp* (rrange 0.2 1) (norm-amp buf))))
                          (amanecer*guitar-clouds (assoc config
                                                         :rate (* (rand-nth [2 3/2 5/4 7/4 1/2 1 1 1 1]) r)
                                                         :interp (rand-nth [4])
                                                         :amp (* amp* (rrange 0 0.7) (norm-amp buf)))))))))))))


(defn algo-2-2-9
  [{:keys [chord
           transpositions
           out-bus
           on-play]
    :or {chord [0 6 12 18]
         transpositions [0]}}]

  (start-rec-loop3!
    {:input-bus-fn (fn [_] (-> @habitat.route/inputs (select-keys [:guitar #_:mic-1 :mic-2]) vals (->> (map :bus))))
     :durs (mapv (fn [_] (rrange 10 20)) (range 40))
     :rec-input-config {:section "gusano-cuantico-2.2.9.2"
                        :subsection "algo-2-2-9"}})
  (hacia-un-nuevo-universo-perc-refrain-v1p2
    {:out-bus out-bus
     :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis) (take 10) (#(when (seq %) (rand-nth %)))))
     :silence-thresh 0.05
     :rates (fib-chord-seq (transpose-chord chord transpositions))
     :amp 0.6
     :period 30
     :durs [2 3 5 3 8 13 5 8 2 3 5]
     :d-weights {8 1
                 5 1
                 3 0.3}
     :d-level-weights {0.3 5
                       0.1 2
                       0.2 3
                       0.4 8}
     :a-weights {(rrange 5 8) 3
                 (rrange 3 5) 2}
     :on-play on-play}))

(defonce sparams (atom []))


(do
  (defn serialize-group
    [synth-params]
    (let [group (-> synth-params :group second)]
      (if (-> (@groups/groups group))
        (assoc synth-params :group group)
        (do
          (timbre/warn "Serializing synth-params with unknown group.")
          (dissoc synth-params :group)))))

  (defn serialize-params
    [{:keys [db-full-keyword buf->keys-map synth-params]}]
    (-> synth-params
        (update :buf (fn [b] [db-full-keyword (buf->keys-map b)]))
        serialize-group
        (dissoc :out)
        )


    )
  #_(serialize-params
    {:db-full-keyword :test/gusano-cuantico-2.2.9.2
     :buf->keys-map (set/map-invert @rec/bufs)
     :synth-params (nth @sparams 1)}))
(comment

  (-> @rec/bufs
      set/map-invert)
  (rec/save-samples
    {:description "gusano cuantico 2.2.9.2 (test)"
     :full-keyword :test/gusano-cuantico-2.2.9.2})
  (count @sparams)
  (-> @sparams)
  (-> (nth @sparams 1)
      :buf
      :id)

  (doseq [x (range 3)]
    (-> @sparams
        (nth x)
        clouds2-4ch)))



(defn smooth-clouds
  [root
   {:keys [r buf amp rate]
    :as config}]
  #_(println :smooth-clouds rate)
  (let [params (merge config
                      {:interp 3
                       :trig-rate 10
                       :grain-dur 1/10
                       :rate rate
                       :amp (* amp (norm-amp buf))
                       :dly-mix (rrand 0.8 1.3)
                       :dly-time-mult (rrand 1 2.5)
                       :root root
                       :moog-freq (* (rand-nth [1 2 8 16]) r root)
                       :moog-reso (rrand 0.5 1.3)})]
    (println "#" (count @sparams))
    (swap! sparams conj params)
    (clouds2-4ch params)))

(comment
  (when @habitat/habitat-initialized?
    (reset! rec/recording? {})
    (main/stop-sequencer! hseq/context)
    (reset! rec/bufs {}))
  (o/stop)
  (o/kill qbr)
  (o/kill rev-filter*)

  (habitat/init! {})

  ;; also part of the initialization of hacia un nuevo universo
  (def in1 (o/audio-bus 4 "algo-2.2.9-out"))

  (def out1 (o/audio-bus 4 "reverb-out"))

  (def qbr (quad-router-2o {:group (groups/mid :tail)
                            :in-bus in1
                            :out-bus1 out1
                            :out-bus2 (habitat.route/main-returns :mixed)}))

  (def rev-filter* (rev-filter
                     {:group (groups/panners)
                      :in-bus out1}))

  #_(open-inputs-with-rand-pan
      {:inputs habitat.route/inputs
       :preouts habitat.route/preouts})

  (hunu.4ch/open-inputs-with-rand-pan*
    {:inputs habitat.route/inputs
     :preouts habitat.route/preouts}
    {}
    #_{:guitar {:amp 1
                :type :clockwise
                :rate 1}})


  (algo-2-2-9 {:out-bus in1
               :chord [10 15 20]
               :transpositions [0 -5 0 5]})
  (gp/stop)
  (reaper/stop))


(comment
  (doseq [i (range 60)]
    (o/demo (o/in i
                  1)))
  (o/demo (o/sin-osc))
  )
