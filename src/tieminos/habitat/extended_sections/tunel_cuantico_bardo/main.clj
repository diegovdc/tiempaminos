(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.main
  "The code from the recorded versions of `2.2.9"
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo.main-4ch
    :as hunu.4ch]
   [tieminos.habitat.extended-sections.harmonies.chords
    :refer [fib-chord-seq transpose-chord]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.save-synths
    :as tc.synth-persistance]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :as habitat]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec :refer [norm-amp silence?]]
   [tieminos.habitat.routing :as routing :refer [main-returns]]
   [tieminos.habitat.scratch.sample-rec2
    :refer [periodize-durs
            quad-router-2o
            rand-latest-buf
            rev-filter
            start-rec-loop3!]]
   [tieminos.habitat.synths.granular
    :refer [amanecer*guitar-clouds clouds2-4ch]]
   [tieminos.math.bezier-samples :as bzs]
   [tieminos.osc.reaper :as reaper]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.utils :refer [rrange wrap-at]]
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
                  (println "silence?" (silence? silence-thresh buf))
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
                                            :rate r
                                            :index i)))
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
           on-play
           clouds-config]
    :or {chord [0 6 12 18]
         transpositions [0]}}]

  (start-rec-loop3!
   {:input-bus-fn (fn [_] (-> @habitat.route/inputs (select-keys [:guitar #_:mic-1 :mic-2]) vals (->> (map :bus))))
    :durs (mapv (fn [_] 5) (range 1))
    :rec-input-config {:section "gusano-cuantico-2.2.9.2"
                       :subsection "algo-2-2-9"}})
  (clouds-refrain
    (merge
      {:out-bus out-bus
       :buf-fn (fn [_] (->> @rec/bufs vals (sort-by :rec/time) reverse (filter :analysis)
                            (remove #(silence? 0.05  %))
                            (take 3) (#(when (seq %) (rand-nth %)))))
       :silence-thresh 0.05
       :rates (fib-chord-seq (transpose-chord chord transpositions))
       :amp 0.6
       :period 30
       :durs [2 3 5 3 8 13 5 8 2 3 5]
       ;; :period 40
       ;; :durs [1 1 1 1 1 1 1]
       :d-weights {5 1
                   3 0.3}
       :d-level-weights {0.3 5
                         0.1 2
                         0.2 3
                         0.4 8}
       :a-weights {(rrange 5 8) 3
                   (rrange 3 5) 2}
       :on-play on-play}
      clouds-config)))

(defonce saved-synth-params (atom []))

(comment

  (->> @rec/bufs
       vals
       (map #(into {} %)))
  (-> @saved-synth-params)
  (tc.synth-persistance/save-params
   {:buffers-db-keyword-prefix :test/gusano-cuantico-2.2.9.2
    :params-file-name "test-gusano-cuantico-2.2.9.2.edn"
    :params-indexes [0]
    :buffers-atom rec/bufs
    :params-atom saved-synth-params}))

(comment
  (def take-1-synths
    (tc.synth-persistance/rehydrate-synth-params
     {:buffers-db-keyword-prefix :gusano-cuantico-2.2.9.2/take-1
      :params-file-name "gusano-cuantico-2.2.9.2_take-1.edn"
      :groups @groups/groups
      :default-group (groups/mid)
      :default-out (routing/get-mixed-main-out)}))

  (def test-synths
    (tc.synth-persistance/rehydrate-synth-params
     {:buffers-db-keyword-prefix :test/gusano-cuantico-2.2.9.2
      :params-file-name "test-gusano-cuantico-2.2.9.2.edn"
      :groups @groups/groups
      :default-group (groups/mid)
      :default-out (routing/get-mixed-main-out)}))

  ;; example for how to delete a sample
  (rec/delete-sample! (-> @(tc.synth-persistance/get-db-keyword-atom!
                            :test/gusano-cuantico-2.2.9.2)
                          vals
                          first))

  (->> take-1-synths
       (map (comp :duration :buf)))
  (->> test-synths
       first
       :buf
       (into {}))

  (gp/stop ::clouds-ref)
  (let [lor (lorentz/init-system :x 0.3 :y 0.02 :z 0.012)
        durs (take 200 (map #(-> % lor (lorentz/bound :x 2 10))
                            (range 200 40000 50)))
        reso (take 200 (map #(-> % lor (lorentz/bound :y 0 1))
                            (range 200 40000 50)))]
    (ref-rain
     :id ::clouds-ref
     :durs durs
     :on-event (on-event
                (let [start (rand)
                      end (min 1 (+ start (rand)))]
                  #_(clouds2-4ch (weighted
                                  {(-> (nth take-1-synths (weighted {0 1, 3 2}))
                                       (update :amp * 2 (rrand 0.5 1.2))
                                       #_(update :rate * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (update :moog-freq * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (update :d * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (assoc :start start
                                              :end end
                                              :pan (rrange -1 1)
                                              :moog-reso (at-i reso)))
                                   5
                                   (-> (nth take-1-synths (weighted {1 1, 2 8}))
                                       (update :amp * 2 (rrand 0.5 1.2))
                                       (update :rate * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (update :moog-freq * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (update :d * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                       (assoc :start start
                                              :a (rrand 2 4)
                                              :end end
                                              :pan (rrange -1 1)
                                              :moog-reso (at-i reso)))
                                   5}))
                  (clouds2-4ch (-> (rand-nth take-1-synths)
                                   (update :rate * (rand-nth [1 1/2 1/4 2 3/2 2/3]))
                                   (assoc :start start

                                          :end end
                                          :pan (rrange -1 1))))))))
  (keys (:buf (nth take-1-synths 1)))

  (ndef/stop ::loop)
  (ndef/ndef
   ::loop
   (->> (range 5)
        (map (fn [i]
               (let [buf (:buf (rand-nth take-1-synths))]
                 (-> (o/play-buf 1
                                 buf
                                 :rate (* 1 (rand-nth [1 1/2 1/4 3/2]))
                                 :start-pos (rand-int (:n-samples buf))
                                 :loop true)
                     (* 4 (lfo-kr (o/rand 0.5 2) 0.2 1))
                     (o/free-verb)
                     (#(o/pan-az 4 % (lfo-kr 0.1 -1 1)))))))
        o/mix)
   {:out (routing/get-mixed-main-out)}))

(defonce smooth-configs (atom []))

(defn smooth-clouds
  [root
   {:keys [r buf amp rate index]
    :as config}]
  #_(println :smooth-clouds rate)
  (let [index (+ index (rrand -3 3))
        params (merge config
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
    (if (and false (> (count @smooth-configs) 15))
      (do
        (println "#---" index)
        (clouds2-4ch (wrap-at index @smooth-configs)))
      (do
        (println "#" (count @saved-synth-params))
        (swap! saved-synth-params conj params)
        (swap! smooth-configs conj params)
        (clouds2-4ch params)))))

(o/defsynth images
  [buf 0
   rate 1
   a 2
   r 2
   out 0
   amp 1]
  (o/out out
         (-> (o/play-buf 1 buf :rate rate)
             (o/free-verb)
             (* amp (lfo-kr (o/rand 0.5 2) 0.2 1)
                (o/env-gen (o/env-perc a r)
                           :action o/FREE))
             (#(o/pan-az 4 % (lfo-kr 0.1 -1 1))))))

(hunu.4ch/open-inputs-with-rand-pan*
 {:inputs habitat.route/inputs
  :preouts habitat.route/preouts}
 {}
 #_{:guitar {:amp 1
             :type :clockwise
             :rate 1}})

(comment
  (doseq [i (range 60)]
    (o/demo (o/in i
                  1)))
  (o/demo (o/sin-osc)))

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
                    :inbus out1}))

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
               :chord [0 6 12 18]
               :transpositions [0 5 0 5]
               :clouds-config {:amp (o/db->amp -24) ;; NOTE interesante cambiar la amplitud
                              }
               })
  (algo-2-2-9 {:out-bus in1
               :chord [10 15 20]
               :transpositions [0 5 0 5]}))