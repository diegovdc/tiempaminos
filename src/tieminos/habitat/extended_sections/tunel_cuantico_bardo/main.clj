(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.main
  "The code from the recorded versions of `2.2.9"
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.habitat.extended-sections.hacia-un-nuevo-universo.main-4ch
    :as hunu.4ch]
   [tieminos.habitat.extended-sections.harmonies.chords
    :refer [fib-chord-seq meta-slendro1 rate-chord-seq transpose-chord]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.osc :as bardo.osc]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.save-synths
    :as tc.synth-persistance]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.init :as habitat]
   [tieminos.habitat.main :as main]
   [tieminos.habitat.main-sequencer :as hseq]
   [tieminos.habitat.recording :as rec :refer [norm-amp silence?]]
   [tieminos.habitat.routing :as habitat.route :refer [main-returns]]
   [tieminos.habitat.scratch.sample-rec2
    :refer [periodize-durs quad-router-2o rand-latest-buf rev-filter
            start-rec-loop3!]]
   [tieminos.habitat.synths.granular
    :refer [amanecer*guitar-clouds clouds2-4ch]]
   [tieminos.math.bezier-samples :as bzs]
   [tieminos.network-utils :refer [get-local-host]]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.utils :refer [rrange wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.scratch.main]))

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
      :default-out (habitat.route/get-mixed-main-out)}))

  (def test-synths
    (tc.synth-persistance/rehydrate-synth-params
     {:buffers-db-keyword-prefix :test/gusano-cuantico-2.2.9.2
      :params-file-name "test-gusano-cuantico-2.2.9.2.edn"
      :groups @groups/groups
      :default-group (groups/mid)
      :default-out (habitat.route/get-mixed-main-out)}))

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
   {:out (habitat.route/get-mixed-main-out)}))

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
    (if (> (count @smooth-configs) 15)
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

(comment
  (when @habitat/habitat-initialized?
    (reset! rec/recording? {})
    (main/stop-sequencer! hseq/context)
    (reset! rec/bufs {}))
  (o/stop)
  (o/kill qbr)
  (o/kill rev-filter*)

  (habitat/init! {:volume-db -24})

  (bardo.osc/init! [["127.0.0.1" 16181]
                    ["192.168.0.101" 16180]
                    ["192.168.0.102" 16180]])

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
  (o/demo (o/in (-> @habitat.route/inputs
                    :mic-2
                    :bus)))

  (hunu.4ch/open-inputs-with-rand-pan*
   {:inputs habitat.route/inputs
    :preouts habitat.route/preouts}
   {:mic-1 {:width 3}
    :mic-2 {:width 3}
    :guitar {:width 3}}
   #_{:mic-1 {:amp 1}
      :mic-2 {:amp 1}})

  (add-watch bardo.live-state/live-state ::post-live-state
             (fn [_key _ref _old-value new-value]
               (println new-value)
               (bardo.osc/throttled-post (dissoc new-value :lorentz)))))

(comment
  (require '[tieminos.overtone-extensions :as oe])

  (oe/defsynth sini
    [freq 200
     amp 0.5
     out 0]
    (o/out out (* amp (o/pan2 (o/sin-osc 200)))))

  (def test-sini (sini :out 20))
  (o/kill test-sini))
