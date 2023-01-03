(ns tieminos.habitat.parts.amanecer
  (:require
   [clojure.string :as str]
   [erv.cps.core :as cps]
   [tieminos.habitat.panners :refer [panner panner-rate]]
   [tieminos.habitat.routing :refer [inputs processes-return-1]]
   [tieminos.habitat.synths.granular :as hgs]
   [tieminos.math.bezier-samples :refer [f fsf s]]
   [tieminos.math.random-walk :refer [rand-walk1]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(defn init-section-1 [inputs base-preouts]
  (doseq [[k {:keys [bus]}] inputs]
    (let [out (:bus (k base-preouts))
          config {:in bus
                  :type (rand-nth [:clockwise :counter-clockwise :rand])
                  :out out}]
      (when-not (or out bus)
        (throw (ex-info "Can not init section 1, data panner data missing"
                        config)))
      (panner config)
      (panner-rate {:in bus :rate (* 0.15 (rand))}))))

(comment
  (require '[tieminos.habitat.routing :refer [guitar-bus
                                              mic-1-bus
                                              mic-2-bus
                                              mic-3-bus
                                              mic-4-bus
                                              preouts]]
           '[tieminos.habitat.recording :as rec])

  (def subsection "pt5")
  (rec/rec-input {:section "amanecer"
                  :subsection subsection
                  :input-name "mic-2"
                  :input-bus mic-2-bus
                  :dur-s 7})
  (rec/rec-input {:section "amanecer"
                  :subsection subsection
                  :input-name "mic-3"
                  :input-bus mic-3-bus
                  :dur-s 2})
  (rec/rec-input {:section "amanecer"
                  :subsection subsection
                  :input-name "mic-4"
                  :input-bus mic-4-bus
                  :dur-s 7})
  (rec/rec-input {:section "amanecer"
                  :subsection subsection
                  :input-name "guitar"
                  :input-bus guitar-bus
                  :dur-s 5}))
(comment
  (defn get-buf-keys [substr]
    (->> @rec/bufs keys (filter #(str/includes? (name %) substr)) (sort-by name)))
  (gp/stop)
  (reset! rec/bufs {})
  (-> @rec/bufs)

  hgs/amanecer*snare-mist
  (let [rates (->> (cps/make 2 [1 3 5 7 9] :norm-fac (* 17 21)) :scale (map :bounded-ratio))
        segment-start (atom 0)
        durs-mult (rrange 1 3)
        durs (map #(* durs-mult %) ((rand-nth [fsf f s])
                                    20
                                    (rand-nth [0.1 0.5 0.3])
                                    (rand-nth [1 0.8 2])))
        pan-poss (rand-walk1 0.5 (count durs))
        [amp rate-harm lpf-max] (rand-nth [[37 [1 1/2 2] 400]
                                           [17 [12 8 10 4] 4000]])]

    (ref-rain
     :id (str "amanercer*nubes" (rand-int 7777))
     :durs durs
     :loop? false
     :ratio 1
     :on-event (on-event
                (let [buf ((-> (get-buf-keys "pt5") first) @rec/bufs) #_(rand-nth (vals @rec/bufs))
                      start (mod @segment-start 1)
                      end (swap! segment-start (fn [_] (+ start (rand (- 1 start)))))
                      rate (*  (rand-nth rate-harm) (rand-nth rates))]
                  (println index dur rate (at-index pan-poss))
                  (hgs/amanecer*guitar-clouds
                   (merge
                    {:buf buf
                     :a (rand-nth [0.1 (* dur-s 0.3)])
                     :a-level 0.5
                     :d (* dur-s 0.5)
                     :trig-rate 160
                     :grain-dur 1/80
                     :rate rate
                     :d-level (rrange 0.2 0.5)
                     :r (* 5 dur-s)
                     :amp amp
                     :start start
                     :end end
                     :pan (at-index pan-poss)
                     :lpf-min 100
                     :lpf-max (rand-nth [lpf-max])
                     :rev-mix (rrange 0 1)
                     :rev-room (rrange 0.5 1)
                     :out processes-return-1}
                    #_(hgs/rand-start-end))))))))
