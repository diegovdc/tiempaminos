(ns tieminos.habitat.parts.amanecer
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.string :as str]
   [erv.cps.core :as cps]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.panners :refer [panner panner-rate]]
   [tieminos.habitat.panners.trayectory-panner
    :refer [random-trayectories
            simple-perc-trayectory-4ch
            trayectory-noise
            trayectory-pan-4ch]]
   [tieminos.habitat.routing
    :refer [clean-return
            input-number->bus*
            inputs
            processes-return-1
            reaper-returns
            reverb-return
            texto-sonoro-rand-mixer-bus]]
   [tieminos.habitat.scratch.convolution :refer [live-convolver]]
   [tieminos.habitat.synths.granular :as hgs]
   [tieminos.math.bezier-samples :refer [f fsf s]]
   [tieminos.math.random-walk :refer [rand-walk1]]
   [tieminos.habitat.groups :as groups]
   [tieminos.utils :refer [ctl-synth ctl-synth2 rrange]]
   [time-time.dynacan.players.gen-poly
    :as
    gp
    :refer
    [on-event ref-rain update-refrain]]))

(def ^:private initial-trayectory-begining
  [{:pos 0 :dur 30 :width 1.2}
   {:pos 0 :dur 15 :width 1.2}
   {:pos 0.5 :dur 15 :width 1.6}
   {:pos 1 :dur 15 :width 2}
   {:pos 1.5 :dur 20 :width 2.5}
   {:pos 3 :dur 10 :width 4}
   {:pos 2 :dur 10 :width 4}
   {:pos 3.75 :dur 10 :width 4}
   {:pos 2.5 :dur 12 :width 2}
   {:pos 2 :dur 10 :width 3.5}
   {:pos 2 :dur 10 :width 2.2}])

(defn- make-initial-trayectory-data
  "Should make a panning trayectory spanning around 5.3 minutes"
  []
  (->> (let [trayectory-start (trayectory-noise
                               {:max-pos-change 0.3
                                :max-width-change 0.7
                                :max-dur-change 4
                                :trayectory initial-trayectory-begining})]
         (into
          trayectory-start
          (random-trayectories
           {:num-steps 30
            :total-dur 150
            :max-pos-change 1
            :initial-pos (:pos (last trayectory-start))})))))

(defn init-section-1 [inputs base-preouts _context]
  (println "IS1")
  (doseq [[k {:keys [bus]}] inputs]
    (let [out (:bus (k base-preouts))
          trayectory (make-initial-trayectory-data)
          config {:in bus
                  :type :trayectory
                  :out out
                  :trayectory trayectory}]
      (timbre/info "Initializing section 1 on bus:" bus config)
      (panner config))))

(defn- rayos-y-reflejos
  [total-dur input]
  (let [[k {:keys [bus]}] input
        elapsed-time (atom 0)
        last-synth (atom nil)
        id (keyword (str "rayos-y-reflejos-" (name k)))]
    (timbre/info "Starting" id)
    (when-not bus
      (throw (ex-info "No input bus" {:input input})))
    (ref-rain
     :id id
     :durs (map (fn [_] (rrange 0.2 1.5)) (range 100))
     :tempo 60
     :on-stop (fn [_]
                (timbre/info "Stopping" id)
                (ctl-synth2 @last-synth :gate 0))
     :on-event
     (on-event
      (let [pt-1-dur (rand dur-s)
            pt-2-dur (- dur-s pt-1-dur)
            start-pos (rand 2)
            end-pos (+ start-pos (rrange -1 1))]
        (if (> @elapsed-time total-dur)
          (do
            (timbre/info "Stopping" id)
            (ctl-synth2 @last-synth :gate 0)
            (update-refrain id :before-update (fn [r] (assoc r :playing? false))))
          (do
            (when-not (zero? index)
              (ctl-synth @last-synth :gate 0))
            (reset! last-synth
                    (trayectory-pan-4ch {:in bus
                                         :out (rand-nth [clean-return reverb-return])
                                         :amp (rrange 1 2)
                                         :a (* (rrange 0.05 0.15) dur-s)
                                         :d (* (rrange 0.1 0.2) dur-s)
                                         :s (rrange 0.7 1)
                                         :trayectory (shuffle [{:pos start-pos
                                                                :dur pt-1-dur
                                                                :width 1.3}
                                                               {:pos end-pos
                                                                :dur pt-2-dur
                                                                :width (rrange 1.3 2)}])
                                         :release (rrange 0.5 1.3)}))
            (swap! elapsed-time + dur-s)))
        (swap! elapsed-time + dur-s))))))

(defn sol-y-luminosidad [inputs base-preouts context]
  (let [{:keys [dur-s]} @context
        guitar (:guitar inputs)
        guitar-trayectory [{:pos -0.5 :dur 15 :width 4}
                           {:pos -0.5 :dur 45 :width 1.3}
                           {:pos -0.5 :dur 70 :width 1.3}]
        perc-inputs (dissoc inputs :guitar)]
    (timbre/info "Salida del sol")
    ;; guitar
    (timbre/info "Starting sol-y-luminosidad (guitar main)"
                 (:bus (:guitar base-preouts)))
    (panner {:in (:bus guitar)
             :type :trayectory
             :out (:bus (:guitar base-preouts))
             :amp 1.5
             :trayectory guitar-trayectory})
    ;; perc
    (doseq [input perc-inputs]
      (rayos-y-reflejos dur-s input)
      (let [[k {:keys [bus]}] input]
        (panner {:in bus
                 :type :rand
                 :out (:bus (k base-preouts))
                 :amp 0.5})
        (panner-rate {:in bus
                      :rate (rrange 0.1 0.5)
                      :max 0.5})))))

(defn intercambios-de-energia [inputs base-preouts _context]
  (timbre/warn "Not implemented yet: intercambios-de-energia"))

(defn before-section-end
  [section-dur s-before-end f]
  (ref-rain
   :id (keyword (str "before-section-end-" (rand-int 10000)))
   :durs [(- section-dur s-before-end) s-before-end]
   :on-event (on-event (when (= index 1) (f)))))

(defn inicio-descomposicion [inputs base-preouts context]
  (timbre/info "inicio-descomposicion")
  (let [convolvers (mapv (fn [[k {:keys [bus]}]]
                           (let [convolver-out (o/audio-bus 1 (str "convolver-out-" (name k)))]

                             {:synth (live-convolver {:group (groups/mid)
                                                      :in1 bus
                                                      :in2 texto-sonoro-rand-mixer-bus
                                                      :out convolver-out})
                              :panner (panner {:group (groups/panners)
                                               :in convolver-out
                                               :type :rand
                                               :out (:bus (k base-preouts))})
                              :out-bus convolver-out}))
                         inputs)]
    (swap! context assoc :amanecer/inicio-descomposicion
           {:convolver-synths convolvers})))

(defn descomposicion-hacia-la-tierra [inputs base-preouts context]
  (timbre/info "descomposicion-hacia-la-tierra")
  (let [{:keys [dur-s amanecer/inicio-descomposicion]} @context
        {:keys [convolver-synths]} inicio-descomposicion]
    (before-section-end
     dur-s 5
     (fn [] (doseq [s convolver-synths]
              (ctl-synth s :gate 0))))))

(do
  (defn quick-jump-trayectories
    "Quick jumps from one place to another"
    [total-dur dur-weights]
    (let [positions (loop [positions []]
                      (let [elapsed-time (:elapsed-time (last positions) 0)
                            dur (weighted dur-weights)]
                        (if  (> elapsed-time total-dur)
                          positions
                          (recur (conj positions
                                       {:pos (+ (rand-nth [0 0.5 1 1.5]) (rand 0.25))
                                        :width (rrange 1.3 1.5)
                                        :dur dur
                                        :elapsed-time (+ elapsed-time dur)})))))]
      (->> positions
           (mapcat (fn [p] [p (-> p
                                  (assoc :dur (rrange 0.1 0.3)
                                         :transition? true)
                                  (dissoc :elapsed-time))]))))))

(defn coro-de-la-manana-cantos-iniciales
  [inputs base-preouts main-fx context]
  (let [{:keys [dur-s]} @context
        guitar (:guitar inputs)
        perc-inputs (dissoc inputs :guitar)]
    (let [trayectory (quick-jump-trayectories
                      (+ dur-s 10)
                      {#(rrange 0.5 1) 4
                       #(rrange 1 2) 3
                       #(rrange 2 3) 2
                       #(rrange 3 4) 1})]
      ;; guitar
      ;; TODO add ref-rain with tiny delays and other earcandy
      (panner {:in (:bus guitar)
               :type :trayectory
               :out (:bus (:heavy-reverb main-fx))
               :trayectory trayectory
               :amp 3})

      ;; TODO review and improve perc
      (doseq [input perc-inputs]
        (let [[k {:keys [bus]}] input]
          (panner {:in bus
                   :type :rand
                   :out (:bus (k base-preouts))
                   :amp 1})
          (panner-rate {:in bus
                        :rate (rrange 0.1 0.5)
                        :max 0.5}))))))

(defn coro-de-la-manana-interacciones-cuanticas [inputs base-preouts _context]
  (let [guitar (:guitar inputs)]
    (panner {:in (:bus guitar)
             :type :trayectory
             :out (:bus (:guitar base-preouts))
             :amp 0.5})

    (timbre/warn "Not implemented yet")))

(defn coro-de-la-manana-distancia-de-la-escucha [inputs base-preouts _context]
  (doseq [[k {:keys [bus]}] inputs]
    (let [out (:bus (k base-preouts))
          trayectory (make-initial-trayectory-data)
          config {:in bus
                  :type :trayectory
                  :out out
                  :trayectory trayectory}]
      (timbre/warn "Not implemented yet"))))

(defn solo-de-milo [inputs base-preouts context]
  (let [{:keys [dur-s]} @context
        perc-inputs (dissoc inputs :guitar)]
    (timbre/info "Solo de Milo")
    (doseq [input perc-inputs]
      (rayos-y-reflejos dur-s input))))

(comment
  (def test-bus (o/audio-bus 1 "test-bus"))
  (def s1 ((o/synth (o/out test-bus
                           (* 0.2 (o/lpf (o/saw 300) 1500))))
           (groups/early)))
  (o/kill s1)
  (o/stop)
  (def st (simple-perc-trayectory-4ch
           {:in test-bus
            :out (reaper-returns 1)
            :start-pos 0
            :end-pos 0.5
            :curve 1.2
             ;; :start-width 2
            }))
  (gp/stop)
  (-> @gp/refrains keys)
  (doseq [id [:hola :hola-1 :hola-2 :hola-3]]
    (ref-rain
     :id id
      ;; TODO use durations beziers or something nicer
     :durs (map (fn [_] (+ 1 (rand))) (range 20))
     :loop? true
     :ratio 1
     :on-event (on-event
                (let [start-pos (rand 2)
                      end-pos (+ start-pos (* (rand-nth [1 -1])
                                              (rand)))
                      start-width (+ 1.2 (rand 3))
                      end-width (-> (+ start-width (* (rand-nth [1 -1])
                                                      (rand 2)))
                                    (max 1.2)
                                    (min 2))
                      [a r] (rand-nth [[1 1]
                                       [2 3]
                                       [2 5]
                                       [3 5]])]
                  (simple-perc-trayectory-4ch
                   {:in (input-number->bus* (rand-int 6))
                    :out (reaper-returns (rand-nth [1 2]))
                    :start-pos start-pos
                    :end-pos end-pos
                    :start-width start-width
                    :end-width end-width
                    :curve (rrange 0.1 0.3)
                    :a a
                    :r r
                    :dur (* (rrange 1.5 4) dur-s)
                    :amp 4}))))))

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
