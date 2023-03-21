(ns tieminos.habitat.parts.amanecer
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.string :as str]
   [erv.cps.core :as cps]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.attractors.lorentz :as lorentz]
   [tieminos.habitat.attractors :refer [lor1]]
   [tieminos.habitat.groups :as groups]
   [tieminos.habitat.panners :refer [panner panner-rate stop-panner!]]
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
            reverb-return]]
   [tieminos.habitat.synths.convolution
    :refer [live-convolver live-convolver-perc]]
   [tieminos.habitat.synths.granular :as hgs]
   [tieminos.habitat.utils
    :refer [before-dur-end free-synth-panner-and-bus]]
   [tieminos.math.bezier-samples :refer [f fsf s]]
   [tieminos.math.random-walk :refer [rand-walk1]]
   [tieminos.utils :refer [ctl-synth ctl-synth2 iter-async-call rrange]]
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

(defn humedad [context]
  (timbre/info "humedad")
  (let [{:keys [inputs preouts]} @context]
    (doseq [[k {:keys [bus]}] @inputs]
      (let [out (:bus (k @preouts))
            trayectory (make-initial-trayectory-data)
            config {:in bus
                    :type :trayectory
                    :out out
                    :trayectory trayectory}]
        (panner config)))))

(defn- rayos-y-reflejos
  [id total-dur input]
  (let [[k {:keys [bus]}] input
        elapsed-time (atom 0)
        last-synth (atom nil)
        id* (keyword (str "rayos-y-reflejos-" id "-" (name k)))]
    (timbre/debug "Starting" id*)
    (when-not bus
      (throw (ex-info "No input bus" {:input input})))
    (ref-rain
     :id id*
     :durs (map (fn [_] (rrange 0.2 1.5)) (range 100))
     :tempo 60
     :on-stop (fn [_]
                (timbre/debug "Stopping" id*)
                (ctl-synth2 @last-synth :gate 0))
     :on-event
     (on-event
      (let [pt-1-dur (rand dur-s)
            pt-2-dur (- dur-s pt-1-dur)
            start-pos (rand 2)
            end-pos (+ start-pos (rrange -1 1))]
        (if (> @elapsed-time total-dur)
          (do
            (timbre/debug "Stopping" id*)
            (ctl-synth2 @last-synth :gate 0)
            (update-refrain id* :before-update (fn [r] (assoc r :playing? false))))
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

(defn sol-y-luminosidad [context]
  (timbre/info "sol-y-luminosidad")
  (let [{:keys [inputs preouts dur-s]} @context
        guitar (:guitar @inputs)
        guitar-trayectory [{:pos -0.5 :dur 15 :width 4}
                           {:pos -0.5 :dur 45 :width 1.3}
                           {:pos -0.5 :dur 70 :width 1.3}]
        perc-inputs (dissoc @inputs :guitar)]
    ;; guitar
    (panner {:in (:bus guitar)
             :type :trayectory
             :out (:bus (:guitar @preouts))
             :amp 1.5
             :trayectory guitar-trayectory})
    ;; perc
    (doseq [input perc-inputs]
      (rayos-y-reflejos "sol-y-luminosidad" dur-s input)
      (let [[k {:keys [bus]}] input]
        (panner {:in bus
                 :type :rand
                 :out (:bus (k @preouts))
                 :amp 0.5})
        (panner-rate {:in bus
                      :rate (rrange 0.1 0.5)
                      :max 0.5})))))

(do
  (defn quick-jump-trayectories
    "Quick jumps from one place to another"
    [total-dur dur-weights & {:keys [min-width max-width]
                              :or {max-width 1.5
                                   min-width 1.3}}]
    (let [positions (loop [positions []]
                      (let [elapsed-time (:elapsed-time (last positions) 0)
                            dur (weighted dur-weights)]
                        (if  (> elapsed-time total-dur)
                          positions
                          (recur (conj positions
                                       {:pos (+ (rand-nth [0 0.5 1 1.5]) (rand 0.25))
                                        :width (rrange min-width max-width)
                                        :dur dur
                                        :elapsed-time (+ elapsed-time dur)})))))]
      (->> positions
           (mapcat (fn [p] [p (-> p
                                  (assoc :dur (rrange 0.1 0.3)
                                         :transition? true)
                                  (dissoc :elapsed-time))]))))))
(do
  (defn make-convolver-1
    "Shuffles inputs and has random ranged defaults for most args.
  NOTE that ins is [{:bus bus} {:bus bus}]"
    [ins out & {:as live-convolver-args
                :keys [min-dur max-dur]
                :or {min-dur 1 max-dur 15}}]
    (live-convolver-perc
     (let [dur (rrange min-dur max-dur)
           a (* dur (rrange 0.01 0.5))
           [in1 in2] (shuffle ins)
           amp-lfo-min (rrange 0.4 1)]
       (merge {:group (groups/mid)
               :in1 (:bus in1)
               :in1-amp 2
               :in2 (:bus in2)
               :in2-amp 2
               :amp-lfo-freq (rrange 0.01 0.8)
               :amp-lfo-min amp-lfo-min
               :amp-lfo-max (rrange amp-lfo-min 1.2)
               :amp (rrange 1 2)
               :a a
               :r (- dur a)
               :curve (rrange -4 4)
               :max-amp (rrange 0.4 0.8)
               :hpf-freq (rrange 300 800)
               :lpf-freq (rrange 2000 10000)
               :bpf-amp (rrange 0.5 0.8)
               :bpf-rev-amp (rrange 1 1.5)
               :rev-mix (rrange 0 1)
               :out out}
              (dissoc live-convolver-args
                      :min-dur
                      :max-dur)))))

  (defn intercambios-de-energia [context]
    (timbre/info "intercambios-de-energia")
    (let [{:keys [inputs dur-s]} @context
          input-pairs [[(:guitar @inputs) (:mic-1 @inputs)]
                       [(:guitar @inputs) (:mic-2 @inputs)]
                       [(:guitar @inputs) (:mic-4 @inputs)]
                       [(:mic-3 @inputs) (:mic-5 @inputs)]
                       [(:mic-6 @inputs) (:mic-7 @inputs)]]
          total-synths (count input-pairs)
          trayectories (mapv (fn [_]
                               (into (quick-jump-trayectories (/ dur-s 2)
                                                              {#(rrange 4 8) 3
                                                               #(rrange 8 12) 1}
                                                              :min-width 1.3
                                                              :max-width 4)
                                     (quick-jump-trayectories (/ dur-s 2)
                                                              {#(rrange 4 8) 3
                                                               #(rrange 1 4) 1}
                                                              :min-width 1.3
                                                              :max-width 4)))
                             (range total-synths))
          convolver-outs (mapv (fn [i] (o/audio-bus 1 (str "intercambios-de-energia-convolver-out-" i)))
                               (range total-synths))
          panners (mapv (fn [convolver-out trayectory]
                          (panner {:in convolver-out
                                   :type :trayectory
                                   :out (reaper-returns 3)
                                   :trayectory trayectory
                                   :amp 3}))
                        convolver-outs
                        trayectories)
          sequencer-ids (mapv (fn [i]
                                (keyword "amanecer" (str "intercambios-de-energia-" i)))
                              (range total-synths))]
      (doall (map-indexed
              (fn [i sequencer-id]
                (ref-rain
                 :id sequencer-id
                 :durs (map :dur (nth trayectories i))
                 :loop? false
                 :on-event (on-event
                            (cond
                              (zero? index) nil
                              (< dur 1) nil
                              (> (rand) 0.0) (make-convolver-1
                                              (rand-nth input-pairs)
                                              (rand-nth convolver-outs))
                              :else nil))))
              sequencer-ids))
      (swap! context assoc
             :amanecer/intercambios-de-energia
             {:sequencer-ids sequencer-ids
              :convolver-outs convolver-outs}))))

(defn free-intercambios-de-energia
  [context]
  (timbre/info "free-intercambios-de-energia")
  (let [{:keys [amanecer/intercambios-de-energia]} @context
        {:keys [convolver-outs sequencer-ids]} intercambios-de-energia]
    (ref-rain
     :id :amanecer/free-intercambios-de-energia
     :durs [10 10 5]
     :loop? false
     :on-event (on-event
                (case index
                  0 (doseq [sequencer-id sequencer-ids]
                      (gp/stop sequencer-id))
                  1 (doseq [out convolver-outs]
                      (stop-panner! out))
                  2 (doseq [out convolver-outs]
                      (o/free-bus out))
                  (timbre/warn "free-intercambios-de-energia should have stopped by now"))))))

(defn inicio-descomposicion [context]
  (timbre/info "inicio-descomposicion")
  (free-intercambios-de-energia context)
  (let [{:keys [inputs preouts texto-sonoro-rand-mixer-bus]} @context
        convolvers (mapv (fn [[k {:keys [bus]}]]
                           (let [convolver-out (o/audio-bus 1 (str "convolver-out-" (name k)))
                                 synth (live-convolver {:group (groups/mid)
                                                        :in1 bus
                                                        :in2 @texto-sonoro-rand-mixer-bus
                                                        :out convolver-out})]
                             (panner {:group (groups/panners)
                                      :in convolver-out
                                      :type :rand
                                      :out (:bus (k @preouts))})
                             {:synth synth
                              ;; use `out-bus` to release the panner
                              :out-bus convolver-out}))
                         @inputs)]
    (swap! context assoc :amanecer/inicio-descomposicion
           {:convolver-synths convolvers})))

(defn descomposicion-hacia-la-tierra [context]
  (timbre/info "descomposicion-hacia-la-tierra")
  (let [{:keys [dur-s amanecer/inicio-descomposicion]} @context
        {:keys [convolver-synths]} inicio-descomposicion]
    (before-dur-end
     dur-s 0.5
     (fn [] (doseq [{:keys [synth out-bus]} convolver-synths]
              (free-synth-panner-and-bus synth out-bus))))))

(defn coro-de-la-manana-cantos-iniciales
  [context]
  (timbre/info  "coro-de-la-manana-cantos-iniciales")
  (let [{:keys [dur-s inputs preouts main-fx]} @context
        guitar (:guitar @inputs)
        perc-inputs (dissoc @inputs :guitar)]
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
               :out (:bus (:heavy-reverb @main-fx))
               :trayectory trayectory
               :amp 3})

      ;; TODO review and improve perc
      (doseq [input perc-inputs]
        (let [[k {:keys [bus]}] input]
          (panner {:in bus
                   :type :rand
                   :out (:bus (k @preouts))
                   :amp 1})
          (panner-rate {:in bus
                        :rate (rrange 0.1 0.5)
                        :max 0.5}))))))

(defn coro-de-la-manana-interacciones-cuanticas [context]
  (timbre/info "coro-de-la-manana-interacciones-cuanticas")
  (let [{:keys [inputs preouts dur-s]} @context
        guitar (:guitar @inputs)
        perc-inputs (dissoc @inputs :guitar)
        make-trayectory (fn [] (quick-jump-trayectories
                                (+ dur-s 10)
                                {#(rrange 0.5 1) 4
                                 #(rrange 0.2 0.5) 3
                                 #(rrange 1 3) 2
                                 #(rrange 3 4) 1.5
                                 #(rrange 5 10) 1}))]
    ;; TODO improve, it just a quick thing
    (panner {:in (:bus guitar)
             :type :trayectory
             :out (:bus (:guitar @preouts))
             :trayectory (make-trayectory)
             :amp 1})

    (doseq [input perc-inputs]
      (let [[k {:keys [bus]}] input]
        (panner {:in bus
                 :type :trayectory
                 :trayectory (make-trayectory)
                 :out (:bus (k @preouts))
                 :amp 1})))))

(defn coro-de-la-manana-distancia-de-la-escucha [context]
  (timbre/info "coro-de-la-manana-distancia-de-la-escucha")
  (let [{:keys [inputs preouts texto-sonoro-rand-mixer-bus]} @context
        convolvers (mapv
                    (fn [[k {:keys [bus]}]]
                      (let [convolver-out (o/audio-bus 1 (str "coro-de-la-mañana-distancia-convolver-out-" (name k)))
                            synth (live-convolver {:group (groups/mid)
                                                   :in1 bus
                                                   :in2 @texto-sonoro-rand-mixer-bus
                                                   :out convolver-out})
                            starting-point (+ 500 (rand-int 300))
                            ctl-stop-fn (iter-async-call
                                         100
                                         (fn [i]
                                           (let [point (lor1 (+ starting-point (* 5 i)))]
                                             (ctl-synth
                                              synth
                                              :in1-amp (lorentz/bound point :x 1 2)
                                              :in2-amp (lorentz/bound point :y 0.5 1)
                                              :rev-mix (lorentz/bound point :x 0.3 1)
                                              :rev-room (lorentz/bound point :y 0 0.5)
                                              :amp (lorentz/bound point :z 0 0.9)))))]

                        (panner {:group (groups/panners)
                                 :in convolver-out
                                 :type :rand
                                 :out (reaper-returns 3)})
                        (panner-rate {:in bus
                                      :rate (rrange 0.1 0.5)
                                      :max 0.5})
                        {:synth synth
                         :ctl-stop-fn ctl-stop-fn
                          ;; use `out-bus` to release the panner
                         :out-bus convolver-out}))
                    @inputs)]
    (swap! context assoc :amanecer/coro-de-la-manana-distancia-de-la-escucha
           {:convolver-synths convolvers})))

(defn coro-de-la-manana-distancia-de-la-escucha-stop
  [context]
  (timbre/info "coro-de-la-manana-distancia-de-la-escucha-stop")
  (let [{:keys [convolver-synths]} (:amanecer/coro-de-la-manana-distancia-de-la-escucha
                                    @context)]
    (ref-rain
     :id :amanecer/coro-de-la-manana-distancia-de-la-escucha-stop
     :durs [5 10 5]
     :loop? false
     :on-event (on-event
                (case index
                  0 (doseq [{:keys [ctl-stop-fn synth]} convolver-synths]
                      (ctl-stop-fn)
                      (ctl-synth2 synth :gate 0))
                  1 (doseq [{:keys [out-bus]} convolver-synths]
                      (stop-panner! out-bus))
                  2 (doseq [{:keys [out-bus]} convolver-synths]
                      (o/free-bus out-bus))
                  (timbre/warn "coro-de-la-manana-distancia-de-la-escucha-stop should have stopped by now"))))))

(defn solo-de-milo [context]
  (timbre/info "solo-de-milo")
  (coro-de-la-manana-distancia-de-la-escucha-stop context)
  (let [{:keys [inputs preouts dur-s]} @context
        perc-inputs (dissoc @inputs :guitar)]
    (doseq [input perc-inputs]
      (rayos-y-reflejos "solo-de-milo" dur-s input))))

