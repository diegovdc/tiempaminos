(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths
  (:require
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec :as bardo.rec]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synth-management :as bardo.synth-management]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [make-synth-fn plug]]
   [tieminos.sc-utils.synths.v1 :refer [lfo-kr]]
   [tieminos.utils :refer [rrange]]))

(oe/defsynth
  cristal-liquidizado
  ;; Original version from 2.2.9.x
  [buf 0
   rate 1
   amp 0.5
   pan 0
   dur 1
   out 0]
  (o/out out
         (-> (o/play-buf 1 buf rate)
             (* amp
                (o/env-gen
                 (o/envelope
                  [0 1 1 0]
                  [(* 0.1 dur)
                   (* 0.7 dur)
                   (* 0.2 dur)])
                 :action o/FREE))
             (#(o/pan-az:ar 4 % pan)))))

(comment
  (def buf (o/load-sample "samples/habitat_samples/take-1-gusano-cuantico-2.2.9.2-algo-2-2-9-mic-2-bus-43.wav"))
  (cristal-liquidizado
   {:buf buf})

  (oe/defsynth sini
    [freq 200
     amp 0.5
     out 0]
    (o/out out (-> (o/sin-osc 200)
                   (* amp (o/env-gen (o/env-perc) :action o/FREE)))))

  (sini :out 3)

  (oe/defsynth sini-o
    [freq 200
     amp 0.5
     out 0]
    (o/out out (-> (o/sin-osc 200)
                   (* amp (o/env-gen (o/env-perc 2 2) :action o/FREE))
                   (#(oe/circle-az :num-channels 4
                                   :in %
                                   :pos (o/lf-saw 1/2))))))
  (sini-o))

(def random-panaz-plug
  (plug #{:outs :pan-vel :pan-width}
        '((fn [sig] (o/pan-az:ar (count outs) sig
                                 (lfo-kr pan-vel -1 1) ;; LFNoise1
                                 :width pan-width)))))
(defn random-panaz
  [& {:keys [vel width]
      :or {vel 0.5, width 1.4}}]
  {:pan-vel vel
   :pan-width width ;; TODO: control lfo width
   :ugen/pan random-panaz-plug})

lfo-kr
(defn lissajous-pan4
  [& {:keys [vel radius ratio phase]
      :or {vel 1
           radius 1
           ratio 1
           phase Math/PI}}]
  {:liss-freq vel
   :liss-radius radius
   :liss-ratio ratio
   :liss-phase phase
   :ugen/pan '((fn [sig]
                 (o/pan4 sig
                         (* liss-radius (o/sin-osc:kr liss-freq 0))
                         (* liss-radius (o/sin-osc:kr (* liss-freq liss-ratio) liss-phase)))))})
(defn manual-pan4
  [& {:keys [x y]
      :or {x 0, y 0}}]
  {:pan-x x
   :pan-y y
   :ugen/pan '((fn [sig] (o/pan4 sig pan-x pan-y)))})

(defn directional-panaz
  [& {:keys [levels width time-scale]
      :or {levels [0 1]
           width 1.3}}]
  {:pan-env-levels levels
   :pan-env-time-scale time-scale
   :pan-width width
   :ugen/pan '((fn [sig]
                 (o/pan-az (count outs)
                           sig
                           (o/env-gen (o/envelope pan-env-levels
                                                  (let [env-parts (dec (count pan-env-levels))]
                                                    (repeat env-parts (/ 1 env-parts))))
                                      :time-scale pan-env-time-scale)
                           :width pan-width
                           :orientation 0)))})

(defn map-outs
  "Given a sequence of outs, map a signal array to each out."
  [out-offset outs-seq sig]
  (if (and (sequential? outs-seq)
           (sequential? sig))
    (map (fn [out sig]
           (o/out:ar (oc/+ out out-offset) sig))
         outs-seq
         sig)
    (do (timbre/warn "[map-outs] `outs-seq` & `sig` are not both vectors. Resorting to default output method for current synth variation.")
        (o/out outs-seq sig))))

(defn +outs1
  [params]
  (assoc params
         :out-offset 0
         :ugen/outs (plug [:out-offset :outs]
                          '((fn [sig] (map-outs out-offset outs sig))))
         :outs [0 1 2 3]))
#_(+outs {})
(do
  (ns-unmap *ns* 'cristal-liquidizado-2)
  (make-synth-fn
   'cristal-liquidizado-2
   (-> {:buf 0
        :buf-pos 0
        :rate 1
        :amp 0.5
        :pan 0
        :dur 1
        :ugen/env (plug #{:levels :env-durs :dur}
                        '(o/env-gen (o/envelope levels env-durs)
                                    :time-scale dur
                                    :action o/FREE))
        :levels [0 1 1 0]
        :env-durs [0.1 0.6 0.4]}
       +outs1
       (merge (random-panaz)))

   '(-> (o/play-buf 1 buf rate :start-pos buf-pos)
        (* amp :ugen/env)
        :ugen/pan ;; FIXME investigate break
        :ugen/outs)
   {:reset? true})
  #_(cristal-liquidizado-2 {:buf buf
                            :dur 10}))
(comment
  (-> cristal-liquidizado-2)
  (o/stop)
  ((o/synth (#'overtone.core/out 0 (-> (#'overtone.core/play-buf 1 buf)))))
  (o/demo (o/play-buf 1 buf))
  (cristal-liquidizado {:buf buf
                        :dur 10}))
(make-synth-fn
 'amanecer*guitar-clouds-2
  ;; TODO: pass in template envelope
 (-> {:buf 0
      :trig-rate 40
      :grain-dur 1/20
      :rate 1
      :amp 1
      :amp-lfo-min 0.5
      :amp-lfo 0.1
      :start 0.1
      :end 0.3
      :a 0.1
      :d 1
      :d-level 0.3
      :r 3
      :out 0
      :lpf-min 100
      :lpf-max 2000
      :pan 0
      :rev-mix 1
      :rev-room 0.5
      :interp 1
      :a-level 1}
     +outs1
     (merge (random-panaz)))
 '(o/out out
         (-> (o/grain-buf
              :num-channels 1
              :trigger (o/impulse trig-rate)
              :dur grain-dur
              :sndbuf buf
              :rate rate
              :pos  (o/line start end (+ a d r))
              :interp interp
              :pan 0)
             (o/lpf (lfo-kr 0.1 lpf-min lpf-max))
             :ugen/pan
             (o/free-verb rev-mix rev-room)
             (* amp
                #_(lfo amp-lfo amp-lfo-min 1)
                (o/env-gen (o/envelope [0 a-level d-level 0] [a d r]
                                       [-1 -5])
                           :action o/FREE))
             :ugen/outs)))

(comment
  (require '[tieminos.math.bezier :refer [plot curve]]
           '[tieminos.math.utils :refer [linlin normalize]]
           '[clojure.math.combinatorics :as combo])
  (plot (linlin 0 1 (curve 4 [0 0 0 0 1])))
  (plot (linlin 0 1 (curve 5 [0
                              (rrange 0 -0.3)
                              (rrange -1 3)
                              (rrange 0 1.5)
                              1])))
  (def arrow-outs
    (concat (mapcat combo/permutations (combo/combinations [0 1 2 3] 3))
            (mapcat combo/permutations (combo/combinations [0 1 2 3] 2))))

  (cristal-liquidizado-2 (merge {:buf buf
                                 :dur 2
                                 :buf-pos (rand-int (:n-samples buf))
                                 :rate 1
                                 :levels [0 1 0]
                                 :env-durs (normalize [0.1 0.9])
                                 :amp 1/2}
                                (directional-panaz (let [outs (rand-nth arrow-outs)]
                                                     {:outs outs
                                                      :width 2
                                                      :time-scale 1
                                                      :levels #_[0 2]
                                                      (let [curve* (curve 8 [0 0 0 0 2 4 2 4])]
                                                        (linlin (apply min curve*)
                                                                (apply max curve*)
                                                                0
                                                                ;; End in the last channel of the `outs` array. This doesn't correspond to the PanAZ documentation (for pos) but it seems to work
                                                                (* 2 (/ (dec (count outs))
                                                                        (count outs)))
                                                                curve*))
                                                      #_(linlin 0 1 (curve 5 [0
                                                                              (rrange 0 -0.3)
                                                                              0
                                                                              (rrange -1 6)
                                                                              (rrange 0.5 1.5)
                                                                              1]))}))
                                #_{:ugen/pan '((fn [sig]
                                                 (o/pan-az (count outs)
                                                           sig
                                                           (o/dc 1.5)
                                                           #_(o/env-gen (o/envelope pan-env-levels
                                                                                    (let [env-parts (dec (count pan-env-levels))]
                                                                                      (repeat env-parts (/ 1 env-parts))))
                                                                        :time-scale dur)
                                                           :width 2 #_pan-width)))}))
  (cristal-liquidizado-2 {:buf buf
                          :dur 10})
  (cristal-liquidizado-2 (merge {:buf buf
                                 :dur 20
                                 :rate 1/4
                                 :amp 1/2}
                                (lissajous-pan4)))
  (cristal-liquidizado-2 (merge {:buf buf
                                 :dur 20
                                 :rate 1/4
                                 :amp 1/2}
                                (manual-pan4))))

(oe/defsynth test-synth
  [freq 200
   amp 0.5
   out 0]
  (o/out out (-> (o/sin-osc freq)
                 (o/pan2)
                 (* amp (o/env-gen (o/env-perc) :action o/FREE)))))

(defn- get-panner
  [{:keys [active-panner panner-config]}]
  (let [{:keys [vel xy radius vel direction pos out]} panner-config]
    (case active-panner
      :random (random-panaz {:vel vel :out-offset out})
      :manual (manual-pan4 {:x (first xy) :y (second xy) :out-offset out})
      (timbre/warn "No panner selected, will use default."))))

(defn- get-filter
  [{:keys [active-filter filter-config]}]
  (let [{:keys [lpf hpf reso q]} filter-config]
    (timbre/warn "TODO: implement filters")
    (case active-filter
      :lpf :TODO/lpf
      :moog-ladder :TODO/moog-ladder
      (timbre/warn "No filter selected, will use default."))))

(defn play-synth
  "Plays a synth. The `:synth` key should be a keyword."
  [{:as data
    :keys [synth params]}]

  (try
    (let [panner (get-panner data)
          filter (get-filter data)
          params* (merge params panner #_filter)
          buf (:buf params)
          _ (def params* (assoc params* :buf buf :dur 10))
          synth* (case synth
                   :crystal (let [instance (cristal-liquidizado-2 (assoc params* :buf buf :dur 10))]
                              (bardo.synth-management/add-synth! instance (:dur params))
                              instance)
                   :granular (amanecer*guitar-clouds-2 params))]
      #_(timbre/info (assoc params* :buf buf :dur 10))
      (timbre/debug "[play-synth]\n" data)
      (timbre/debug "[play-synth]\n" (keys data))
      (timbre/debug "[play-synth] panner" panner)
      (timbre/debug "[play-synth] filter" filter)

      (when buf
        (swap! bardo.rec/currently-playing-bufs update buf conj synth*)))
    (catch Exception e (timbre/error e))))

(comment
  (-> params*)
  (o/defsynth sini
    [freq 200
     amp 0.5
     out 0]
    (o/out out (* amp (o/pan2 (o/sin-osc 200)))))
  (println sini)
  (def test-sini (sini (:group params*) :freq 400))
  (o/kill test-sini)
  (:group params*)
  (keys params*)
  (cristal-liquidizado-2 {:buf buf :dur 10 :rate 1/8})
  (cristal-liquidizado-2 (-> params*

                             (dissoc

                              :rev-room))))
