(ns tieminos.habitat.extended-sections.tunel-cuantico-bardo.synths
  (:require
   [clojure.core.async :as a]
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.rec :as bardo.rec]
   [tieminos.habitat.extended-sections.tunel-cuantico-bardo.synth-management :as bardo.synth-management]
   [tieminos.math.bezier :as bz]
   [tieminos.math.utils :refer [linlin]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.synths.template-synth.v0 :refer [defplug make-synth-fn
                                                       plug*]]
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
  (-> buf :duration)
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
  (plug* #{:outs :pan-vel :pan-width}
         '((fn [sig] (o/pan-az:ar (count outs) sig
                                  (lfo-kr pan-vel -1 1) ;; LFNoise1
                                  :width pan-width)))))
(defn random-panaz
  [& {:keys [vel width]
      :or {vel 0.5, width 1.4}}]
  {:pan-vel vel
   :pan-width width ;; TODO: control lfo width
   :ugen/pan random-panaz-plug})

(defplug random-panaz
  #{:outs}
  {:pan-vel 0.5
   :pan-width 1.4 ;; TODO: control lfo width
   :ugen/pan '((fn [sig] (o/pan-az:ar (count outs) sig
                                      (lfo-kr pan-vel -1 1) ;; LFNoise1
                                      :width pan-width)))})

(macroexpand-1
 '(defplug random-panaz
    #{:outs}
    {:pan-vel 0.5
     :pan-width 1.4 ;; TODO: control lfo width
     :ugen/pan '((fn [sig] (o/pan-az:ar (count outs) sig
                                        (lfo-kr pan-vel -1 1) ;; LFNoise1
                                        :width pan-width)))}))

lfo-kr
#_(defn lissajous-pan4
    [& {:keys [vel radius ratio phase]
        :or {vel 1
             radius 1
             ratio 1
             phase Math/PI}}]
    {:liss-freq vel
     :liss-radius radius
     :liss-ratio ratio
     :liss-phase phase
     :ugen/pan (plug* #{:liss-freq :liss-radius :liss-ratio :liss-phase}
                      '((fn [sig]
                          (o/pan4 sig
                                  (* liss-radius (o/sin-osc:kr liss-freq 0))
                                  (* liss-radius (o/sin-osc:kr (* liss-freq liss-ratio) liss-phase))))))})
(defplug lissajous-pan4
  {:liss-freq 1
   :liss-radius 1
   :liss-ratio 1
   :liss-phase Math/PI
   :ugen/pan '((fn [sig]
                 (o/pan4 sig
                         (* liss-radius (o/sin-osc:kr liss-freq 0))
                         (* liss-radius (o/sin-osc:kr (* liss-freq liss-ratio) liss-phase)))))})
(defplug manual-pan4
  {:pan-x 0
   :pan-y 0
   :ugen/pan '((fn [sig] (o/pan4 sig pan-x pan-y)))})

#_(defn directional-panaz
    [& {:keys [levels width time-scale]
        :or {levels [0 1]
             width 1.3}}]
    {:pan-env-levels levels
     :pan-env-time-scale time-scale
     :pan-width width
     :ugen/pan (plug* #{:pan-env-levels :pan-env-time-scale :pan-width}
                      '((fn [sig]
                          (o/pan-az (count outs)
                                    sig
                                    (o/env-gen (o/envelope pan-env-levels
                                                           (let [env-parts (dec (count pan-env-levels))]
                                                             (repeat env-parts (/ 1 env-parts))))
                                               :time-scale pan-env-time-scale)
                                    :width pan-width
                                    :orientation 0))))})
(defplug directional-panaz
  #{:outs}
  {:pan-env-levels [0 1]
   :pan-env-time-scale 1
   :pan-orientation 0
   :pan-width 1.3
   :ugen/pan
   '((fn [sig]
       (o/pan-az (count outs)
                 sig
                 (o/env-gen (o/envelope pan-env-levels
                                        (let [env-parts (dec (count pan-env-levels))]
                                          (repeat env-parts (/ 1 env-parts))))
                            :time-scale pan-env-time-scale)
                 :width pan-width
                 :orientation pan-orientation)))})

(defplug lpf
  {:lpf 20000
   :reso 0.1
   :ugen/filter '((fn [sig] (o/rlpf sig (o/clip lpf 30 20000) reso)))})

(defplug hpf
  {:hpf 30
   :reso 0.1
   :ugen/filter '((fn [sig] (o/rhpf sig (o/clip hpf 30 20000) reso)))})

(defplug moog-ladder
  {:lpf 20000
   :reso 0.1
   :ugen/filter '((fn [sig] (o/moog-ladder sig (o/clip lpf 30 20000) reso)))})

(defplug moog-ladhp
  {:lpf 20000
   :hpf 40
   :reso 0.5
   :q 0.5
   :ugen/filter '((fn [sig] (-> sig
                                (o/moog-ladder (o/clip lpf 30 20000) reso)
                                (o/b-moog (o/clip hpf 60 20000) q 1))))})
(defplug moog-hplad
  {:lpf 20000
   :hpf 40
   :reso 0.5
   :q 0.5
   :ugen/filter '((fn [sig] (-> sig
                                (o/moog-ladder (o/clip lpf 30 20000) reso)
                                (o/b-moog (o/clip hpf 60 20000) q 1))))})
(defplug moog-bp
  {:lpf 400
   :q 0.5
   :ugen/filter '((fn [sig] (o/b-moog sig (o/clip lpf 60 20000) q 2)))})

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
         :ugen/outs (plug* [:out-offset :outs]
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
        :ugen/env (plug* #{:levels :env-durs :dur}
                         '(o/env-gen (o/envelope levels env-durs)
                                     :time-scale dur
                                     :action o/FREE))
        :levels [0 1 1 0]
        :env-durs [0.1 0.6 0.4]}
       +outs1
       (random-panaz))

   '(-> (o/play-buf 1 buf rate :start-pos buf-pos)
        :ugen/filter
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
      :amp-env-levels [0 1 0.3 0]
      :amp-env-durations [0.1 1 3]
      :out 0
      :lpf-min 100
      :lpf-max 2000
      :pan 0
      :rev-mix 1
      :rev-room 0.5
      :interp 1
      :a-level 1}
     +outs1
     (random-panaz))
 '(o/out out
         (-> (o/grain-buf
              :num-channels 1
              :trigger (o/impulse trig-rate)
              :dur grain-dur
              :sndbuf buf
              :rate rate
              :pos  (o/line start end (apply + amp-env-durations))
              :interp interp
              :pan 0)
             :ugen/filter
             :ugen/pan
             (o/free-verb rev-mix rev-room)
             (* amp
                #_(lfo amp-lfo amp-lfo-min 1)
                (o/env-gen (o/envelope amp-env-levels
                                       amp-env-durations
                                       [-1 -5])
                           :action o/FREE))
             :ugen/outs))
 {:reset? true})

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

(defn- add-panner
  [params {:keys [active-panner panner-config]}]
  (let [{:keys [vel x y xy radius vel direction pos range]} panner-config]
    (timbre/spy :debug :panner-config [panner-config params])
    (case active-panner
      :random (random-panaz params {:pan-vel vel})
      :manual (manual-pan4 params {:pan-x (-> (first xy) (* 2) (+ -1))
                                   :pan-y (-> (second xy) (* 2) (+ -1))})
      :lissajous (do
                   (timbre/warn "TODO: lissajous-pan4 still needs work")
                   (lissajous-pan4 params
                                   {:liss-freq vel
                                    :liss-radius radius
                                    :liss-ratio (max 0.1 (/ (* 11 x)
                                                            (* 11 y)))
                                    :liss-phase Math/PI}))
      :arrows (do
                (timbre/warn "TODO: directional-panaz (arrows) panner still needs work")
                (directional-panaz params
                                   (timbre/spy
                                    :debug "ARROWS"
                                    {:panner-env-time-scale vel
                                     :pan-env-levels (let [curve* (bz/curve 8 [0 (rrange -3 3)
                                                                               (rrange -3 3)
                                                                               (rrange -3 3)
                                                                               2 4 2 4])]
                                                       (linlin (apply min curve*)
                                                               (apply max curve*)
                                                               0
                                                                ;; End in the last channel of the `outs` array. This doesn't correspond to the PanAZ documentation (for pos) but it seems to work
                                                               (* 2 (/ (dec 4) 4))
                                                               curve*))
                                     :pan-orientation (* 2 pos)
                                     :panner-width range})))
      (do (timbre/warn (format "No panner %s selected, will use default."
                               active-panner))
          params))))

(defn- add-filter
  [params {:keys [active-filter filter-config]}]
  (let [{:keys [_lpf _hpf _reso _q]} filter-config]
    (case active-filter
      :lpf (lpf params (timbre/spy :debug "lpf" filter-config))
      :hpf (hpf params (timbre/spy :debug "hpf" filter-config))
      :moog-ladder (moog-ladder params (timbre/spy :debug "moog-ladder" filter-config))
      :moog-ladhp (moog-ladhp params (timbre/spy :debug "moog-ladhp" filter-config))
      :moog-hplad (moog-hplad params (timbre/spy :debug "moog-hplad" filter-config))
      :moog-bp (moog-bp params (timbre/spy :debug "moog-bp" filter-config))
      params)))

(defn play-synth
  "Plays a synth. The `:synth` key should be a keyword."
  [{:as data
    :keys [synth params]}]
  #_(println "===============")
  (a/go
    (try
      (let [;; filter (get-filter data)
            params* (-> params
                        (add-panner data)
                        (add-filter data))
            _ (def params* params*)
            buf (:buf params)
            synth* (case synth
                     :crystal (let [instance #_(cristal-liquidizado (assoc params :out (:out-offset params)))
                                    (cristal-liquidizado-2 params*)]
                                (bardo.synth-management/add-synth! instance (:dur params))
                                instance)
                     :granular (amanecer*guitar-clouds-2 params*))]
        #_(timbre/info (assoc params* :buf buf :dur 10))
        (timbre/debug "[play-synth]\n" data)
        (timbre/debug "[play-synth]\n" (keys data))
        #_(timbre/debug "[play-synth] panner" panner)
        (timbre/debug "[play-synth] filter" filter)

        (when buf
          (swap! bardo.rec/currently-playing-bufs update buf conj synth*)))
      (catch Exception e (timbre/error e)))))

(comment
  (require '[tieminos.habitat.extended-sections.tunel-cuantico-bardo.live-state :as bardo.live-state])
  (-> @bardo.live-state/live-state)
  (-> params*)
  (o/defsynth sini
    [freq 200
     amp 0.5
     out 0]
    (o/out out (* amp (o/pan2 (o/sin-osc 200)))))
  (println sini)
  #_(def params* (assoc params* :buf buf :dur 10))

  (oe/defsynth sini
    [buf 0
     amp 0.5
     out 0]
    (o/out out (* amp (o/pan2 (o/play-buf 1 buf)))))

  (def test-sini (sini :buf (:buf params*)))
  (o/kill test-sini)
  (def test-sini (sini (:group params*) :freq 400))
  (o/kill test-sini)
  (:group params*)
  (keys params*)
  (cristal-liquidizado-2 {:buf (:buf params*)})
  (cristal-liquidizado-2 (-> params*

                             (dissoc

                              :rev-room))))
