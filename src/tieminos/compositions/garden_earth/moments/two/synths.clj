(ns tieminos.compositions.garden-earth.moments.two.synths
  (:require
   [clojure.data.generators :refer [weighted]]
   [overtone.core :as o]
   [tieminos.compositions.garden-earth.moments.two.harmonies :as two.harmonies]
   [tieminos.compositions.garden-earth.routing :as ge.route :refer [fl-i1]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [ctl-range lfo-kr]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :refer [on-event ref-rain]]))

(oe/defsynth simple-playbuf
  [buf 0
   rate 1
   amp 0.5
   delay 0
   pan 0
   amp-ctl 1000
   amp-ctl-min 1
   amp-ctl-max 1
   out 0]
  (let [dur (/ (o/buf-dur buf) rate)]
    (o/out out
           (-> (o/play-buf 1 buf rate)
               (o/delay-l delay delay)
               ;; TODO agregar control de reverb via OSC
               #_(o/free-verb (lfo-kr 4 0 1))
               (* amp
                  (ctl-range amp-ctl amp-ctl-min amp-ctl-max)
                  (o/env-gen
                    (o/envelope
                      [0 0 1 1 0]
                      [delay
                       (* 0.2 dur)
                       (* 0.6 dur)
                       (* 0.2 dur)])
                    :action o/FREE))
               (#(o/pan-az:ar 2 % pan))))))

(oe/defsynth buf-mvts-subterraneos
  [buf 0
   rate 1
   amp 0.5
   pan 0
   max-rev-mix 0 ;; NOTE no reverb by default
   rev-room 1
   amp-ctl 1000
   amp-ctl-min 1
   amp-ctl-max 1
   out 0]
  (let [dur (/ (o/buf-dur buf) rate)]
    (o/out out
           (-> (o/play-buf 1 buf rate)
               (o/lpf 3000)
               (o/free-verb (lfo-kr 4 0 max-rev-mix)
                            rev-room)
               (o/delay-l 0.01 [0 0.001])
               (* amp
                  (o/env-gen
                   (o/envelope
                    [0 1 0.7 0]
                    [(* 0.01 dur)
                     (* 0.79 dur)
                     (* 0.2 dur)])
                   :action o/FREE))
               ((fn [sig]
                  (+ (* sig (lfo-kr 3 0.5 1))
                     (-> sig
                         (o/bpf (lfo-kr 3 40 80)
                                (lfo-kr 3 0.1 0.5))
                         (* 2 (lfo-kr 3 0.5 1))
                         (o/distort)))))
               (* (ctl-range amp-ctl amp-ctl-min amp-ctl-max))
               (o/mix)
               (o/pan2 pan)))))

(defn ndef-mvts-subterraneos
  [{:keys [id out
           lpf rates
           amp-boost-ctl-bus amp-boost-min amp-boost-max amp-boost-lag]
    :or {lpf 8000
         amp-boost-lag 0.5
         rates [1/2 1/4 11/8 2]
         amp-boost-min 0
         amp-boost-max 1}}]
  (ndef/ndef
   id
   (-> (o/in (fl-i1 :bus))
       (o/lpf lpf) ;; FIXME high pitched noise due to guitar amp line out
       ((fn [sig]
          (o/mix (map (fn [ratio]
                        (-> sig
                            (o/pitch-shift 0.1 ratio)
                            (o/pan2 (lfo-kr 0.5 -1 1))
                            (* (lfo-kr 1 0 1))))

                      rates))))
       (o/free-verb 0.7 2)

       (* (o/db->amp 20) ;; TODO perhaps this should be a control bus
          (if-not amp-boost-ctl-bus
            1
            (o/lag (ctl-range amp-boost-ctl-bus
                              amp-boost-min
                              amp-boost-max)
                   amp-boost-lag))
          (lfo-kr 0.5 0.5 1)))
   {:out out
    :fade-time 7
    :group (groups/mid)}))

(comment
  (o/stop)
  (tieminos.compositions.garden-earth.init/init!
   {:inputs-config {:in-1 {:amp (o/db->amp 8)}}})
  (ndef-mvts-subterraneos
   {:id :test
    :out (ge.route/out :ndef-1)
    :amp-boost-ctl-bus (ge.route/ctl-bus :exp/pedal-1)})
  (ndef/stop))

(oe/defsynth magma
  [buf 0
   rate 1
   dur 1
   a-level 3
   d-level 0.1
   amp 0.5
   start 0
   end 1
   pan 0
   out 0]
  (o/out
   out
   (->
    (o/grain-buf
     :num-channels 1
     :trigger (o/impulse 40)
     :dur dur
     :sndbuf buf
     :rate rate
     :pos (/ (o/phasor:ar
              0
              (* (/ 1 dur) (o/buf-rate-scale:kr buf))
              (* start (- (o/buf-samples:kr buf) 1))
              (* end (- (o/buf-samples:kr buf) 1)))
             (o/buf-samples:kr buf))
     :interp 2
     :pan pan)
    (o/lpf 800) ;; FIXME high pitched noise due to guitar amp line out
    (* 4 (o/env-gen
          (o/envelope
               ;; TODO ADSR
           [0 a-level d-level 0]
           [(* 0.01 dur)
            (* 0.79 dur)
            (* 0.2 dur)])
          :action o/FREE))
      ;; NOTE rangos (asumiendo `a-level` 3): -- pero puede llegar hasta el 10 sin problemas
      ;; 0.05 - burbujeos
      ;; 0.3 - magma viva
      ;; 0.5 - ya bastante suave, y brilloso
      ;; 1 - magma brillosa
      ;; Aumentar el a-level aumenta el burbujeo
    (o/sine-shaper 0.5)
    (* 1/2 amp)
    (o/pan2 pan))))

(do
  (defn ndef-magma-lava
    [{:keys [id
             in
             in-num-channels
             out
             group
             amp
             lpf
             a-level-ctl
             a-level-ctl-min
             a-level-ctl-max
             a-level-ctl-lag]
      :or {id ::magma-lava
           in-num-channels 1
           amp 1/2
           lpf 800
           a-level-ctl 1
           a-level-ctl-min 1
           a-level-ctl-max 1
           a-level-ctl-lag 0.5}}]
    (println {:in in :in-num-channels in-num-channels :out out :amp amp :lpf lpf :a-level-ctl a-level-ctl :a-level-ctl-min a-level-ctl-min :a-level-ctl-max a-level-ctl-max :a-level-ctl-lag a-level-ctl-lag})
    (ndef/ndef
     id
     (->
      (o/in in in-num-channels)
      (o/lpf lpf)
      (o/pitch-shift 0.2 [1/2 2/3 1 3/2 11/4 13/4])
      (#(o/compander % % 0.5 1 2))
      (* 40)
          ;; NOTE rangos (asumiendo `a-level` 3): -- pero puede llegar hasta el 10 sin problemas
          ;; 0.05 - burbujeos
          ;; 0.3 - magma viva
          ;; 0.5 - ya bastante suave, y brilloso
          ;; 1 - magma brillosa
          ;; Aumentar el a-level aumenta el burbujeo
      (o/sine-shaper (o/lag (ctl-range a-level-ctl
                                       a-level-ctl-min
                                       a-level-ctl-max)
                            a-level-ctl-lag))
      (* amp)
      (o/free-verb 0.5 2)

      (o/pan2 (lfo-kr 0.5 -0.5 0.5)))
     {:fade-time 7
      :out out
      :group group}))

  (comment
    (o/stop)
    (ndef-magma-lava
     {:group (groups/mid)
      :in (fl-i1 :bus)
      :lpf 20000
      :a-level-ctl (ge.route/ctl-bus :exp/pedal-1)
      :a-level-ctl-min 0.3
      :a-level-ctl-max 1
      :a-level-ctl-lag 0.1
      :amp 1
      :out (ge.route/out :magma-ndef)})))

(oe/defsynth delayed-ps
  [in 0
   delay 0
   dur 2
   a% 0.2
   r% 0.2
   pan-freq 1
   ratio 1
   amp 1
   amp-boost-ctl-bus 1000
   amp-boost-min 1
   amp-boost-max 1
   amp-boost-lag 1
   out 0]
  (let [a (* dur a%)
        r (* dur r%)
        s (- dur a r)
        sig
        (-> (o/in in 1)
            #_(o/lpf 4000)
            (o/delay-l delay delay)
            (o/pitch-shift 0.3 ratio)
            (o/pan2 (lfo-kr pan-freq -1 1))
            (* 2  amp
               (lfo-kr 0.3 0.5 1)
               (o/lag (ctl-range
                        amp-boost-ctl-bus
                        amp-boost-min
                        amp-boost-max)
                      amp-boost-lag)
               (o/env-gen (o/envelope
                            [0 0 1 1 0]
                            [delay a s r])
                          :action o/FREE)))]
    (o/out out sig)))

(comment
  (ref-rain
   :id ::estratos
   :durs (fn [_] (rrange 0.5 2))
   :on-event (on-event
              (delayed-ps
               {:group (groups/mid)
                :in (fl-i1 :bus)
                :delay (rrange 0.1 10)
                :dur (rrange 5 30)
                :a (rrange 0.2 0.5)
                :ratio (* (-> two.harmonies/meta-pelog
                              #_(nth (rand-nth  [1 3 7 9]))
                              rand-nth
                              :bounded-ratio)
                          (weighted {1 1
                                     1/2 3
                                     1/4 2
                                     2 2}))
                :amp 1
                :amp-boost-ctl-bus (ge.route/ctl-bus (rand-nth [:exp/btn-2 :exp/btn-a]))
                :amp-boost-min 1
                :amp-boost-max 4
                :amp-boost-lag (rrange 14 18)
                :out (ge.route/out :estratos-rain)})))

  (defn test-play-synth []
    (println "playing synths")
    (doseq [_ (range 1 (+ 1 (rand-int 3)))]
      (delayed-ps
       {:group (groups/mid)
        :in (fl-i1 :bus)
        :delay (rrange 0.1 2)
        :dur (rrange 5 15)
        :a (rrange 0.1 0.5)
        :ratio (* (->> two.harmonies/fib
                       rand-nth
                       :bounded-ratio)
                  (weighted {1 3
                             1/2 2
                             2 2}))
        :amp 2
        :out (ge.route/out :estratos-rain)}))))
