(ns tieminos.compositions.garden-earth.moments.two.synths
  (:require
   [clojure.data.generators :refer [weighted]]
   [erv.scale.core :as scale]
   [overtone.core :as o]
   [overtone.sc.ugen-collide-list :as oc]
   [tieminos.compositions.garden-earth.moments.two.harmonies :as two.harmonies]
   [tieminos.compositions.garden-earth.routing :as ge.route :refer [fl-i1]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [ctl-range lfo-kr]]
   [tieminos.utils :refer [rrange]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(oe/defsynth simple-playbuf
  [buf 0
   rate 1
   amp 0.5
   delay 0
   pan 0
   a 0.2
   s 0.6
   r 0.2
   amp-ctl 1000
   amp-ctl-min 1
   amp-ctl-max 1
   rev-mix 0
   rev-damp 0.5
   rev-room 1
   out 0]
  (let [dur (/ (o/buf-dur buf) rate)]
    (o/out out
           (-> (o/play-buf 1 buf rate)
               (o/delay-l delay delay)
               ;; TODO agregar control de reverb via OSC
               (o/free-verb rev-mix rev-room rev-damp)
               (* amp
                  (ctl-range amp-ctl amp-ctl-min amp-ctl-max)
                  (o/env-gen
                   (o/envelope
                    [0 0 1 1 0]
                    [delay
                     (* a dur)
                     (* s dur)
                     (* r dur)])
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
             a-level-ctl-lag
             fade-time]
      :or {id ::magma-lava
           in-num-channels 1
           amp 1/2
           lpf 800
           a-level-ctl 1
           a-level-ctl-min 1
           a-level-ctl-max 1
           a-level-ctl-lag 0.5
           fade-time 7}}]
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
      (o/sine-shaper (o/lag2 (ctl-range a-level-ctl
                                        a-level-ctl-min
                                        a-level-ctl-max)
                             a-level-ctl-lag))
      (* amp)
      (#(o/compander % % (o/db->amp -8) :slope-above 1/3))
      (o/free-verb 0.5 2)
      (o/pan2 (lfo-kr 0.5 -0.5 0.5)))
     {:fade-time fade-time
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
   pan-min -1
   pan-max 1
   lpf 20000
   out 0]
  (let [a (* dur a%)
        r (* dur r%)
        s (- dur a r)
        sig
        (-> (o/in in 1)
            (o/delay-l delay delay)
            (o/pitch-shift 0.3 ratio)
            (o/pan2 (lfo-kr pan-freq pan-min pan-max))
            (o/lpf lpf)
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

(defn ndef-clean-out
  [{:keys [id group in pan out fade-time]
    :or {id ::ndef-clean-out
         group (groups/mid)
         in 0
         pan 0
         out 0
         fade-time 0.5}}]
  (ndef/ndef
   id
   (o/pan2 (o/in in 1) pan)
   {:group group
    :fade-time fade-time
    :out out}))

(oe/defsynth bi-out
  [in 0
   r 3
   gate 1
   out-1 0
   out-2 2]
  (let [sig (-> (o/in in 2)
                (* (o/env-gen (o/adsr 3 1 1 r)
                              :gate gate
                              :action o/FREE)))]
    (o/out out-1 sig)
    (o/out out-2 sig)))

(comment
  (ndef-clean-out
   {:in golpe-bus})
  (ndef/stop)
  (o/stop)
  (bi-out {:group (groups/mid)
           :in golpe-bus
           :out-1 0
           :out-2 1})
  (golpe {:group (groups/early)
          :freq (rrange 30 50)
          :out golpe-bus}))

(defn ndef-interior-del-la-tierra
  [{:keys [id in out fade-time group]
    :or {id ::mtvs-al-interior-de-la-tierra
         out 0
         fade-time 0.5
         group (groups/mid)}}]
  (ndef/ndef
   id
   (let [sig* (o/in in 1)
         sig (-> (o/in in 1)
                 (o/pitch-shift (lfo-kr 1 0.1 0.7) 1/2))
         echoes (->> 5
                     range
                     (map (fn [_]
                            (-> sig
                                #_(o/comb-l 1 (lfo-kr 0.1 0.1 1) 1)
                                (* 2  (o/clip:kr (lfo-kr 4 -1 1) 0 0.5))
                                ((fn [sig]
                                   (+ sig
                                      (* 2 (o/lpf sig
                                                  (lfo-kr 1 80 90))))))))))]
     (+ (* 1/2 sig)
        (* 2 (o/pan2 sig (lfo-kr 1 -1 1)))
        (-> echoes
            (o/mix)
            (o/pan2 (lfo-kr 1 -1 1))
            (o/free-verb 1
                         (o/clip (lfo-kr 1 -2 2) 0 1)
                         (lfo-kr 4 0 1)))
        (-> echoes
            (->> (map (fn [sig] (-> sig
                                    (* 4)
                                    (o/sine-shaper)
                                    (o/distortion2 0.2)
                                    (o/lpf 400)
                                    (* 1/2)
                                    (* (lfo-kr 10 0 2))))))
            (o/mix)
            (o/free-verb 1
                         (o/clip (lfo-kr 1 -2 2) 0 1)
                         (lfo-kr 4 0 1)))))
   {:group group
    :out out
    :fade-time fade-time}))

(defn ndef-erupcion-rev-bank
  [{:keys [id in out fade-time group]
    :or {id ::mtvs-al-interior-de-la-tierra.rev-bank
         out 0
         fade-time 0.5
         group (groups/fx)}}]
  (ndef/ndef
   id
   (let [sig (o/in in 1)
         rev-bank (map (fn [i]
                         (let [distor (fn [sig]
                                        (if (> 0.5 (rand (/ i 10)))
                                          sig
                                          (-> sig
                                              (o/disintegrator 0.5 -1)
                                              (* 5))))]
                           (-> sig
                               (o/pan2 (lfo-kr (o/rand 0.5 2) -1 1))
                               distor
                               (o/free-verb 1 (lfo-kr 2 0.5 i) (lfo-kr 2 0.5 1))
                               #_(* (lfo-kr (o/rand 2 5) 0 1.5))
                               (o/compander sig 0.05 :slope-above 1/24)
                               (* (lfo-kr (o/rand 0.1 1) 0 2/3)))))
                       (range 10))]
     (o/mix rev-bank))
   {:group group
    :out out
    :fade-time fade-time}))

(comment
  (ndef-erupcion-rev-bank
   {:in golpe-bus})
  (golpe {:group (groups/early)
          :freq (rrange 30 50)
          :out golpe-bus})
  (ndef/stop))

(comment

  (def golpe-bus (o/audio-bus 1 "golpe-bus"))

  (oe/defsynth golpe
    [freq 40
     out 0]
    (o/out out
           (-> (o/saw (* freq [1 2 3]))
               o/mix
               (* 1 (o/env-gen (o/env-perc)
                               :action o/FREE)))))

  (ndef-interior-del-la-tierra
   {:in golpe-bus})
  (ndef-clean-out
   {:in golpe-bus})

  (ndef-erupcion-rev-bank
   {:in golpe-bus})
  (golpe {:group (groups/early)
          :freq (rrange 30 50)
          :out golpe-bus})
  (ndef/stop)
  (do
    (ndef/ndef
     ::mtvs-al-interior-de-la-tierra
     (let [sig (o/in golpe-bus 1)
           echoes (->> 5
                       range
                       (map (fn [_]
                              (-> sig
                                  #_(o/delay-n 1 (lfo-kr 0.1 0 1))
                                  (o/comb-l 1 (o/rand 0.1 0.5) #_(lfo-kr 0.1 0.1 1)
                                            1)
                                  (* 2  (o/clip:kr (lfo-kr 4 -1 1) 0 0.5))
                                  ((fn [sig]
                                     (+ sig
                                        (* 2 (o/bpf sig
                                                    (lfo-kr 1 80 90)
                                                    (lfo-kr 0.5 0.2 0.5))))))))))]
       (+ (o/pan2 sig (lfo-kr 1 -1 1))
          (-> echoes
              (o/mix)
              (o/pan2 (lfo-kr 1 -1 1))
              (o/free-verb 1
                           (o/clip (lfo-kr 1 0 1) 0 1)
                           (lfo-kr 4 0 1)))
          (-> echoes
              (->> (map (fn [sig] (-> sig
                                      (* 4)
                                      (o/distortion2 0.2)
                                      (o/lpf 400)
                                      (* 1/2)
                                      (* (lfo-kr 10 0 2))))))
              (o/mix)
              (o/free-verb 1
                           (o/clip (lfo-kr 1 -1 1) 0 1)
                           (lfo-kr 4 0 1)))))
     {:group (groups/mid)}))
  (golpe {:group (groups/early)
          :freq (rrange 30 50)
          :out golpe-bus})
  (ref-rain
   :id :test
   :durs [1/2]
   :on-event (on-event
              (golpe {:group (groups/early)
                      :freq (rrange 30 50)
                      :out golpe-bus})))
  (gp/stop :test))

;; FIXME probably not going to work
(oe/defsynth meru-filter
  ;; For use in totalidad with a meru scale
  ;; NOTE input must be stereo
  [in 0
   freq 200
   gate 1
   amp 0.1
   out 0]
  (o/out out
         (-> (o/in in 2)
             #_(#(o/grain-in 2 (o/impulse:kr 80) 0.1 %))
             (o/b-moog freq 0.5 2)
             #_(o/comb-l 0.1 0.1 0.1)
             (* 0.1 (o/env-gen (o/adsr 3 1 1 3)
                               :gate gate
                               :action o/FREE)
                (lfo-kr 1 1 2))
             #_(o/pan2 (lfo-kr 1 -1 1))
             #_(o/free-verb 1 2)
             #_(o/sine-shaper 0.5))))
(comment

  (def prueba-fondo-oceanico (o/load-sample "/Users/diego/Music/diego/garden-earth/two_erupciones_for-guitar/renders/prueba-fondo-oceanico.wav"))
  (def test-bus (o/audio-bus 2 "test-bus"))

  (require '[tieminos.compositions.garden-earth.moments.two.harmonies :refer [meta-slendro2]])

  (ndef/ndef :test-sample
             (o/play-buf 2 prueba-fondo-oceanico)
             {:group (groups/early)
              :out test-bus})

  (ndef/ndef :out
             (* 1 (o/in test-bus 2)))

  (do
    (try (doseq [f filters] (o/ctl f :gate 0)) (catch Exception _ nil))
    (def filters (mapv (fn [i]
                         (meru-filter {:group (groups/fx)
                                       :in test-bus
                                       :freq (* 800
                                                (scale/deg->freq meta-slendro2 1
                                                                 (* 2 i)))}))
                       (range 4))))
  (doseq [f filters] (o/ctl f :amp 1))
  (doseq [[i f] (map-indexed vector filters)]
    (o/ctl f :freq (* 200
                      (scale/deg->freq meta-slendro2 1
                                       (* 2 i)))))

  (o/stop))

(defn fade-rev
  "A reverb controlled by an exp/btn.
  ins must be an array of buses"
  [{:keys [id
           ins
           out
           room
           damp
           amp-boost-bus
           amp-boost-min
           amp-boost-max
           amp-boost-lag
           fade-time]
    :or {id :fade-rev
         room 1
         damp 0.5
         amp-boost-bus 1000
         amp-boost-min 0
         amp-boost-max 1
         amp-boost-lag 0.5
         fade-time 2}}]
  (ndef/ndef
   id
   (-> (apply oc/+
              (map (fn [in] (let [n-chans (:n-channels in)]
                              (cond-> (o/in in n-chans)
                                (= 1 n-chans) (o/pan2 (lfo-kr 0.5 -0.5 0.5)))))
                   ins))
       (o/free-verb 1 room damp)
       (* (o/lag2 (ctl-range
                   amp-boost-bus
                   amp-boost-min
                   amp-boost-max)
                  amp-boost-lag)))
   {:group (groups/fx)
    :out out}))

(comment
  (fade-rev
   {:id :test
    :ins [(ge.route/fl-i1 :bus)]
    :out (ge.route/out :ndef-1)
    :amp-boost-bus (ge.route/ctl-bus :exp/btn-1)
    :amp-boost-min 0
    :amp-boost-max 1
    :amp-boost-lag 5})
  (ge.route/set-ctl :exp/btn-1 127)
  (ge.route/set-ctl :exp/btn-1 0))
