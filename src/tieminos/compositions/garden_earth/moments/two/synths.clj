(ns tieminos.compositions.garden-earth.moments.two.synths
  (:require
   [overtone.core :as o]
   [tieminos.compositions.garden-earth.routing :refer [fl-i1]]
   [tieminos.compositions.garden-earth.routing :as ge.route]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.synths.v1 :refer [ctl-range lfo-kr]]))

(oe/defsynth simple-playbuf
  [buf 0
   rate 1
   amp 0.5
   pan 0
   amp-ctl 1000
   amp-ctl-min 1
   amp-ctl-max 1
   out 0]
  (let [dur (/ (o/buf-dur buf) rate)]
    (o/out out
           (-> (o/play-buf 1 buf rate)
               ;; TODO agregar control de reverb via OSC
               #_(o/free-verb (lfo-kr 4 0 1))
               (* amp
                  (ctl-range amp-ctl amp-ctl-min amp-ctl-max)
                  (o/env-gen
                    (o/envelope
                      [0 1 1 0]
                      [(* 0.2 dur)
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
