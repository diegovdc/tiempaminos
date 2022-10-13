(ns tieminos.habitat.quad-inputs
  (:require
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.habitat.osc :as habitat-osc]
   [tieminos.overtone-extensions :as oe]
   [tieminos.sc-utils.groups.v1 :as groups]))

(o/defsynth rev [in 0 out 0 mix 0.5 room 0.8 damp 0.5 amp 1]
  (o/out out (* amp (o/free-verb (o/in in) mix room damp))))

(o/defsynth rand-pan4
  [in 0
   out 0
   lfo-freq-x 0.3
   lfo-freq-y 0.3
   release 2
   gate 1]
  (o/out out
         (-> (o/pan4:ar
              (o/in in)
              (o/lf-noise1 lfo-freq-x)
              (o/lf-noise1 lfo-freq-y))
             (* (o/env-gen (o/env-adsr 2 1 1 release :curve -0.5)
                           gate
                           :action o/FREE)))))

(o/defsynth circle-pan-4ch
  ;; For circular motion use `direction` 1 or -1 only
  [in 0
   out 0
   rate 0.2
   release 2
   direction 1
   gate 1]
  (o/out out
         (->
          (o/pan4
           (o/in in)
           (o/sin-osc rate 0)
           (o/sin-osc rate (* direction (/ Math/PI 2))))
          (* (o/env-gen (o/env-adsr 2 1 1 release :curve -0.5)
                        gate
                        :action o/FREE)))))

(oe/defsynth sini
  [freq 200 amp 0.5 out 0]
  (o/out out (* amp (o/sin-osc freq))))

(oe/defsynth sini-2
  [amp 0.5 out 0]
  (o/out out (* (o/sin-osc 200)
                (o/env-gen (o/env-perc) :action o/FREE)
                amp)))

(oe/defsynth whitey
  [amp 0.2 out 0]
  (o/out out (* amp (o/white-noise))))

(defn args->map [args]
  (try
    (->> args (partition 2 2)
         (map (fn [[k v]]
                {(keyword k) (if (string? v) (keyword v) v)}))
         (apply merge))
    (catch Exception _
      (throw (ex-info "Could not convert args to map"
                      {:args args})))))

(def input->panner-input
  {0 14
   1 15
   2 16})

(defonce current-panners
  ;; "A map of input (int) to {synth, type}"
  (atom {}))

(defn ctl-synth [synth & params]
  (when synth
    (try (apply o/ctl synth params)
         (catch Exception e (timbre/error e)))))

(defn panner [{:keys [in type] :as _args}]
  (let [current-panner (get @current-panners in)
        in* (input->panner-input in)
        new-panner (case type
                     :clockwise (circle-pan-4ch
                                 (groups/mid)
                                 :in in*
                                 :out 8)
                     :counter-clockwise (circle-pan-4ch
                                         (groups/mid)
                                         :in in*
                                         :direction -1
                                         :out 8)
                     :rand (rand-pan4
                            (groups/mid)
                            :in in*
                            :out 8))]

    (try (when (:synth current-panner)
           (o/ctl (:synth current-panner) :gate 0))
         (catch Exception e (timbre/error e)))
    (swap! current-panners assoc in {:synth new-panner
                                     :type type})))

(defn panner-rate [{:keys [in rate] :as _args}]
  (let [panner (get-in @current-panners [in :synth])]
    (ctl-synth panner :rate rate)))

(comment

  (habitat-osc/init)

  (habitat-osc/responder
   (fn [{:keys [path args] :as msg}]
     (let [args-map (args->map args)]
       (case path
         "/panner" (panner args-map)
         "/panner-rate" (panner-rate args-map)
         "/adios" (println "dicen adios" args)
         (println "Unknown path for message: " msg)))))
  (sini-2)
  (sini :out (input->panner-input 1))
  (whitey :out 14)

  (into {} [(list :a :b)])
  (def randy (rand-pan4 :in 14 :lfo-freq-x 1))
  (o/ctl randy :gate 0 :release 2)
  (def circly (circle-pan-4ch :in 14 :direction -1))

  (o/ctl circly :rate 0.1)
  (o/ctl circly :gate 0 :release 2)
  (o/stop))

(defonce fx (atom {}))

(defn init-fx! []
  (let [fx-g (groups/fx)]
    (reset! fx
            {:rev-1 (rev fx-g 8 0 :amp 2)
             :rev-2 (rev fx-g 9 1 :amp 2)
             :rev-3 (rev fx-g 10 2 :amp 2)
             :rev-4 (rev fx-g 11 3 :amp 2)})))
(oe/defsynth instrument-in
  [in 0
   amp 1
   out 0]
  (o/out out (* amp (o/sound-in in))))

(comment
  (o/stop)
  (reset! current-panners {})
  (groups/init-groups!)
  (init-fx!)
  #_(tieminos.core/rec "habitat-taller-abierto-24-09-2022")
  (sini {:group (groups/early) :out (input->panner-input 0)})
  (sini {:group (groups/early) :freq 500 :out (input->panner-input 1)})
  (panner {:in 1 :type :clockwise})
  (panner-rate {:in 0 :rate 2})
  (o/ctl (:synth (get @current-panners 0)) :gate 0)
  (rand-pan4 (groups/mid) 14 8)
  (o/recording-stop)
  (let [bus 14]
    (instrument-in {:group (groups/early)
                    :in 0
                    :out bus})
    (rand-pan4 (groups/mid) bus 8))
  (let [bus 15]
    (instrument-in {:group (groups/early)
                    :in 1
                    :out bus})
    (rand-pan4 (groups/mid) bus 8))
  (let [bus 16]
    (instrument-in {:group (groups/early)
                    :in 2
                    :out bus})
    (rand-pan4 (groups/mid) bus 8)))

(comment
  (o/demo 20  (-> (o/sound-in 0)
                  (o/free-verb 1 1)
                  (* 1.2)
                  (o/pan4:ar (o/lf-noise1 0.3)
                             (o/lf-noise1 0.3))))
  (o/demo (-> (o/sin-osc)
              (o/distortion2 3)
              (* 0.001))))
