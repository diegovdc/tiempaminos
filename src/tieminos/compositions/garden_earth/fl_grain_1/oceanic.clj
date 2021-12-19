(ns tieminos.compositions.garden-earth.fl-grain-1.oceanic
  (:require
   [clojure.data.generators :refer [weighted]]
   [clojure.set :as set]
   [erv.utils.core :as utils]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.base
    :refer [eik interval-from-pitch-class on-event ref-rain stop subcps]]
   [tieminos.compositions.garden-earth.synths.fx :refer [early-g]]
   [tieminos.compositions.garden-earth.synths.granular :as granular]
   [tieminos.compositions.garden-earth.synths.recording :as rec]))

(def cluster-dek (subcps "3)5 of 3)6 1.5.7.9.11"))
(map (juxt :set :pitch) cluster-dek)

;; `{pitch-class [bufkey]}`
(defonce generated-bufkeys (atom {}))
(comment (reset! generated-bufkeys {}))

(defn gen-bufkey! []
  (let [gend @generated-bufkeys
        gend-set (set (keys gend))
        dek-set (->> cluster-dek (map (comp :class :pitch)) set)
        missing-pcs (set/difference dek-set gend-set)
        bufkey (cond
                 (empty? gend) ["A+53" :oceanic 1]
                 (seq missing-pcs)
                 [(rand-nth (vec missing-pcs)) :oceanic 1]
                 :else (let [pc (rand-nth (vec dek-set))]
                         [pc :oceanic (inc (count (get gend pc)))]))]
    (swap! generated-bufkeys update (first bufkey) conj bufkey)
    bufkey))

(defn record-oceanoises []
  (ref-rain
   :id :oceanic/rec
   :durs [5]
   :on-event (on-event
              (let [rec? (weighted {true 60, false 30})]
                (println index rec?)
                (when (and rec? (not @rec/recording?))
                  (reset! rec/recording? true)
                  (rec/rec-flute (+ 2 (rand-int 3))
                                 (gen-bufkey!)
                                 (fn [_] (println (count @generated-bufkeys) "/10"))))))))
(comment

  (do #_(reset! rec/bufs {})
      (reset! rec/recording? false)
      (reset! generated-bufkeys {}))
  (record-oceanoises)
  (stop))

#_(do
  (defmacro defn& [name key-default-map body]
    (let [ks# (vec (keys key-default-map))
          args# ['& {:keys ks# :or key-default-map}]]
      `(defn ~name ~args# ~body)))
  ((defn& holabola {hola 5} hola)))
(apply hash-map [1 2 3 4])

(do
  (defmacro defn& [name key-default-map body]
    (let [key-default-map* (apply hash-map key-default-map)
          ks# (vec (keys key-default-map*))
          args# ['& {:keys ks# :or key-default-map*}]]
      `(defn ~name ~args# ~body)))
  ((defn& holabola [hola 5] hola)))

(do
  (defn& oceanoises
    [amp 6
     durs [1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 3]
     bufs (filter (fn [[[_ id]]] (= :oceanic id)) @rec/bufs)]
    (ref-rain
     :id :oceanic/noises
     :durs durs
     :tempo 120
     :ratio 2
     :on-event (on-event
                (let [[[pc] b] (-> (into [] bufs) rand-nth)
                      dur* (* dur (weighted {1 5 2 5}))
                      rates (map (fn [i]
                                   (interval-from-pitch-class
                                    cluster-dek
                                    pc
                                    (utils/wrap-at
                                     i
                                     [(at-index [1 2 1 2 1 2 5])
                                      (at-index [1 3 2])
                                      (at-index [-9 0])])))
                                 (range 3))
                      trig-rate 100]
                  (doseq [g-dur (take 1 [1/20 1/10 1/3])
                          rate rates]
                    (granular/ocean
                     {:out 8
                      :group [:head early-g]
                      :bu0.f b :a (* 0.1 dur*) :r (* 0.9 dur*)
                      :trig-rate trig-rate :grain-dur g-dur :dur dur*
                      :rate rate :speed (/ 1 dur*) :panl -1 :panr 1 :pan-lfo 2
                      :amp (* amp (rand-nth [10 5 3]))
                      ;; :amp-lfo 5
                      :mix 1
                      :room 2}))))))
  (comment (oceanoises {:amp 6})))

(do
  (defn& oceanoises-pluck
    [bufs (rec/filter* (fn [[_ id]] (= :oceanic id)) @rec/bufs)
     scale (subcps "3)4 of 3)6 3.5.7.9")
     amp 14
     a% 0.1
     r% 0.8
     decay 1
     get-trig-rate (fn [at-idx] (at-idx [100 100 10 10 16 16 8]))
     degrees-layers (fn [at-index]
                      [(at-index [1 2 5])
                       (at-index [1 3 2])
                       (at-index [-9 0 1 2 3])
                       (at-index [4 -4 -3 3 -2 -2 -1 -1])])
     durs [3] #_[3 3 3 3 3 3 3 3 3 3 7 3 3 3 3 3 3 3 3 3 4 1 1 1]
     dur-amp (weighted {1 20 3/2 1 4/3 5}) ;;longer durs amplify sound
     ]
    (if-not (seq bufs) (timbre/error "Bufs can not be empty")
      (ref-rain
       :id :oceanic/noises-pluck
       :durs durs
       :tempo 120
       :ratio 1/9
       :on-event (on-event
                  (let [[[pc] b] (rand-nth (into [] bufs))
                        deg-ls (degrees-layers at-index)
                        rates (map (fn [i]
                                     (interval-from-pitch-class
                                      scale pc
                                      (utils/wrap-at i deg-ls)))
                                   (range (count deg-ls)))
                        dur* (* 1/3 dur-amp)
                        trig-rate (get-trig-rate at-index)
                        [mix amp*] (weighted {[1 5] 10
                                              [0.5 0.8] 5
                                              [0 0.4] 3})]
                    (doseq [g-dur (take 1 [1/10 1/8 1/32])
                            rate rates]
                      (granular/ocean-2
                       {:out 8
                        :group [:head early-g]
                        :buf b :a (* a% dur*) :r (* r% dur*)
                        :trig-rate trig-rate :grain-dur g-dur :dur dur*
                        :rate rate :speed 3 :panl -1 :panr 1 :pan-lfo 5
                        :amp (* amp amp* (rand-nth [10 5 3]))
                        :amp-lfo 5
                        :decay decay
                        :mix mix
                        :room 2})))))))

  (comment
    (do (o/stop) (stop))
    (oceanoises-pluck )))

(comment
  (oceanoises-pluck
   :get-trig-rate (constantly 10)
   :scale (subcps "2)4 of 3)6 11-3.5.7.9")
   :durs [3 3 3]
   :dur-amps (weighted {1 10})
   :amp 14))


(comment
  (o/recording-start "/home/diego/Desktop/oceanoises-pluck2.wav")
  (o/recording-stop)
  )
