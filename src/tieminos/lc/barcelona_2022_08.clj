(ns tieminos.lc.barcelona-2022-08
  (:require
   [clojure.set :as set]
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [erv.utils.core :refer [interval wrap-at]]
   [overtone.core :as o]
   [overtone.midi :as midi]
   [tieminos.midi.algo-note :refer [algo-note]]
   [tieminos.midi.mpe
    :refer [all-notes-off get-cps-pitch-class mpe-note-off mpe-note-on]]
   [tieminos.overtone-extensions :as oe]
   [tieminos.synths :refer [sharp-plate]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]
   [time-time.standard :refer [rrand]]))

(comment
  "Usage"
  (require '[tieminos.midi.core :refer [midi-in-event]]
           '[tieminos.midi.mpe :refer [mpe-note-on mpe-note-off]]
           '[erv.cps.core :as cps])
  (def out-1 (midi/midi-out "VirMIDI"))
  (def out-2 (midi/midi-out "VirMIDI 2"))
  (def hex (cps/make 2 [11 13 15 17]))
  (all-notes-off out-1)
  (mpe-note-on :sink out-2
               :scale (:scale hex)
               :base-freq 30
               :get-pitch-class get-cps-pitch-class
               :deg-offset 0
               :midi-note 4
               :vel 10)
  (mpe-note-off out-2 4)

  (algo-note :sink out-1
             :dur 120 ;; milliseconds
             :scale (:scale hex)
             :base-freq 200
             :get-pitch-class get-cps-pitch-class
             :deg-offset 0
             :midi-note 4
             :vel 60)
  (gp/stop)
  (ref-rain :id :bd
            :durs [3 2 3 3 2]
            :tempo 131
            :ratio 1/8
            :on-event (on-event
                       (algo-note :sink out-1
                                  :dur (rrand 80 90) ;; milliseconds
                                  :scale (:scale hex)
                                  :base-freq 30
                                  :get-pitch-class get-cps-pitch-class
                                  :deg-offset 0
                                  :midi-note [(at-index [2 10 2 2 4 2 3 5])
                                              #_(at-index [8 10 0 0 8 13 14 11 12])]
                                  :vel (at-index [80 127 60 120 40]))))

  (ref-rain :id :pad
            :durs [3 2 3 3 2 3 2]
            :tempo 131
            :ratio 10
            :on-event (on-event
                       (do #_(println dur-ms)
                           (algo-note :sink out-2
                                      :dur dur-ms
                                      :scale (:scale hex)
                                      :base-freq 120
                                      :get-pitch-class get-cps-pitch-class
                                      :deg-offset -6
                                      :midi-note [(at-index [7 4 8 6 9 10 13 5 11])
                                                  #_(at-index [17 18 19 10])]
                                      :vel (at-index [40 120 40 90])))))
  (ref-rain :id :pad-2
            :durs [3 2 2 4 2 3 2]
            :tempo 131
            :ratio 6
            :on-event (on-event
                       (do #_(println dur-ms)
                           (algo-note :sink out-2
                                      :dur dur-ms
                                      :scale (:scale hex)
                                      :base-freq 120
                                      :get-pitch-class get-cps-pitch-class
                                      :deg-offset 8
                                      :midi-note [(at-index [6 10 14 11 3 2])
                                                  (at-index [10 5 11 3 2 15 5])]
                                      :vel (at-index [120]))))))

;;;;;;;;;;;;;;;;;;;;;;
;;;HEBDOMEKOTANY;;;;;;
;;;;;;;;;;;;;;;;;;;;;;


(def hebdo (cps/make 3 [1 19 5 7 11 13 3]))
(def hebdo-graph (-> hebdo
                     :graphs
                     :full))
(-> hebdo-graph)
(interval 5 4)
(do
  (defn sort-by-closest-interval
    "NOTE only does descending intervals for now"
    [reference-note graph &
     {:keys [desc?]
      :or {desc? true}}]
    (let [set* (:set reference-note)
          bounded-ratio* (:bounded-ratio reference-note)]
      (->> reference-note
           graph
           (filter (fn [note*]
                     (= (count (set/intersection (:set note*) set*))
                        (dec (count set*)))))
           (map (fn [{:keys [bounded-ratio bounding-period] :as note*}]
                  (assoc note* :interval
                         (let [ratio (interval bounded-ratio* bounded-ratio)]
                           (if (> ratio 1)
                             (/ ratio bounding-period)
                             ratio)))))
           (sort-by :interval >)))))

(sort-by-closest-interval
 {:set #{7 1 9}
  :archi-set #{:e :d :a}
  :ratio 63
  :bounded-ratio 63/32
  :bounding-period 2}
 hebdo-graph)
(* 63/32 17/32 17/32)
(sort-by-closest-interval
 {:set #{7 1 17}
  :archi-set #{:g :d :a}
  :ratio 119
  :bounded-ratio 119/64
  :bounding-period 2}
 hebdo-graph)

(defn make-interval-chain
  [starting-note total-notes nth-closest graph]
  (loop [chain [starting-note]]
    (if (> (count chain) total-notes)
      chain
      (recur (conj chain
                   (wrap-at nth-closest
                            (sort-by-closest-interval
                             (dissoc (last chain) :interval)
                             graph)))))))

(def cycle-0
  "descending cycle using distance-1"
  [{:ratio 39, :archi-set #{:b :f :a}, :freq 3900.0}
   {:ratio 273, :archi-set #{:b :d :f}, :freq 3412.5}
   {:ratio 455, :archi-set #{:c :d :f}, :freq 2843.75}
   {:ratio 105, :archi-set #{:c :b :d}, :freq 2625.0}
   {:ratio 195, :archi-set #{:c :b :f}, :freq 2437.5}
   {:ratio 351, :archi-set #{:e :b :f}, :freq 2193.75}
   {:ratio 663, :archi-set #{:g :b :f}, :freq 2071.875}
   {:ratio 39, :archi-set #{:b :f :a}, :freq 1950.0}])

(def cycle-1
  "descending cycle using distance-2"
  [{:ratio 39, :archi-set #{:b :f :a}, :freq 3900.0}
   {:ratio 65, :archi-set #{:c :f :a}, :freq 3250.0}
   {:ratio 117, :archi-set #{:e :f :a}, :freq 2925.0}
   {:ratio 27, :archi-set #{:e :b :a}, :freq 2700.0}
   {:ratio 189, :archi-set #{:e :b :d}, :freq 2362.5}
   {:ratio 351, :archi-set #{:e :b :f}, :freq 2193.75}
   {:ratio 39, :archi-set #{:b :f :a}, :freq 1950.0}])

(def cycle-2
  "descending cycle using distance-3"
  [{:ratio 35, :archi-set #{:c :d :a}, :freq 7000.0}
   {:ratio 15, :archi-set #{:c :b :a}, :freq 6000.0}
   {:ratio 51, :archi-set #{:g :b :a}, :freq 5100.0}
   {:ratio 21, :archi-set #{:b :d :a}, :freq 4200.0}
   {:ratio 273, :archi-set #{:b :d :f}, :freq 3412.5}
   {:ratio 819, :archi-set #{:e :d :f}, :freq 2559.375}
   {:ratio 91, :archi-set #{:d :f :a}, :freq 2275.0}
   {:ratio 35, :archi-set #{:c :d :a}, :freq 1750.0}])

(->> (make-interval-chain
      (->> hebdo-graph keys rand-nth)
      200
      0
      hebdo-graph)
     (reduce (fn [freq-chain note]
               (cond
                 (empty? freq-chain)
                 [(assoc note :freq (float (* 200 (:bounded-ratio note) 32)))]
                 (> 50 (:freq (last freq-chain)))
                 (conj freq-chain
                       (assoc note :freq (float (* (:freq (last freq-chain))
                                                   (:interval note)
                                                   256))))
                 :else
                 (conj freq-chain
                       (assoc note :freq (float (* (:freq (last freq-chain))
                                                   (:interval note)))))))
             [])

     #_(map #(select-keys % [:ratio :set :interval :freq])))
(comment
  (let [chain (->> (make-interval-chain
                    (->> hebdo-graph keys rand-nth)
                    200
                    0
                    hebdo-graph)
                   (reduce (fn [freq-chain note]
                             (cond
                               (empty? freq-chain)
                               [(assoc note :freq (float (* 200 (:bounded-ratio note) 32)))]
                               (> 50 (:freq (last freq-chain)))
                               (conj freq-chain
                                     (assoc note :freq (float (* (:freq (last freq-chain))
                                                                 (:interval note)
                                                                 256))))
                               :else
                               (conj freq-chain
                                     (assoc note :freq (float (* (:freq (last freq-chain))
                                                                 (:interval note)))))))
                           []))
        cycle cycle-2]
    (ref-rain :id :bd
              :durs [3 2 3 3 2]
              :tempo 131
              :ratio 1/2
              :on-event (on-event [(sharp-plate :freq (/ (:freq (at-index
                                                                 (rand-nth [cycle (reverse cycle)])))
                                                         (at-index [16 8 16 2]))
                                                :amp (at-index [0.5 1 0 0.1])
                                                :mod-freq (at-index [30 2 50])
                                                :dcy (at-index [0.1 2 0.1 0.5]))

                                   (sharp-plate :freq (/ (:freq (at-index
                                                                 (rand-nth [cycle (reverse cycle)])))
                                                         (at-index [16 4 1 2 1]))
                                                :amp (at-index [(rand-nth [1 0]) 1 0.2 2 0.1])
                                                :dcy (at-index [1 2 0.1 0.5])
                                                :mod-freq (at-index [(at-index [3000 100 3 2000 5000])]))]))))

(comment
  (o/recording-start "recordings/interval-chains2.wav")
  (o/recording-stop)

  (gp/stop))


;;;;;;;;;;;;;;
;; Cascade
;;;;;;;;;;;;;;


(->> (cps/make 3 [47 53 59 67 71 79 89] :norm-fac (* 47 53 59))
     :scale
     (map :ratio)
     reverse)

(def h2 (->> (cps/make 3 [47 53 59 67 71 79 89] :norm-fac (* 47 53 59))
             :scale
             (sort-by :ratio)))

(def ratio->ratios
  (->> (cps/make 3 [47 53 59 67 71 79 89] :norm-fac (* 47 53 59))
       :graphs
       :full
       (map (juxt (comp :ratio first)
                  (comp sort (partial map :ratio) second)))
       (into {})))

(defn make-cascade [length ratio->ratios]
  (loop [cascade [(->> ratio->ratios keys sort last)]]
    (if (> (count cascade) length)
      cascade
      (recur (conj cascade
                   (let [prev-note (last cascade)
                         next-note (last (take-while #(< % prev-note)
                                                     (ratio->ratios prev-note)))]
                     (if next-note
                       next-note
                       (rand-nth (take-last 5 (ratio->ratios prev-note))))))))))

(comment
  (gp/stop)
  (o/stop)

  (tieminos.core/rec "cheap-imitation")
  (o/recording-stop)

  (defn lfo [freq min* max*]
    (o/lin-lin (o/lf-noise1 freq) -1 1 min* max*))
  (do
    (o/defsynth cascader2
      [freq 85
       amp 0.5
       atk 0.1
       dcy 1]
      (o/out 0
             (-> (mapv
                  (fn [h]
                    (o/pan2 (+ (* (* (/ 1 h) #_(lfo 1 0 1))
                                  (o/sin-osc (* h freq)
                                             :phase (lfo 4 -1 1)))
                               (* (* (/ 1 h) (lfo 1 0 1))
                                  (o/sin-osc (o/range-lin (o/sin-osc (/ freq 4 #_(lfo 0.1 3 5)))
                                                          (- (* h freq) freq)
                                                          (+ (* h freq) freq)))))
                            (lfo 1 -1 1)))
                  (conj (range 1 15 2)))
                 o/mix
                 (o/free-verb 1 1)
                 (* amp (o/env-gen (o/env-perc atk dcy) :action o/FREE))))))
  (def cascade (map #(float (* 85 4 %))
                    (make-cascade 500 ratio->ratios)))

  (def current-ratio (atom 1))
  (ref-rain
   :id ::cascade-2
   :durs [7 5]
   :ratio 1
   :on-event (on-event
              (do
                (cascader2
                 :amp 1
                 :freq (* 85 4 (swap! current-ratio #(rand-nth (ratio->ratios %))))
                 :atk (* (rand-nth [2 3 5]) dur)
                 :dcy  (* (rand-nth [2 3 5]) dur)))))

  (o/stop)

  (defn get-fracts
    [scale]
    (let [facts (sort (map :ratio scale))]
      (->> facts
           (map-indexed (fn [i f] {(keyword (str "fac-" (inc i)))
                                   (/ f (first facts))}))
           (into {}))))

  (def h3 (-> (cps/make 3 [47 53 59 67 71 79 89] :norm-fac (* 47 53 59))
              cps/+all-subcps))
  (-> h3 :subcps
      keys sort)
  (def facts
    (memoize (fn  [subcps-str]
               (-> h3
                   :subcps
                   (get subcps-str)
                   :scale
                   get-fracts))))

  (def sub-cps
    [;; "1)3 of 3)7 53.79-67.71.89"
     ;; "1)3 of 3)7 53.89-47.59.67"
     ;; "1)3 of 3)7 53.89-47.59.71"
     ;; "1)3 of 3)7 53.89-47.59.79"
     ;; "1)3 of 3)7 53.89-47.67.71"
     ;; "1)3 of 3)7 53.89-47.67.79"
     ])
  (def sub-cps
    [;; "3)4 of 3)7 47.59.71.89"
     ;; "3)4 of 3)7 47.59.79.89"
     ;; "3)4 of 3)7 47.67.71.79"
     ;; "3)4 of 3)7 47.67.71.89"
     ;; "3)4 of 3)7 47.67.79.89"
     ;; "3)4 of 3)7 47.71.79.89"
     ;; "3)4 of 3)7 53.59.67.71"
     "3)4 of 3)7 53.59.67.79"
     ;; "3)4 of 3)7 53.59.67.89"
     ;; "3)4 of 3)7 53.59.71.79"
     ;; "3)4 of 3)7 53.59.71.89"
     ])
  (def sub-cps ["2)4 of 3)7 71-47.53.67.79"
                ;; "2)4 of 3)7 71-47.53.67.89"
                ;; "2)4 of 3)7 71-47.53.79.89"
                "2)4 of 3)7 71-47.59.67.79"
                "2)4 of 3)7 71-47.59.67.89"])
  (-> h3 :subcps (get (wrap-at 0 sub-cps))
      :scale
      (scale/deg->freq 200 1))

  (wrap-at 0 sub-cps)
  (gp/stop)
  (do
    (oe/defsynth cps-3
      [freq 200
       fac-1 1 fac-2 2 fac-3 3
       amp-1 0.5 amp-2 0.5 amp-3 0.5
       amp 1
       atk 0.1
       dcy 0.5
       pan 0]
      (o/out 0
             (-> (+ (o/sin-osc (* freq fac-1) :mul amp-1)
                    (o/sin-osc (* freq fac-2) :mul amp-2)
                    (o/sin-osc (* freq fac-3) :mul amp-3))
                 (o/pan2)
                 (* amp (o/env-gen (o/env-perc atk dcy) :action o/FREE)))))
    (cps-3 (merge (facts (rand-nth sub-cps))
                  {:amp-1 0.7 :amp-2 0.4 :amp-3 0.2})))
  (tieminos.core/rec "coso-cepeesoso")
  (o/recording-stop)
  (gp/stop)
  (ref-rain
   :id :fact-rain
   :durs (flatten [2])
   :ratio 1/10
   :on-event (on-event
              (do
                (when-not (#{7 8 9} (mod index 10))
                  (cps-3 (merge
                          (facts (wrap-at index sub-cps))
                          {:freq  (-> h3 :subcps (get (wrap-at (inc index) sub-cps))
                                      :scale
                                      (scale/deg->freq
                                       (at-index (concat (repeat 1 100)
                                                         [200 100 200]))
                                       (+ (at-index [-6 5 0 0 0])
                                          (at-index [0 1 0 11 12 13]))))
                           :amp-1 0.7 :amp-2 0.4 :amp-3 0.2
                           :amp   (at-index [1])
                           :atk   (at-index (concat (repeat 17 0.01)))
                           :dcy   (at-index [0.3 0.5])})))

                (when-not (#{1 5} (mod index 9))
                  (cps-3 (merge
                          (facts (wrap-at index sub-cps))
                          {:freq  (-> h3 :subcps (get (wrap-at index sub-cps))
                                      :scale
                                      (scale/deg->freq
                                       200
                                       (+ (at-index [0 10])
                                          (at-index [0 0 1 2 3 4]))))
                           :amp-1 0.7 :amp-2 0.4 :amp-3 0.2
                           :amp   (at-index [1 0.5 0.7])
                           :atk   (at-index (concat (repeat 17 0.01)
                                                    [0.4 1]))
                           :dcy   (at-index [0.3 0.5])})))
                (when-not (#{1 8} (mod index 9))
                  (cps-3 (merge
                          (facts (wrap-at index sub-cps))
                          {:freq  (-> h3 :subcps (get (wrap-at (inc index) sub-cps))
                                      :scale
                                      (scale/deg->freq
                                       200
                                       (+ #_(at-index [0 0 0 1 11 1 11 2 12 3])
                                          (at-index (reverse (range 10 21))))))
                           :amp-1 0.7 :amp-2 0.4 :amp-3 0.2
                           :amp   (at-index [1 0.5 0.7])
                           :atk   (at-index (concat (repeat 3 0.01)
                                                    [0.4 1]))
                           :dcy   (at-index [0.3 0.5 1])})))))))
