(ns tieminos.scales.17o7
  (:require
   [clojure.edn :as edn]
   [clojure.set :as set]
   [erv.utils.ratios :refer [ratios->scale]]
   [erv.utils.scale :refer [dedupe-scale]]
   [overtone.midi :as midi]
   [tieminos.midi.plain-algo-note :refer [malgo-note]]
   [tieminos.osc.surge :as surge]
   [time-time.dynacan.players.gen-poly :as gp]))

;;  some scales with 17 and 7
(def scales-17&7
  (map #(ratios->scale 3 %)
       [(let [cell [1 17/7]]
          ;; chida
          (concat cell
                  (map #(* 3/2 %) cell)
                  (map #(* 17/5 3/2 %) cell)))

        (let [cell [1 17/16 (* 3 7/17) 4/3]]
          ;; no ta mal
          (concat cell
                  (map #(* 3/2 %) cell)
                  (map #(* 3/2 3/2 %) cell)))
        ;; bonita
        [1
         2
         (* 2 17/5)
         (* 2 17/5 17/5)
         4/3
         3/2
         17/5
         (* 3/2 17/5)]
        [1
         17/11
         17/7
         7/4
         (* 8/7 7/4)
         2
         3/2
         4/3
         17/5
         17/9
         (* 17/9 10/9)
         10/6]
        (let [cell [1 17/7 4/3]]
          ;; chida
          (concat cell
                  (map #(* 3/2 %) cell)
                  (map #(* 3/2 4/3 %) cell)))]))

(comment
  (midi/midi-sinks)
  (def sink (midi/midi-out "VirMIDI"))
  (def sink2 (midi/midi-out "VirMIDI Bus 3"))
  (def sink3 (midi/midi-out "VirMIDI Bus 4"))
  (surge/init)
  (gp/stop :sequencer2)
  (let [scale (dedupe-scale (ratios->scale 3
                                           (let [cell [1 17/7 4/3]]
                                             ;; chida
                                             (concat cell
                                                     (map #(* 3/2 %) cell)
                                                     (map #(* 3/2 4/3 %) cell)))))
        scale-data {:meta {:scl/name "17/7"
                           :scl/description "An experiment"}
                    :scale scale}
        scale-size (count (:scale scale-data))]
    (gp/ref-rain
     :id :sequencer2
     :tempo 160
     :durs [1/2]
     :on-event (gp/on-event
                #_(println data)
                (when (#{0 1 3} (mod i 4))
                  (malgo-note {:sink sink
                               :dur 1/30
                               :vel (min 127 (int (* 12 (at-i [8 3 4 5 3]))))
                               :scale-size scale-size
                               :base-midi-chan 2
                               :base-midi-deg 60
                               :deg (+ (at-i [4 4 4 1 4 1 1 1 4 1 1])
                                       (at-i (range (count scale))))}))

                (when (#{1 2} (mod i 3))
                  (malgo-note {:sink sink2
                               :dur 1/3
                               :vel (at-i [127 80 80])
                               :scale-size scale-size
                               :base-midi-deg 60
                               :deg 0}))
                (when (#{1 2 4 6 8 9} (mod i 11))
                  (malgo-note {:sink sink3
                               :dur 1/3
                               :vel (at-i [127 80 80])
                               :scale-size scale-size
                               :base-midi-deg 60
                               :deg (at-i [0 0 5 6 7])}))))
    (surge/set-scale
     {:scale scale-data
      :scale-name "dev/17o7"})
    scale-data)
  (gp/stop))

(comment
  (require '[tieminos.lattice.v1.lattice :as lattice.v1]
           '[tieminos.pitch-wheel.v1.pitch-wheel :as pitch-wheel.v1]
           '[erv.constant-structures.brute-force :as cs.brute-force])

  (lattice.v1/draw-lattice
   {:ratios (let [cell [1 17/7 4/3]]
              (concat cell
                      (map #(* 3/2 %) cell)
                      (map #(* 3/2 4/3 %) cell)))
    :period 3
    :custom-edges #{17/7}
    :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[2 3]])})
  (def scale
    (->> (let [cell [1 17/7 4/3]]
           (concat cell
                   (map #(* 3/2 %) cell)
                   (map #(* 3/2 4/3 %) cell)))
         (ratios->scale 3)
         (map :ratio)
         sort
         (dedupe)))

  (do
    (def brute-force-results (cs.brute-force/find-1
                              (->> scale (ratios->scale 3))
                              3 100
                              4 (fn [n] (assoc n  :color [255 0 0]))))
    brute-force-results)

  (->> scale
       (ratios->scale 3))
  ;;
  (def pw-atom (pitch-wheel.v1/init! {:scale (ratios->scale 3 scale)
                                      :period 3}))

  (-> brute-force-results
      :constant-structures
      (nth 0)
      :added-notes)

  (pitch-wheel.v1/update! pw-atom
                          {:analyze-cs? true
                           :added-notes
                           (map (fn [r] {:bounded-ratio r :color [255 0 0]})
                                #_[8/7
                                   (* 3/2 8/7 17/16)
                                   (* 3/2 8/7 17/16 17/14)]
                                [9/8
                                 17/9
                                 9/4])})
  (def br-results (let [scale [{:ratio 1, :bounded-ratio 1, :bounding-period 3}
                               {:bounded-ratio 9/8}
                               {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                               {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                               {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                               {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                               {:bounded-ratio 17/9}
                               {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                               {:bounded-ratio 9/4}
                               {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                               {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}]
                        results (cs.brute-force/find-1
                                 scale
                                 3
                                 66
                                 4
                                 (fn [n] (assoc n  :color [255 0 0])))]
                    results))
  (def pw-atom (pitch-wheel.v1/init! {:scale [{:ratio 1, :bounded-ratio 1, :bounding-period 3}
                                              {:bounded-ratio 9/8}
                                              {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                                              {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                                              {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                                              {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                                              {:bounded-ratio 17/9}
                                              {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                                              {:bounded-ratio 9/4}
                                              {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                                              {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}]
                                      :period 3}))

  (erv.scale.scl/spit-file "/Users/diego/Music/tunings/17o7-variations/11tone-cs-17o7.scl"
                           {:meta {:scl/name "11tone-cs-17o7.scl"
                                   :scl/description "17o7 scale with a constant structure wrapping"}
                            :scale [{:ratio 1, :bounded-ratio 1, :bounding-period 3}
                                    {:bounded-ratio 9/8}
                                    {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                                    {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                                    {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                                    {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                                    {:bounded-ratio 17/9}
                                    {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                                    {:bounded-ratio 9/4}
                                    {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                                    {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}]})
  ;; 15 tones
  (pitch-wheel.v1/update! pw-atom
                          {:analyze-cs? true
                           :added-notes (map (fn [r] {:bounded-ratio r :color [255 0 0]})

                                             [17/13       ;; 1.2570133745218284
                                              17/10       ;; 1.6414832176209966
                                              (* 17/13 2) ;; 2.4966610978032238
                                              (* 17/13 2 5/4) ;; 2.6944671537313805
                                              ])})
  (def br-results2 (let [scale [{:ratio 1, :bounded-ratio 1, :bounding-period 3}
                                {:bounded-ratio 9/8}
                                {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                                {:bounded-ratio 17/13, :color [255 0 0]}
                                {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                                {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                                {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                                {:bounded-ratio 17/10, :color [255 0 0]}
                                {:bounded-ratio 17/9}
                                {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                                {:bounded-ratio 9/4}
                                {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                                {:bounded-ratio 34/13, :color [255 0 0]}
                                {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}
                                {:bounded-ratio 85/26, :color [255 0 0]}]
                         results (cs.brute-force/find-1
                                  scale
                                  3
                                  66
                                  2
                                  (fn [n] (assoc n  :color [255 0 0])))]
                     results))

  (erv.scale.scl/spit-file "/Users/diego/Music/tunings/17o7-variations/15tone-cs-17o7.scl"
                           {:meta {:scl/name "15tone-cs-17o7.scl"
                                   :scl/description "17o7 scale with a constant structure wrapping"}
                            :scale [{:ratio 1, :bounded-ratio 1, :bounding-period 3}
                                    {:bounded-ratio 9/8}
                                    {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                                    {:bounded-ratio 17/13, :color [255 0 0]}
                                    {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                                    {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                                    {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                                    {:bounded-ratio 17/10, :color [255 0 0]}
                                    {:bounded-ratio 17/9}
                                    {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                                    {:bounded-ratio 9/4}
                                    {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                                    {:bounded-ratio 34/13, :color [255 0 0]}
                                    {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}
                                    {:bounded-ratio 85/26, :color [255 0 0]}]})
  ;; 17 tones
  (let [{:keys [scale+added-notes]}
        (pitch-wheel.v1/update! pw-atom
                                {:analyze-cs? true
                                 :scale [{:ratio 1, :bounded-ratio 1, :bounding-period 3}
                                         {:bounded-ratio 9/8}
                                         {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                                         {:bounded-ratio 17/13, :color [255 0 0]}
                                         {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                                         {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                                         {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                                         {:bounded-ratio 17/10, :color [255 0 0]}
                                         {:bounded-ratio 17/9}
                                         {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                                         {:bounded-ratio 9/4}
                                         {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                                         {:bounded-ratio 34/13, :color [255 0 0]}
                                         {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}
                                         {:bounded-ratio 85/26, :color [255 0 0]}]
                                 :added-notes (map (fn [r] {:bounded-ratio r :color [255 0 255]})

                                                   [(#(/ % (inc %)) 11)
                                                    (#(/ % (inc %)) 26)])})]

    scale+added-notes)

  (erv.scale.scl/spit-file "/Users/diego/Music/tunings/17o7-variations/17tone-cs-17o7.scl"
                           {:meta {:scl/name "17tone-cs-17o7.scl"
                                   :scl/description "17o7 scale with a constant structure wrapping"}
                            :scale [{:bounded-ratio 11/12, :color [255 0 255]}
                                    {:bounded-ratio 26/27, :color [255 0 255]}
                                    {:ratio 1, :bounded-ratio 1, :bounding-period 3}
                                    {:bounded-ratio 9/8}
                                    {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                                    {:bounded-ratio 17/13, :color [255 0 0]}
                                    {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                                    {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                                    {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                                    {:bounded-ratio 17/10, :color [255 0 0]}
                                    {:bounded-ratio 17/9}
                                    {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                                    {:bounded-ratio 9/4}
                                    {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                                    {:bounded-ratio 34/13, :color [255 0 0]}
                                    {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}
                                    {:bounded-ratio 85/26, :color [255 0 0]}]})

  (def lattice-atom (lattice.v1/draw-lattice
                     {:ratios (map :bounded-ratio [{:bounded-ratio 11/12, :color [255 0 255]}
                                                   {:bounded-ratio 26/27, :color [255 0 255]}
                                                   {:ratio 1, :bounded-ratio 1, :bounding-period 3}
                                                   {:bounded-ratio 9/8}
                                                   {:ratio 17/14, :bounded-ratio 17/14, :bounding-period 3}
                                                   {:bounded-ratio 17/13, :color [255 0 0]}
                                                   {:ratio 4/3, :bounded-ratio 4/3, :bounding-period 3}
                                                   {:ratio 3/2, :bounded-ratio 3/2, :bounding-period 3}
                                                   {:ratio 34/21, :bounded-ratio 34/21, :bounding-period 3}
                                                   {:bounded-ratio 17/10, :color [255 0 0]}
                                                   {:bounded-ratio 17/9}
                                                   {:ratio 2N, :bounded-ratio 2N, :bounding-period 3}
                                                   {:bounded-ratio 9/4}
                                                   {:ratio 17/7, :bounded-ratio 17/7, :bounding-period 3}
                                                   {:bounded-ratio 34/13, :color [255 0 0]}
                                                   {:ratio 8/3, :bounded-ratio 8/3, :bounding-period 3}
                                                   {:bounded-ratio 85/26, :color [255 0 0]}])
                      :period 3
                      :custom-edges #{17/7}
                      :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[2 3]])}))
  (-> br-results
      :constant-structures
      (nth 3)
      :added-notes
      (->> (map :ratio)))

  (def *22tone17o7
    (concat
     (->> scale
          (ratios->scale 3)
          (map #(assoc % :scale-order 0)))
     (->> (map #(* % 2) scale)
          (ratios->scale 3)
          (map #(assoc %
                       :color [0 255 255]
                       :scale-order 1)))
     (->> (map #(* % 4/3) scale)
          (ratios->scale 3)
          (map #(assoc %
                       :color [0 0 255]
                       :scale-order 2)))
     (->> (map #(* % 8/3) scale)
          (ratios->scale 3)
          (map #(assoc %
                       :color [255 0 255]
                       :scale-order 3)))
     (->> (map #(* % 16/9) scale)
          (ratios->scale 3)
          (map #(assoc %
                       :color [100 255 25]
                       :scale-order 4)))
     (->> (map #(* % 32/27) scale)
          (ratios->scale 3)
          (map #(assoc %
                       :color [180 100 155]
                       :scale-order 5)))
     (->> (map #(* % 64/27) scale)
          (ratios->scale 3)
          (map #(assoc %
                       :color [180 155 100]
                       :scale-order 6)))
     (->> (map #(* % 128/81) scale)
          (ratios->scale 3)
          (map #(assoc %
                       :color [180 0 0]
                       :scale-order 6)))))

  (defn merge-notes
    [notes]
    (reduce
     (fn [data note]
       (-> data
           (merge (dissoc note :scale-order))
           (update :scale-orders (fnil conj #{}) (:scale-order note))))
     {}
     notes))

  (def **22tone17o7 (->> *22tone17o7
                         (group-by (juxt :bounding-period :bounded-ratio))
                         (map (fn [[_k notes]]
                                (merge-notes notes)))
                         (sort-by :bounded-ratio)
                         (map-indexed (fn [i note]
                                        (assoc note :degree i)))))

  (lattice.v1/draw-lattice
   {:ratios (map :bounded-ratio **22tone17o7)
    :period 3
    :custom-edges #{17/7}
    :coords (erv.lattice.v2/swap-coords erv.lattice.v2/base-coords [[2 3]])})

  (erv.scale.scl/spit-file "/Users/diego/Music/tunings/17o7-variations/22tone-17o7-parallell.scl"
                           {:meta {:scl/name "22tone-17o7-parallell.scl"
                                   :scl/description "17o7 transposed within itself to different degrees"}
                            :scale **22tone17o7})

  (def cs-of-22t (->> **22tone17o7
                      (cs.brute-force/cs-subsets 11)))
  (spit "src/tieminos/scales/17o7/constant-structures-of-22tone-17o7_v1.edn"
        (str (into [] cs-of-22t)))
  (set/subset? #{1} #{1 2})

  (def cs-of-22t*
    (->>  cs-of-22t
          (filter #(set/subset? (set scale) (->> % (map :bounded-ratio) set)))
          (group-by count)
          (mapcat (fn [[size scales]]
                    (map-indexed
                     (fn [index scale] {:scale/index index
                                        :size size
                                        :scale scale})
                     scales)))))

  (doseq [{:keys [scale/index size scale]} cs-of-22t*]
    (let [name* (format "%stone-cs-including-17o7_v%s.scl" size index)]
      (erv.scale.scl/spit-file
       (format "/Users/diego/Music/tunings/17o7-variations/%s" name*)
       {:meta {:scl/name name*
               :scl/description (format "%s tone constant structure subset containing the 8 tone 17o7" size)}
        :scale scale})))

  (let [scale-orders (->> **22tone17o7
                          (map :scale-orders)
                          (apply concat)
                          set
                          sort)]
    (->> scale-orders
         (map (fn [order] (->> **22tone17o7
                               (filter #((:scale-orders %) order))
                               (map :degree))))))

  (let [size 22
        degss '((0 4 6 9 11 14 18 20)
                (0 2 6 11 12 14 18 20)
                (2 3 6 11 12 14 16 20)
                (2 3 6 8 12 16 17 20)
                (3 8 10 12 16 17 20 21)
                (1 3 8 10 12 13 17 21)
                (1 3 5 7 10 13 15 17 19 21))
        intervals (fn [degs]
                    (let [init (if (= 0 (first degs)) [] [0])
                          final (if (= 22 (last degs)) [] [22])]
                      (->> (concat init degs final)
                           (partition 2 1)
                           (map (fn [[a b]] (- b a))))))]
    (map intervals degss))

  (->> @pw-atom
       :scale+added-notes
       (filter :scale-order)
       (sort-by :scale-order)

       #_count)
  (->> @pw-atom
       :scale+added-notes
       count)
  (pitch-wheel.v1/update! pw-atom {:analyze-cs? false
                                   :added-notes (concat
                                                 (->> (map #(* % 2) scale)
                                                      (ratios->scale 3)
                                                      (map #(assoc %
                                                                   :color [0 255 255]
                                                                   :scale-order 1)))
                                                 (->> (map #(* % 4/3) scale)
                                                      (ratios->scale 3)
                                                      (map #(assoc %
                                                                   :color [0 0 255]
                                                                   :scale-order 2)))
                                                 (->> (map #(* % 8/3) scale)
                                                      (ratios->scale 3)
                                                      (map #(assoc %
                                                                   :color [255 0 255]
                                                                   :scale-order 3)))
                                                 (->> (map #(* % 16/9) scale)
                                                      (ratios->scale 3)
                                                      (map #(assoc %
                                                                   :color [100 255 25]
                                                                   :scale-order 4)))
                                                 (->> (map #(* % 32/27) scale)
                                                      (ratios->scale 3)
                                                      (map #(assoc %
                                                                   :color [180 100 155]
                                                                   :scale-order 5)))
                                                 (->> (map #(* % 64/27) scale)
                                                      (ratios->scale 3)
                                                      (map #(assoc %
                                                                   :color [180 155 100]
                                                                   :scale-order 6)))
                                                 (->> (map #(* % 128/81) scale)
                                                      (ratios->scale 3)
                                                      (map #(assoc %
                                                                   :color [180 0 0]
                                                                   :scale-order 6))))})

  "LsLsLLss"
  ;; analysis of
  (def scale
    (->> (let [cell [1 17/7 4/3]]
           (concat cell
                   (map #(* 3/2 %) cell)
                   (map #(* 3/2 4/3 %) cell)))
         (ratios->scale 3)
         (map :ratio)
         sort
         (dedupe))))

(comment

  ;; as two pentatonic scales a fifth appart

  (map #(* % 2/3) [#_3/2 34/21 2/1 17/7 8/3 3/1]) ;; (68/63 4/3 34/21 16/9 2N)

  [17/14 4/3 3/2 34/21 2]
  [68/63 4/3 34/21 16/9 2N]

  (map :ratio (dedupe-scale (ratios->scale (concat
                                            [17/14 4/3 3/2 34/21 2]
                                            (map #(/ % 16/9) [68/63 4/3 34/21 16/9 2N])))))
  (ratios->scale [17/14 4/3 3/2 34/21 2])
  (surge/set-scale
   {:scale {:meta {:scl/name "second child of 17/7"
                   :scl/description "An experiment"}
            :scale  (dedupe-scale (ratios->scale (concat
                                                  [17/14 4/3 3/2 34/21 2]
                                                  (map #(/ % 16/9) [68/63 4/3 34/21 16/9 2N]))))}
    :scale-name "dev/17o7-penta1"}))
