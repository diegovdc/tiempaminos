(ns tieminos.pitch-wheel.v1.pitch-wheel
  (:require
   [erv.constant-structures.core :refer [analyze]]
   [erv.cps.core :as cps]
   [erv.edo.core :as edo]
   [erv.utils.conversions :as convo]
   [quil.core :as q]))

(defn setup []
  #_(q/pixel-density 2)
  (q/frame-rate 1))

(def width 600)
(def height 600)

(defn ratio->radians
  ([ratio] (ratio->radians ratio 2))
  ([ratio period-ratio]
   (q/radians (- (* 360 (/ (convo/ratio->cents ratio)
                           (convo/ratio->cents period-ratio))) 90))))

(def additional-note-color [0 255 0])

(defn scale+added-notes
  "`scale` is a scale, added notes is a vector of numbers"
  [scale added-notes]
  scale
  #_(->> scale
         (group-by :bounded-ratio)
         (map #(-> % second first))
         (concat (map
                  #(assoc {:color additional-note-color}
                          :bounded-ratio %)
                  added-notes))
         (sort-by :bounded-ratio)))

(defn make-arcs [analysis period]
  (->> analysis
       :intervals
       (mapcat (fn [[_ {:keys [intervals]}]]
                 (map
                  (fn [{:keys [steps interval]}]
                    {:start (ratio->radians (first interval) period)
                     :end (ratio->radians (second interval) period)
                     :steps steps})
                  intervals)))))

(defn draw* [state]
  (let [{:keys [scale+added-notes arcs max-arcs period analyze-cs?]} @state
        cx (/ width 2)
        cy (/ height 2)
        diam (* 0.9 height)
        radius (/ diam 2)]
    (q/background 0)
    (q/translate cx cy)
    (q/stroke 255)

    (q/no-fill)
    (q/stroke-weight 4)

    (q/stroke 0 100 0 170)

    #_(doseq [{:keys [bounded-ratio] :as note} ref-scale]
        (q/push-matrix)
        (q/rotate (ratio->radians bounded-ratio))
        (q/line 0 0 radius 0)
        (q/pop-matrix))

    (q/stroke 255)
    (doseq [{:keys [bounded-ratio color] :as _note} scale+added-notes]
      (q/push-matrix)
      (q/rotate (q/radians (- (* 360 (/ (convo/ratio->cents bounded-ratio)
                                        (convo/ratio->cents period)))
                              90)))
      (if color
        (apply q/stroke color)
        (q/stroke 255))
      (q/line 0 0 radius 0)
      (q/pop-matrix))

    (q/stroke 255)

    ;; Draw a circle at x y with the correct diameter
    (q/ellipse 0 0 diam diam)

    (when analyze-cs?
      (doseq [[i {:keys [start end]}] (take max-arcs (map-indexed vector arcs))]
        (q/stroke 255 0 0)
        (let [i* (* i 10)]
          (q/arc 0 0 (+ i* (- radius 100))
                 (+ i* (- radius 100)) start end))))))
(defn draw [state]
  (fn []
    (draw* state)))

(defn concat-scales [& scales]
  (->> (flatten scales)
       (sort-by (juxt :bounded-ratio :scale-order))
       (reduce (fn [scale note]
                 (let [last-note (last scale)]
                   (if (and (= (:bounded-ratio last-note)
                               (:bounded-ratio note))
                            (= (:bounding-period last-note)
                               (:bounding-period note)))
                     scale
                     (conj scale note))))
               [])))

(defn make-state
  [{:keys [scale period added-notes analyze-cs?]
    :or {period 2
         added-notes []}}]
  (let [scale+added-notes* (concat-scales scale added-notes)
        state (let [analysis (-> (analyze scale+added-notes*) :non-cs-intervals)
                    arcs (make-arcs analysis period)]
                {:scale scale
                 :period period
                 :added-notes added-notes
                 :scale+added-notes scale+added-notes*
                 :analyze-cs? analyze-cs?
                 :arcs arcs
                 :total-arcs (count arcs)
                 :max-arcs 100})]
    state))

(defn update! [state {:keys [_scale _period _added-notes _analyze-cs?] :as config}]
  (reset! state (make-state (merge
                             {:scale (:scale @state)
                              :period (:period @state)
                              :added-notes (:added-notes @state)}
                             config))))

(defn init! [{:keys [_scale _period _added-notes] :as config}]
  (let [state (atom (make-state config))]
    (q/defsketch pitch-wheel
      :title "Pitch Wheel"
      :settings #(q/smooth 80)
      :setup setup
      :draw (draw state)
      :size [width height])
    state))

(comment
  (def scale (:scale (cps/make 2 [1 3 5 7 11] :norm-fac 77)))
  (println (map :bounded-ratio scale))
  (def scale (:scale (cps/make 2 [1 2 5 7]  :period 3)))
  (map (juxt :ratio :bounded-ratio :set) scale)
  (clojure.pprint/pprint scale)
  (def s (init! {:scale scale
                 :added-notes [;; 57/49
                              ;; 7/6
                              ;; 19/16
                              ;; 76/63
                              ;; 1701/1444
                               #_(convo/cents->ratio 400)
                              ;; 19/16
                              ;; 5/4
                              ;; 76/63
                              ;; 24/19
                              ;; 700
                              ;; 900
                              ;; 1020
                               ]}))
  (update! s scale [#_(convo/cents->ratio 780)
                    #_(convo/cents->ratio 0)
                    #_(convo/cents->ratio 780)
                    #_(convo/cents->ratio 280)
                    #_(convo/cents->ratio 120)
                    #_(convo/cents->ratio 290)
                    11/9
                    #_(convo/cents->ratio 580)
                    11/8
                    #_(convo/cents->ratio 350)
                    #_(convo/cents->ratio 850)
                    8/5
                    #_(convo/cents->ratio 750)
                    #_(convo/cents->ratio 820)
                    #_(convo/cents->ratio 930)]))
