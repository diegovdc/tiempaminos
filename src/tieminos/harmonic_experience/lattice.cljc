(ns tieminos.harmonic-experience.lattice
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.lattice.v2 :refer [base-coords ratios->lattice-data]]
   [erv.utils.conversions :as conv]
   [erv.utils.core :refer [round2]]
   [quil.core :as q]
   [tieminos.harmonic-experience.drones.sounds :refer [harmonic]]
   [tieminos.harmonic-experience.utils :refer [intervals midi->ratio&freq]]
   [tieminos.lattice.v1.lattice :as lattice.v1 :refer [add-played-ratio
                                                       remove-played-ratio]]
   [tieminos.math.utils :refer [linexp*]]
   [tieminos.midi.core :refer [midi-in-event]]))

(defn draw [text-type width height lattice-data]
  (fn []
    (let [{:keys [data edges min-x max-x min-y max-y period played-notes]
           :or {played-notes #{}}} @lattice-data
          x-length (->> [min-x max-x]
                        (map #(Math/abs %))
                        (apply +))
          y-length (->> [min-y max-y]
                        (map #(Math/abs %))
                        (apply +))
          cx (/ width 2)
          cy (/ height 2)
          zoom (* 0.7 (min (/ width x-length)
                           (/ height y-length)))]
      (q/background 0)
      (q/translate cx cy)
      (q/scale zoom)
      (comment (q/stroke 255 255)

               (q/rect min-x min-y x-length y-length))

      (q/stroke 255 255)

      (comment (q/fill 255 0)
               (q/stroke-weight 20)
               (q/point 0 0))

      (q/fill 255 255)
      #_(q/push-matrix)
      (q/translate (- (/ (+ max-x min-x) 2))
                   (- (/ (+ max-y min-y) 2)))

      (comment
        (q/stroke 255 255)
        (q/fill 255 255 0 200)
        (q/stroke-weight 2.5)
        (q/rect min-x min-y x-length y-length))

      (q/fill 255)
      (q/stroke-weight 0.2)
      (doseq [edge edges]
        (let [[coords-1 coords-2] edge]
          (q/line (:x coords-1)
                  (:y coords-1)
                  (:x coords-2)
                  (:y coords-2))))
      (q/stroke-weight 3.5)
      (doseq [{:keys [coords ratio]} data]
        (when (played-notes ratio)
          (q/stroke-weight 5.5)
          (q/stroke 150 0 200))
        (q/point (:x coords) (:y coords))
        (q/stroke-weight 3.5)
        (q/stroke 255 255))
      (q/text-font (q/create-font "Monospace" 5) 5)
      (q/stroke-weight 0)
      #_(q/fill 255 0 0)
      (doseq [{:keys [ratio coords numer-factors denom-factors]} data]
        (q/text (let [denom-factors* (str/join "*" (remove #(= period %) denom-factors))]
                  (if (= :factors @text-type)
                    (str (str/join "*" (let [ns (remove #(= period %) numer-factors)]
                                         (if (seq ns) ns [1])))
                         (when (seq denom-factors*)
                           (str "/" denom-factors*)))
                    (str ratio)))
                (+ (:x coords) 2) (- (:y coords) 0.4))))))

(defn draw-lattice
  [{:keys [ratios width height text-type]
    :or {width 800
         height 800
         text-type :ratios              ; #{:factors :ratios}
         }}]
  (let [lattice-data (atom (assoc (ratios->lattice-data base-coords
                                                        ratios)
                                  :played-notes #{}
                                  :text-type text-type))]
    (q/defsketch lattice-tool
      :title "Lattice Tool"
      :host "lattice-canvas"
      :settings #(q/smooth 80)
      :setup (fn []
               #_(q/pixel-density 2)
               (q/frame-rate 24))
      :draw (#'draw (atom text-type) width height lattice-data)
      :size [width height])
    lattice-data))

(defn update-ratios!
  [lattice-data-atom new-ratios]
  (swap! lattice-data-atom
         merge
         (ratios->lattice-data base-coords
                               new-ratios)))

(defonce lattice-sketch-atom (atom nil))

(defn draw-lattice2
  [ratios lattice-size]
  (if @lattice-sketch-atom
    (lattice.v1/update-ratios! @lattice-sketch-atom ratios)
    (reset! lattice-sketch-atom
            (lattice.v1/draw-lattice
             {:ratios ratios
              :width (* 16 lattice-size)
              :height (* 9 lattice-size)
              :on-close (fn [] (reset! lattice-sketch-atom nil))})))
  lattice-sketch-atom)
(comment
  (-> @lattice-sketch-atom))

(defn get-lattice-atom!
  []
  @lattice-sketch-atom)

(defonce played-ratios (atom #{}))

(defn add-played-absolute-ratio
  [ratio]
  (swap! played-ratios set/union #{ratio}))

(defn remove-played-absolute-ratio
  [ratio]
  (swap! played-ratios set/difference #{ratio}))

(defn setup-kb
  [{:keys [midi-kb ref-note root scale lattice? lattice-size]
    :or {lattice? true
         lattice-size 120}}]
  (let [get-note-data (fn [ev] (midi->ratio&freq {:ref-note ref-note
                                                  :root root
                                                  :scale scale
                                                  :midi-note (:note ev)}))
        lattice-atom (when lattice? @(draw-lattice2 (map :bounded-ratio scale) lattice-size))]

    (add-watch played-ratios ::print-intervals
               (fn [_ _ _ new-val]
                 (let [intervals* (intervals new-val)]
                   (println "Intervals:" intervals* (map conv/ratio->cents intervals*)))))

    (when midi-kb
      (midi-in-event
       :midi-input midi-kb
       :note-on (fn [ev]
                  (let [{:keys [ratio freq absolute-ratio]} (get-note-data ev)]
                    (when lattice? (add-played-ratio lattice-atom {:ratio ratio :stroke-weight 10  :color [200 200 120]}))

                    (println (:note ev) ratio (round2 2 (conv/ratio->cents ratio)))
                    (add-played-absolute-ratio absolute-ratio)
                    (harmonic freq :amp (linexp* 0 127 0.1 3 (:velocity ev))
                              :a 5)))
       :note-off (fn [ev]
                   (let [{:keys [ratio absolute-ratio]} (get-note-data ev)]
                     (when lattice? (remove-played-ratio lattice-atom {:ratio ratio}))
                     (remove-played-absolute-ratio absolute-ratio)))))))

(comment
  (reset! lattice-sketch-atom nil))
