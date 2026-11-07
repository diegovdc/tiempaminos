(ns tieminos.harmonic-experience.lattice
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.lattice.v2 :refer [base-coords ratios->lattice-data]]
   [erv.utils.conversions :as conv :refer [midi->cps]]
   [erv.utils.core :refer [round2]]
   [overtone.core :as o]
   [quil.core :as q]
   [taoensso.timbre :as timbre]
   [tieminos.harmonic-experience.drones.sounds :refer [harmonic]]
   [tieminos.harmonic-experience.utils :refer [intervals midi->ratio&freq]]
   [tieminos.lattice.v1.lattice :as lattice.v1 :refer [add-played-ratio
                                                       remove-all-played-ratios
                                                       remove-played-ratio]]
   [tieminos.math.utils :refer [linexp*]]
   [tieminos.midi.core :refer [clear-all-synths! midi-in-event]]))

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
  [ratios lattice-size
   & {:as lattice-config}]
  (if @lattice-sketch-atom
    (lattice.v1/update-ratios! @lattice-sketch-atom ratios)
    (reset! lattice-sketch-atom
            (lattice.v1/draw-lattice
             (merge {:id "Harmonic Experience Lattice"
                     :frame-rate 10
                     :ratios ratios
                     :width (* 16 lattice-size)
                     :height (* 9 lattice-size)
                     :on-close (fn [] (reset! lattice-sketch-atom nil))}
                    lattice-config))))
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

(defn reset-played-notes!
  []
  (swap! lattice-sketch-atom update :played-notes
         (fn [m]
           (->> m
                (mapv (fn [[k _v]] [k ()]))
                (into {}))))
  true)

(defn- replace-notes
  [replacement-ratios-map
   scale]
  (->> scale
       (map
        (fn [{:keys [bounded-ratio] :as note}]
          (if-let [replacement (get replacement-ratios-map bounded-ratio)]
            (assoc note
                   :bounded-ratio replacement
                   :ratio replacement)
            note)))))

(defn- play-sound [ev freq {:keys [amp out]
                            :or {amp 1 out 0}
                            :as _synth-config}]

  (let [max-vel 110
        vel (min max-vel (:velocity ev))
        amp (* amp (linexp* 0 max-vel 0.1 0.9 vel))
        a 0.1]
    (timbre/debug {:freq freq :vel (:velocity ev) :amp amp :a a})
    (harmonic {:freq freq
               :amp amp
               :a a
               :curve 2
               :out out})))

;; TODO: move to erv lib
(defn subset-from-degs
  "Make a subset of a scale from vector of degrees"
  [scale degs]
  (mapv (fn [deg] (nth scale deg))
        degs))

(defn setup-kb
  [{:keys [midi-kb kb-degs ref-note root scale lattice? lattice-size
           stroke-width note-color sound? on-note-on
           replacements
           lattice-config synth-config]
    :or {ref-note 60
         root (midi->cps 60)
         lattice? true
         lattice-size 120
         stroke-width 10
         note-color [200 200 120]
         sound? true}}]
  (let [scale* (replace-notes replacements scale)
        kb-scale (if-not kb-degs scale* (subset-from-degs scale* kb-degs))
        get-note-data (fn [ev] (midi->ratio&freq {:ref-note ref-note
                                                  :root root
                                                  :scale kb-scale
                                                  :midi-note (:note ev)}))
        lattice-atom (when lattice? @(draw-lattice2 (map :bounded-ratio scale*) lattice-size lattice-config))]

    (add-watch played-ratios ::print-intervals
               (fn [_ _ _ new-val]
                 (let [intervals* (intervals new-val)]
                   (println "Intervals:" intervals* (map (comp #(round2 1 %) conv/ratio->cents) intervals*)))))

    (when midi-kb
      (midi-in-event
       :midi-input midi-kb
       :note-on (fn [ev]
                  (let [{:keys [ratio freq absolute-ratio]} (get-note-data ev)]
                    (when lattice? (add-played-ratio lattice-atom {:ratio ratio
                                                                   :group-id ::note
                                                                   :stroke-weight stroke-width
                                                                   :color note-color}))

                    (add-played-absolute-ratio absolute-ratio)
                    (when on-note-on (on-note-on {:ratio ratio :absolute-ratio absolute-ratio}))
                    (when sound? (play-sound ev freq synth-config))))
       :cc (fn [{:keys [note velocity]}]
          ;; for some reason javax.sound.midi misses some note-off messages so...
             (when (and (= note 21) ;; knob 1 click
                        (= velocity 127))
               (clear-all-synths!)
               (reset! played-ratios #{})
               (remove-all-played-ratios lattice-atom)))
       :mpe {:z (fn [synth val]
                  (o/ctl synth :amp (* (:amp synth-config 1) (min 1 (linexp* 20 127 0.1 1 val)))))}
       :note-off (fn [ev]
                   (let [{:keys [ratio absolute-ratio]} (get-note-data ev)]
                     (when lattice? (remove-played-ratio lattice-atom {:ratio ratio, :group-id ::note}))
                     (remove-played-absolute-ratio absolute-ratio)))))))

(comment
  (reset! lattice-sketch-atom nil))
