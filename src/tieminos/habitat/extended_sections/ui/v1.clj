(ns tieminos.habitat.extended-sections.ui.v1
  (:require
   [clojure.string :as str]
   [erv.utils.core :refer [round2]]
   [overtone.music.time :refer [now]]
   [quil.core :as q]
   [tieminos.habitat.reactivity.amp :refer [amp-analysis]]))

(defonce state
  (atom {:title "Section Title"
         :init-time (now)
         :inputs/lw-avg-amp 0
         :event/base-width 150 ;; width of event when amp is 0
         :events [] ;; {:init-time (now) :end-time (+ 5 (now) :color [20 30 90] :coords {:x 0 :y 0}}
         }))

;; API
(declare random-color height width draw habitat-extended-ui)

(defn init []
  (q/defsketch habitat-extended-ui
    :size [width height]
    :title "Habitat Extended"
    :settings #(q/smooth 80)
    :setup (fn [] (q/frame-rate 24))
    :draw draw)
  (swap! state assoc :ui/inited? true))

(defn init-section
  [{:keys [title]}]
  (swap! state assoc :title title))

(defn add-event [{:keys [dur amp]}]
  (println "ADD EVENT")
  (when (:ui/inited? @state)
    (let [now* (now)
          original-width (* (:event/base-width @state) amp)
          event {:start-time now*
                 :end-time (+ now* (* 1000 dur))
                 :color (random-color)
                 :coords {:y (rand-int height)
                          :x (rand-int width)}
                 :original-width original-width
                 :width original-width}]
      (swap! state update :events conj event))))

(defn- get-rec-bar-config
  [k]
  (let [height 20]
    (case k
      :guitar-bus {:label (str/capitalize (name k))
                   :color [200 0 80]
                   :height height
                   :coords {:x 0 :y (- 1000 height)}}
      :mic-1-bus {:label (str/capitalize (name k))
                  :color [0 200 80]
                  :height height
                  :coords {:x 0 :y (- 1000 (* 2 height))}}
      :mic-2-bus {:label (str/capitalize (name k))
                  :color [0 80 200]
                  :height height
                  :coords {:x 0 :y (- 1000 (* 3 height))}}
      {:label (str/capitalize (name k))
                  :color [0 200 80]
                  :height height
       :coords {:x 0 :y (- 1000 (* 4 height))}})))

(defn add-rec-bar [{:keys [input-bus seconds]}]
  (when (:ui/inited? @state)
    (let [now* (now)
          input-bus-key (keyword (:name input-bus))
          bar-config (get-rec-bar-config input-bus-key)
          total-time (* 1000 seconds)
          rec-bar (merge
                    bar-config
                    {:total-time total-time
                     :end-time (+ now* total-time)
                     :width 0})]
      (swap!
        state
        (fn [s] (update
                  s
                  :rec-bars
                  (fnil assoc {})
                  input-bus-key
                  rec-bar))))))

(update {} :x (fnil assoc {}) :a 5)

(comment
  (-> @state)
  (swap! state dissoc :rec-bars)
  (add-rec-bar {:input-bus {:name "guitar-bus"} :seconds 5})
  (init))


;; impl
;;

(def ^:private width 400)

(def ^:private  height 1000)



(defn- milliseconds-to-mm-ss [milliseconds]
  (let [seconds (/ milliseconds 1000)
        minutes (int (mod (Math/floor (/ seconds 60)) 60))]
    (str (format "%02d" minutes) ":" (format "%02d" (mod (int seconds)
                                                         60)))))

(def ^:private px 5)
(def ^:private title-height 100)

(defn- random-color []
  (shuffle [(+ 156 (rand-int 100))
            (rand-int 256)
            (rand-int 256)]))



(comment
  (swap! state assoc :ui/inited? true)
  (swap! state assoc :events [])
  (doseq [_ (range 200)]
    (add-event {:dur (rand-int 20) :amp (+ 0.7 (rand 0.5))})))

(defn- events []
  (doseq [{:keys [coords color width]} (:events @state [])]
    (apply q/stroke color)
    (apply q/fill color)
    (q/stroke-weight width)
    (q/point (:x coords) (:y coords))))

(defn- rec-bars []
  (let [bars (:rec-bars @state)]
    (when (seq bars)
      (doseq [[_ {:keys [label coords color width height]
                  :or {label "N/A"}}] bars]
        (let [width* (max 0 width)]
          (apply q/stroke color)
          (apply q/fill color)
          (q/stroke-weight width*)
          (q/stroke-weight 1)
          (q/rect (:x coords) (:y coords) width* height)
          (q/text-size 27)
          (q/text label px (+ (:y coords) height)))))))

#_(rec-bars)
(defn- update-events [state-data]
  (let [now* (now)
        events (reduce
                 (fn [evs {:keys [end-time start-time original-width] :as ev}]
                   (let [remaining-time (- end-time now*)]
                     (if (<= remaining-time 0)
                       evs
                       (conj evs
                             (assoc ev
                                    :width (max 0 (* original-width
                                                     (try (/ remaining-time
                                                             (- end-time start-time))
                                                          (catch Exception _ 0)))))))))
                 []
                 (:events state-data))]
    (assoc state-data :events events)))

(defn- update-rec-bars [state-data]
  (let [now* (now)
        rec-bars (reduce
                   (fn [bars [k {:keys [end-time total-time] :as bar}]]
                     (let [remaining-time (- end-time now*)]
                       (if (<= remaining-time 0)
                         bars
                         (assoc
                           bars
                           k (assoc bar
                                    :width (try (max 0 (* width (/ (- total-time remaining-time)
                                                                   total-time)))
                                                (catch Exception _ 0)))))))

                   {}
                   (:rec-bars state-data))]
    (assoc state-data :rec-bars rec-bars)))

(update-rec-bars {:rec-bars {:guitar-bus {:end-time (+ 5000 (now))
                                          :total-time 5000
                                          :width 1184/25}}})
(-> @state)

(defn- title-and-time
  []
  (q/text-font (q/create-font "Monospace" 20) 30)
  (q/fill 255)
  (q/text (:title @state) px px (- width px) title-height)
  (q/text-size 60)
  (q/text (milliseconds-to-mm-ss
            (- (now) (:init-time @state)))
          px
          title-height)
  (q/text-size 30))

(defn- draw []
  (q/background 0)
  (title-and-time)
  (let [avg-30s (-> @amp-analysis
                    :lin-weighted-amps
                    :avg-30s
                    (or 0)
                    )
        avg-5s  (-> @amp-analysis
                    :lin-weighted-amps
                    :avg-5s
                    (or 0)
                    )]
    (q/text (format "30-LW~amp: %s/%s"
                    (round2 3 avg-30s)
                    (round2 2 (* 7 avg-30s)))
            px
            150)
    (q/text (format "5-LW~amp: %s/%s"
                    (round2 3 avg-5s)
                    (round2 2 (* 7 avg-5s)))
            px
            180))

  (q/blend-mode :difference)
  (events)
  (rec-bars)

  (swap! state (comp update-rec-bars
                     update-events)))

(comment
  (swap! state assoc :ui/inited? true)
  (swap! state assoc :events [])
  (doseq [_ (range 200)]
    (add-event {:dur (rand-int 20) :amp (+ 0.7 (rand 0.5))})))
