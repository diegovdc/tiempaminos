(ns tieminos.compositions.garden-earth.base
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.cps.core :as cps]
   [erv.scale.core :as scale :refer [+names]]
   [erv.utils.core :as utils]
   [overtone.midi :as midi]
   [potemkin :refer [import-vars]]
   [taoensso.timbre :as timbre]
   [tieminos.compositions.garden-earth.fingerings :as fingerings]
   [tieminos.math.utils :refer [normalize]]
   [time-time.dynacan.players.gen-poly :as gp]))

(defonce groups (atom {}))
(defonce fx (atom {}))
(defn early-g [] [:head (:early @groups)])

;; for the `import-vars` below
(require '[tieminos.midi.core]
         '[tieminos.midi.mpe]
         '[tieminos.midi.algo-note]
         '[erv.utils.core]
         '[time-time.dynacan.players.gen-poly]
         '[tieminos.compositions.garden-earth.fingerings])

(import-vars
 [tieminos.midi.core midi-in-event]
 [tieminos.midi.mpe get-cps-pitch-class mpe-note-on mpe-note-off]
 [tieminos.midi.algo-note algo-note]
 [erv.utils.core wrap-at]
 [time-time.dynacan.players.gen-poly on-event ref-rain refrains]
 [tieminos.compositions.garden-earth.fingerings fingerings])

(defn seconds->dur [secs bpm] (* secs (/ bpm 60)))

(defn dur->bpm [dur-ms] (/ 60000 dur-ms))

(def bpm 900)

(def base-freq 440)

(defmacro log-with-msg [msg code]
  `(do
     (when ~msg (timbre/info ~msg))
     (pprint ~code)
     (println "\n")
     ~code))

(defmacro log [code]
  `(do (pprint ~code)
       (println "\n")
       ~code))


(defn stop
  ([refrain-key]
   (gp/stop refrain-key))

  ([]
   (eval '(reset! tieminos.compositions.garden-earth.synths.recording/recording?
                  false))
   (gp/stop)))

(def midi-out-1 (midi/midi-out "VirMIDI"))

(def eik (-> [1 3 5 7 9 11]
             (->> (cps/make 3)
                  cps/+all-subcps)
             (update :scale (partial +names base-freq))))

(defn has-set? [set* degree]
  (= set* (set/intersection (:set degree) set*)))

;;;;;;;;;;;;;;;;;;;
;;;; auto-scales
;;;;;;;;;;;;;;;;;;

(defonce auto-scales (atom {}))

(defn sub-cps-scale
  [cps-name]
  (-> eik :subcps (get cps-name) :scale))


(defn const-log-fn [tabs]
  (fn [_] (str/join "\n" tabs)))

(defn run-auto-scale
  "To make a basic `log-fn`, that ignores `cps-name`, `const-log-fn` can be used"
  [sub-cps-scales key*
   & {:keys [id durs log-fn]
      :or {id :auto-scale
           durs [130 130 210]
           log-fn (fn [cps-name] "")}}]
  (let [scales (map (fn [cps-name] [cps-name (sub-cps-scale cps-name)])
                    sub-cps-scales)]
    (gp/ref-rain
     :id id
     :tempo 900
     :durs durs
     :on-event (gp/on-event
                (let [[cps-name scale] (wrap-at index scales)]
                  (timbre/info "CPS" index cps-name "\n"
                               (str "\n" (log-fn cps-name))
                               "\n\n\n\n")
                  (swap! auto-scales assoc key* scale))))))

(def eik-notes
  "Eikosany scale grouped-by note name (pitch class name)"
  (->> (eik :scale)
       (group-by #(-> % :pitch :class))
       (map (juxt key (comp first val)))
       (into {})))

(def eik-sets
  "Eikosany scale grouped-by note name (pitch class name)"
  (->> (eik :scale)
       (group-by #(-> % :set))
       (map (juxt key (comp first val)))
       (into {})))

(defn subcps [subcps-name]
  (+names base-freq
          (-> eik :subcps (get subcps-name) :scale)))


(defn scale->fingerings [scale]
  (->> scale
       (+names base-freq)
       (map #(-> % :pitch :class
                 ((juxt identity fingerings/fingerings))))))

(defn pr-set [set*]
  (-> set* str (str/replace "#" "∈")))


(defn pitch-class->pr-fingering [pitch-class]
  (str pitch-class " " (pr-set (:set (get eik-notes pitch-class)))
       "\n"
       (->> pitch-class
            fingerings/fingerings
            (str/join "\n     ")
            (str "     "))))

(defn scale->pr-fingerings
  "Get fingerings for a `scale` as a printable string"
  [scale]
  (->> scale
       (+names base-freq)
       (map #(-> % :pitch :class pitch-class->pr-fingering))
       (str/join "\n\n")))

(comment (scale->fingerings (subcps "1)4 of 3)6 1.3-5.7.9.11")))




(defn dur->env
  "`env-points` is a map of ratios of duration between each segment of the envelope
  Returns a map with the scaled `env-points` and the `dur`"
  [env-points dur]
  (let [env-points* (into [] env-points)
        ratios (map second env-points*)]
    (into {:dur dur}
          (map (fn [[k] r] [k (* dur r)])
               env-points*
               (normalize ratios)))))
(comment
  (dur->env {:a 1 :b 1 :c 2 :r 2} 5)
  (apply + (vals (dur->env {:a 2 :b 4 :c 5 :r 1} 5))))

(do
  (defn interval-from-note
    "By inputing any `note` in `eik` generate an interval ratio
  with reference to `eik-scale`.
  `eik-scale` should be any subset from the `eik` scale.
  `deg-interval` is a degree in `eik-scale`"
    ([eik-scale note deg-interval]
     (let [target (wrap-at deg-interval eik-scale)
           period (scale/get-period 0 (count eik-scale) deg-interval)
           transp (scale/transpose-by (:bounding-period target) period)]
       (* transp (utils/interval (:bounded-ratio note)
                                 (:bounded-ratio target))))))
  (comment)
  (interval-from-note (subcps "1)4 of 3)6 1.3-5.7.9.11")
                      (nth (:scale eik) 0)
                      4))


(defn pc-index [scale pitch-class]
  (.indexOf (map (comp :class :pitch) scale) pitch-class))
(comment (pc-index (:scale eik) "C+59"))

(defn interval-from-pitch-class
  [eik-scale pitch-class deg-interval]
  (interval-from-note eik-scale (get eik-notes pitch-class) deg-interval))

(defn get-pc
  "Get the pitch class string of a note"
  [note]
  (-> note :set eik-sets :pitch :class))

(def scale-freqs-map
  "Maps `eik` freqs between 440 and 880 to their corresponding pitch-classes"
  (->> eik
       :scale
       (scale/+names base-freq)
       (map #(vector (float (* base-freq
                               (% :bounded-ratio)))
                     (:class (:pitch %))))
       (into {})))

(def scale-freqs-ranges
  (let [ranges (->> scale-freqs-map keys sort (partition 2 1))
        ;; add upper and lower bounds so that it wraps on itself
        low-note (first (first ranges))
        high-note (last (last ranges))
        low-range [(/ high-note 2) low-note]
        high-range [high-note (* 2 low-note)]]
    (concat [low-range] ranges [high-range])))



(defn eiko-round-freq
  "Round `freq` to the nearest pitch class in the `eikosany`"
  [freq]
  (let [
        [freq-in-octave transp-period]
        (loop [f freq octaves 1]
          (cond (> 880 f 440) [f octaves]
                (> f 880) (recur (/ f 2) (* 2 octaves))
                (< f 440) (recur (* f 2) (/ octaves 2))))

        [t-freq1 t-freq2] (or (->> scale-freqs-ranges
                                   (filter #(> (second %)
                                               freq-in-octave
                                               (first %)))
                                   first)
                              )
        diff1 (- t-freq1 freq-in-octave)
        diff2 (- t-freq2 freq-in-octave)
        [eik-freq diff-hz] (if (> (Math/abs diff2) (Math/abs diff1))
                             [t-freq1 diff1]
                             [t-freq2 diff2])]
    {:pitch-class (scale-freqs-map eik-freq)
     :eik-freq (* transp-period eik-freq)
     :eik-freq-ref eik-freq
     :transp transp-period
     :diff-hz diff-hz
     :original-freq freq}))

(comment (eiko-round-freq 454.6221))
