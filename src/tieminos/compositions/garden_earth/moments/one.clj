(ns tieminos.compositions.garden-earth.moments.one
  "First piece or section from garden earth.
  Audio routing assumes the use of `garden-earth/one.rpp`"
  (:require
   [clojure.core.async :as a]
   [clojure.string :as str]
   [erv.scale.core :refer [+names]]
   [erv.utils.core :refer [period-reduce]]
   [overtone.core :as o]
   [re-affect.alpha.core :as ræ]
   [taoensso.timbre :as timbre]
   [tieminos.blackhole :as bh]
   [tieminos.compositions.garden-earth.analysis
    :refer [pitch-class->note-set]]
   [tieminos.compositions.garden-earth.base
    :refer [base-freq eik eik-pitch-classes eik-sets
            interval-from-pitch-class2 subcps subcps-graphs]]
   [tieminos.compositions.garden-earth.fl-grain-1.sample-arp
    :refer [arp arp-reponse-2 default-interval-seq-fn]]
   [tieminos.compositions.garden-earth.init :as ge.init]
   [tieminos.compositions.garden-earth.routing :as ge.route]
   [tieminos.compositions.garden-earth.synths.live-signal
    :refer [freq-history pan-verb start-signal-analyzer]]
   [tieminos.compositions.garden-earth.synths.ps-freeze :refer [ps-freeze]]
   [tieminos.compositions.garden-earth.web.ajax
    :refer [post-live-state post-note-tuning]]
   [tieminos.midi.core :refer [get-pacer! midi-in-event]]
   [tieminos.osc.reaper :as reaper]
   [tieminos.sc-utils.ndef.v1 :as ndef]
   [tieminos.sc-utils.recording.v1 :as sc.rec.v1]
   [tieminos.utils :refer [cb-interpolate wrap-at]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

;;;;;;;;;;;;;;;;;;
;; * Config
;;;;;;;;;;;;;;;;;;

(def outputs
  {:main-synth (bh/bus 20)
   :arp (bh/bus 22)
   :harmonizer (bh/bus 24)})

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Re-affect/State init
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private initial-state
  {:section 0
   :arp.refrain/on? false
   :arp/pattern-index 0
   :arp/cps-index 0
   :harmonizer/on? false
   :harmonizer/harmony-index 0})

(defonce ^:private live-state (atom initial-state))

(ræ/reg-state ::db live-state)
(declare reg-event-db dispatch get-subval reg-event-fx reg-fx reg-sub)
(ræ/defapi ::db)

;;;;;;;;;;;;;;;;;;
;; * Arp
;;;;;;;;;;;;;;;;;;

(declare make-repeat-cell)

(defn simple-pattern
  [pattern pitch-class scale]
  (swap! live-state assoc :arp/pattern-str (str (into [] pattern))) ;; just for the UI's benefit
  (map #(interval-from-pitch-class2 scale pitch-class %)
       pattern))

(defn negate-seq [coll] (map #(* -1 %) coll))

(defn make-seq-range
  "Make a sequenential range, always starting from the zeroth degree.
  If `converge?` then the sequence will be reversed."
  [{:keys [len interval down? converge?]}]
  (let [seq* (range 0 (* interval (max 1 len)) interval)]
    (cond-> seq*
      down? negate-seq
      converge? reverse)))

(comment
  (make-seq-range {:len 0
                   :interval 3
                   :down? true
                   :converge? false}))

(def arp-patterns
  "`:fn` is a function that takes a pitch-class and a scale as arguments and should return a sequence of ratios.
  The `pitch-class` is the one with which the sample was tagged."
  [;; S.0
   [{:name (str [0 2])
     :fn #(make-repeat-cell [0 2] %1 %2 {:max-len 15})}
    {:name (str [0 -2])
     :fn #(make-repeat-cell [0 -2] %1 %2 {:max-len 15})}
    {:name (str [0 3 1 -2])
     :fn #(make-repeat-cell [0 3 1 -2] %1 %2 {:max-len 15})}
    {:name (str [6 -6 -12 -6 0 6])
     :fn #(make-repeat-cell (shuffle [6 -6 -12 -6 0 6]) %1 %2 {:max-len 9})}
    {:name ":div-conv"
     :fn #(simple-pattern
           (make-seq-range {:len (rand-int 5)
                            :interval (inc (rand-int 4))
                            :down? (rand-nth [true false])
                            :converge? (rand-nth [true false])})
           %1 %2)}]])

(comment
  ((-> arp-patterns
       (nth 0)
       (nth 3)
       :fn)
   "A+92"
   (subcps "2)4 of 3)6 11-1.5.7.9")))

(defn get-arp-pattern-data
  [pattern-index section]
  (let [index pattern-index
        pattern (->> section :arp :patterns
                     (wrap-at index))]
    {:arp/pattern-index index
     :arp/pattern-name (:name pattern)
     :arp/pattern-fn (:fn pattern)}))

(defn get-arp-scale-data
  [cps-index section]
  (let [index cps-index
        cps (-> section :arp :cps)
        subcps-name (wrap-at index cps)
        scale (subcps subcps-name)]
    {:arp/cps-index  index
     :arp/subcps-name subcps-name
     :arp/harmony-strs [(str/replace subcps-name #"of 3\)6" "")
                        (str/join " " (map (comp :class :pitch) scale))]
     :arp/scale scale}))

(comment
  (dispatch {::inc-arp-cps-index {}})
  (dispatch {::inc-arp-pattern-index {}})
  (dispatch {::inc-harmonizer-harmony-index {}})
  (get-subval ::arp-cps-data)
  (get-subval ::arp-pattern-data))

(comment
  (add-watch live-state
             ::live-state-portal
             (fn [_ _ _ s] (user/tap :live-state s))))

(defonce ^:private last-sets (atom '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Signal Analyzer
;; Signal analyzer
;; Expects the webapp to be running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn on-receive-pitch
  [{:keys [pitch-class diff-cents]
    :as freq-analysis-data}]
  (let [set* (pitch-class->note-set pitch-class)]
    (when (not= (first @last-sets) set*)
      (swap! last-sets #(take 6 (conj % set*))))
    (when-not pitch-class
      (timbre/error (ex-info "Pitch class not found"  freq-analysis-data)))
    #_(println (format "%s {%s} - %s" pitch-class (str/join "." set*) diff-cents))
    (post-note-tuning (assoc freq-analysis-data
                             :label (format "%s {%s}" pitch-class (str/join "." set*))
                             :last-sets @last-sets
                             :diff-cents diff-cents))))

(defn start-signal-analyzer!
  [in]
  (start-signal-analyzer {:in in
                          :freq 10
                          :analyzer-amp 3
                          :pitch-path "/receive-pitch-5"
                          #_#_:scale-freqs-ranges (make-scale-freqs-ranges
                                                   scale-freqs-map
                                                   (set (map (comp :class :pitch)
                                                             scale-1)))
                          :on-receive-pitch #'on-receive-pitch}))

;;;;;;;;;;;;;;;;;;;;
;; * SAMPLE & Hold
;;;;;;;;;;;;;;;;;;;;

;; NOTE: `ge-live-sig/start-signal-analyzer' should be running

(defn stop-sample-arp! []
  (timbre/info :stopping-arp)
  (gp/stop ::arp-rain))

(defn start-sample-arp!
  []
  (timbre/info :starting-arp)
  (ref-rain :id ::arp-rain
            :durs [5 3 8 2 1 5]
            :ratio 1/3
            :on-event (on-event
                       (let [scale (:arp/scale (get-subval ::arp-cps-data))
                             pattern-fn (:arp/pattern-fn (get-subval ::arp-pattern-data))]
                         (if-not (or scale pattern-fn)
                           (timbre/warn ":arp-rain needs scale and pattern" {:scale scale :pattern pattern-fn})
                           (arp {:bufs-atom sc.rec.v1/bufs
                                 :dur 0.5
                                 :index index
                                 :in (ge.route/fl-i1 :bus)
                                 :play-fn #_(partial #'arp-reponse-1 {:scale scale
                                                                      :out (bh 0)})
                                 (partial #'arp-reponse-2 {:scale scale
                                                           :interval-seq-fn (or pattern-fn default-interval-seq-fn)
                                                           :out (outputs :arp)})}))))))
(defn make-repeat-cell
  [pattern-cell
   pitch-class
   scale
   & {:keys [min-len max-len]
      :or {min-len 3 max-len 9}}]
  (let [len (max min-len (rand-int max-len))
        pattern (->> pattern-cell
                     repeat
                     flatten
                     (take len))]
    ;; just for the UI's benefit
    (swap! live-state assoc :arp/pattern-str (str (into [] pattern)))

    (map #(interval-from-pitch-class2 scale pitch-class %)
         pattern)))

;;;;;;;;;;;;;;;;;
;; * Harmonizer
;;;;;;;;;;;;;;;;;

(defn make-harmony
  [root-deg subcps-name]
  (let [scale (+names base-freq (subcps subcps-name))
        root (wrap-at root-deg scale)
        root-ratio (:bounded-ratio root)]
    {:root root
     :harmony (->> scale
                   (map (fn [d] (/ (:bounded-ratio d) root-ratio)))
                   (remove #(= 1 %)))}))

(defn get-harmonizer-data
  [harmony-index section]
  (let [index harmony-index
        [root-deg subcps-name] (->> section :harmonizer :harmonies
                                    (wrap-at index))
        {:keys [root harmony]} (make-harmony root-deg subcps-name)
        subcps-name* (str/replace subcps-name #"of 3\)6" "")
        pitch-class (-> root :pitch :class)
        set* (->> root :set sort (str/join ".") (#(str "{" % "}")))]
    {:harmonizer/harmony-index index
     :harmonizer/harmony harmony
     :harmonizer/harmony-str (str (into [] harmony) " - " subcps-name* " on " pitch-class " " set*)}))

(comment
  (get-section-data (:section @live-state))
  (harmonizer-data-sub @live-state))

(defn start-harmonizer! []
  (timbre/info "(re)starting-harmonizer")
  (if-let [ratios (:harmonizer/harmony (get-subval ::harmonizer-data))]
    (do
      (println ratios)
      (ndef/ndef
       ::harmonizer
       (-> (o/sound-in (ge.route/fl-i1 :in))
           #_(o/delay-l 1 1)
           (o/pitch-shift 0.1 ratios)
           ((fn [sig] (if (> (count ratios) 1) (o/mix sig) sig)))
           (o/free-verb 0.5 3)
           (o/pan2)
           (* 8))
       {:out (outputs :harmonizer)}))
    (timbre/error "No :harmonizer/harmony found")))

(defn stop-harmonizer! []
  (timbre/info :stopping-harmonizer)
  (ndef/stop ::harmonizer)
  (swap! live-state assoc :harmonizer/on? false))

;;;;;;;;;;;;;;;;;;;;
;; ** ps-freeze: estrellas y espejos reflejantes
;; frozen harmonies
;; brief harmonies, toggled with note-on/off that leave a spectrum
;; harmony based on the current note. If note on arp-cps harmony, then use a n-ad (di-tri-tetra) from there, else use the selected harmonizer harmony.
;;;;;;;;;;;;;;;;;;;;

(defn- gen-reflejos-voicing
  [root periods harmony]
  (->> harmony
       (mapv #(* (rand-nth periods) (period-reduce (/ (:bounded-ratio %) root))))))

(defn- prev-harmony-path
  [current-note size]
  [:harmonizer/prev-harmonies current-note size])

(defn- gen-reflejos-harmony
  [size periods note-data graph]
  (let [set* (graph (:set note-data))
        harmony (->> set* (shuffle)
                     (take (dec size))
                     (map eik-sets)
                     (concat [note-data]))

        root (:bounded-ratio (first harmony))]
    {:harmony harmony
     :voicing (gen-reflejos-voicing root periods harmony)}))

(defn prev-reflejos-harmony
  [db pitch-class size]
  (let [prev-harmonies (get-in db (prev-harmony-path pitch-class size))]
    (when prev-harmonies
      (rand-nth (vec prev-harmonies)))))

(defn- gen-reflejos-ps-freeze-data
  [db chord-size periods current-pc]
  (when current-pc
    (let [{:keys [arp/scale arp/subcps-name]} (get-subval ::arp-cps-data)
          note-data (get eik-pitch-classes current-pc)
          prev-harmony (prev-reflejos-harmony db current-pc chord-size)]
      (cond
        (some #(-> % :pitch :class (= current-pc)) scale)
        (gen-reflejos-harmony
         chord-size periods note-data (-> subcps-name subcps-graphs :simple))

        ;; if note is not in harmony but has been used  ~50% chance of using a previous harmony
        (and prev-harmony (> (rand) 0.5))
        {:harmony prev-harmony
         :voicing (gen-reflejos-voicing (:bounded-ratio (first prev-harmony))
                                        periods
                                        prev-harmony)}

        :else (gen-reflejos-harmony
               chord-size periods note-data (-> eik :graphs :simple))))))

(comment
  (map
   #(gen-reflejos-ps-freeze-data {} 2 [1 2] %)
   (keys eik-pitch-classes))
  (-> @live-state)
  (toggle-ps-freeze {:db {}} {:chord-size 2}))

(defn- toggle-ps-freeze
  [{:keys [db]} {:keys [off? chord-size ps-periods synth-params freeze?]
                 ;; `ps-periods` is a vector used by `gen-reflejos-voicing`: it will randomly pick a period for each voice
                 :or {ps-periods [1 1/2 2]}}]
  (let [synth (:synth/ps-freeze db)]
    (if (or synth off?)
      {:db (dissoc db :synth/ps-freeze)
       :fx [(when freeze? [::ctl-synth {:synth synth :params {:freeze-gate 1}}])
            [::stop-synth {:synth synth}]]}
      (let [current-pc (:pitch-class (first @freq-history))
            {:keys [harmony voicing]} (gen-reflejos-ps-freeze-data db chord-size ps-periods current-pc)]
        (when (and harmony voicing)
          {:db (update-in db (prev-harmony-path current-pc chord-size) (fnil conj #{}) harmony)
           :fx {::start-ps-freeze (assoc synth-params :ps-ratios voicing)}})))))

;;;;;;;;;;;;;;;;;;
;; * El camino a través de la foresta: Sections
;;;;;;;;;;;;;;;;;;

;;  
;; dedicada a las palabras Yanomami de Davi Kopenawa
;; ref. Ursula K. LeGuin, The Telling (El Relato)

;; ** 1. Nubosidad del bosque
;; TODO: pt 1 y 2 buscar un modo/cps contrastante con pt 4
;; *** A. Invocación sentida/o
;;  NOTE: "3)4 of 3)6 1.3.5.9"
;;     - eólicos
;;     - melódico, reverberante
;;     - cresciendo energía hasta transición
;; *** B. Transición
;;     - unos cuantos arpegios al final y 1 freeze largo y denso
;; ** 2.Lianas (redes-tejidos del bosque)
;; *** A. Seres
;;     - insectos: clicks y ruidos vocales
;;     - aves: tonos breves, de pronto arp
;;     - aire/hojas: eólicos y silbidos
;; *** B. Primeras luminiscencias
;;     - continuar con 1
;;     - esporádico: ps-freeze breves (ps-amp 0)
;;     - transición: silencio? o?
;; ** 3. El cruce al cielo - cruce del lattice F+56 - E+55 - D#+75
;; *** A. (breve)
;;     - c/armonizador: jet eólico intenso: inspirar - expirar
;;     -   -> respiración circular
;;     - nube de reveración muy larga y varios ps-freeze largos ... 
;; ** 4. Colores y espejos de los xapiri
;;  NOTE: "2)4 of 3)6 9-1.5.7.11"
;; *** A.
;;    - dentro del la nube (decayendo)
;;    - melódico, menos rev
;;      - frecuente con armonizador (buscar acorde, quizá 1.3.5?)
;; *** B. Danzaorquesta de los xapiri
;;    - arpegiador
;;    - poco a poco más ps-freeze
;; ** 5. fin
;;    - bajar el fader del micro
(defn- html-list
  [strs]
  (->> strs
       (map (fn [x] [:li x]))
       (concat [:ul])
       vec))

(def ^:private s0-preinicio
  [:div [:h1 "0. fader abajo"]])

(def ^:private s1-nubosidad-del-bosque
  [:div [:h1 "1. Nubosidad del bosque"]
   [:h2 "A. Invocación sentida/o"]
   (html-list ["eólicos"
               "melódico, reverberante"
               "cresciendo energía hasta transición"])
   [:h2 "B. Transición"
    (html-list ["unos cuantos arpegios al final y 1 freeze largo y denso"])]])

(def ^:private s2-lianas-tejidos
  [:div [:h1 "2.Lianas (redes-tejidos del bosque)"]
   [:h2 "A. Seres"]
   (html-list ["insectos: clicks y ruidos vocales"
               "aves: tonos breves, de pronto arp"
               "aire/hojas: eólicos y silbidos"])
   [:h2 "B. Primeras luminiscencias"
    (html-list ["continuar con 1"
                "esporádico: ps-freeze breves (ps-amp 0)"
                "transición: silencio? o?"])]])

(def ^:private s3-cruce-al-cielo
  [:div [:h1 "3. El cruce al cielo"]
   (html-list ["(breve)"
               "c/armonizador: jet eólico intenso: inspirar - expirar"
               "...   -> respiración circular"
               "nube de reveración muy larga y varios ps-freeze largos ..."])])

(def ^:private s4-colores-espejos-xapiri
  [:div [:h1 "4. Colores y espejos de los xapiri"]
   [:h2 "A. Inicio"]
   (html-list ["dentro del la nube (decayendo)"
               "melódico, menos rev"
               "...  frecuente con armonizador (buscar acorde, quizá 1.3.5?)"])
   [:h2 "B. Danzaorquesta de los xapiri"]
   (html-list ["arpegiador" "poco a poco más ps-freeze"])])

(def ^:private s5-fin
  [:div [:h1 "5. fin"] (html-list ["baja el fader del micro"])])

(def default-arp-config
  {:cps ["3)5 of 3)6 1.3.7.9.11"]
   :patterns (nth arp-patterns 0)})

(def default-harmonizer-config
  {:harmonies [[0 "3)4 of 3)6 1.3.5.9"]]})

(def sections
  ;; FIXME: default configs are necessary to prevent app from breaking
  [{:notes s0-preinicio
    :arp default-arp-config
    :harmonizer default-harmonizer-config}
   ;; TODO: pt 1 y 2 buscar un modo/cps contrastante con pt 4
   {:notes s1-nubosidad-del-bosque
    :arp {:cps ["3)5 of 3)6 1.3.7.9.11"
                #_"2)4 of 3)6 11-1.5.7.9"
                #_"2)4 of 3)6 9-1.5.7.11"]
          :patterns (nth arp-patterns 0)}
    :harmonizer {:harmonies [[0 "3)4 of 3)6 1.3.5.9"]
                             [0 "1)4 of 3)6 3.9-1.5.7.11"]
                             [2 "1)4 of 3)6 5.9-1.3.7.11"]
                             [1 "1)4 of 3)6 1.5-3.7.9.11"]]}}
   {:notes s2-lianas-tejidos
    :arp default-arp-config
    :harmonizer default-harmonizer-config}
   {:notes s3-cruce-al-cielo
    :arp default-arp-config
    :harmonizer default-harmonizer-config}
   {:notes s4-colores-espejos-xapiri
    :arp default-arp-config
    :harmonizer default-harmonizer-config}
   {:notes s5-fin
    :arp default-arp-config
    :harmonizer default-harmonizer-config}])

#_(defn sections
    "`config-key` is something like `:arp` or `:harmonizer`.
  All configs should be wrapped in a `fn`"
    [config-key live-state-data]
    (let [sections*
          {0 {:arp (fn [] {:subcps-name (wrap-at (:arp/cps-index live-state-data 0)
                                                 ["2)4 of 3)6 11-1.5.7.9"
                                                  "2)4 of 3)6 9-1.5.7.11"])
                           :interval-seq-fn (partial make-repeat-cell
                                                     (wrap-at (:arp/pattern-index live-state-data 0)
                                                              [[0 2]
                                                               [0 -2]
                                                               [0 3 1 -2]]))})}}]

      ((get-in sections* [(:section live-state-data 0) config-key]))))

;;;;;;;;;;;;;;;;;;
;; * UI
;;;;;;;;;;;;;;;;;;

(defn post-live-state*
  [live-state-data]
  (post-live-state (-> live-state-data
                       (update :arp/pattern :name)
                       (update :synth/main str)
                       (update :synth/signal-analyzer str)
                       (update :synth/ps-freeze str)
                       (merge (get-subval ::arp-cps-data)
                              {:arp/pattern (:arp/pattern-name (get-subval ::arp-pattern-data))}
                              (get-subval ::harmonizer-data)
                              (get-subval ::section-data)))))

(comment
  (post-live-state* @live-state))
;;;;;;;;;;;;;;;;;;
;; * Events
;;;;;;;;;;;;;;;;;;

(reg-event-fx
 ::init
 (fn [_ {:keys [midi?]}]
   {:db initial-state
    :fx [[::init.fx]
         (when midi? [::init-midi.fx])]}))

(defn- limit-section-index-range
  [i]
  (-> i (max 0) (min (dec (count sections)))))

(reg-event-db
 ::change-section
 (fn [db {:keys [inc?]}]
   (let [op (if inc? inc dec)]
     (update db :section (comp limit-section-index-range op)))))

(comment
  (dispatch {::change-section {:inc? false}})
  (dispatch {::change-section {:inc? true}})
  (-> @live-state :section))

(reg-event-db
 ::inc-arp-cps-index
 (fn [db _]
   (update db :arp/cps-index (fnil inc 0))))

(reg-event-db
 ::inc-arp-pattern-index
 (fn [db _]
   (update db :arp/pattern-index (fnil inc 0))))

(reg-event-db
 ::inc-harmonizer-harmony-index
 (fn [db _]
   (update db :harmonizer/harmony-index (fnil inc 0))))

(reg-event-fx
 ::toggle-sample-arp
 (fn [{:keys [db]} _]
   (let [on? (:arp.refrain/on? db)]
     {:db (assoc db :arp.refrain/on? (not on?))
      :fx (if on?
            {::stop-sample-arp {}}
            {::start-sample-arp {}})})))

(reg-event-fx
 ::toggle-harmonizer
 (fn [{:keys [db]} _]
   (let [on? (:harmonizer/on? db)]
     {:db (assoc db :harmonizer/on? (not on?))
      :fx (if on?
            {::stop-harmonizer {}}
            {::start-harmonizer {}})})))

(def ^:private main-synth-default-params
  {:amp 4
   :min-mix 0.3 :mix 1
   :min-room 0.7 :room 1
   :damp-min 0.3 :damp 0.7
   :pan-min -0.5 :pan 0.5})

(reg-event-fx
 ::start-main-synth
 (fn [{:keys [db]} _]
   (let [sy (:synth/main db)
         sy* (if (o/node-active? sy)
               sy
               (do
                 (timbre/info "Starting main synth")
                 (pan-verb (merge {:in (ge.route/fl-i1 :in)
                                   :out (outputs :main-synth)}
                                  main-synth-default-params))))]
     {:db (assoc db :synth/main sy*)})))

(reg-event-fx
 ::stop-main-synth
 (fn [{:keys [db]} _]
   (let [sy (:synth/main db)]
     {:db (dissoc db :synth/main)
      :fx (when (o/node-active? sy)
            {::stop-synth {:synth sy}})})))

(reg-event-fx
 ::ctl-synth
 (fn [{:keys [db]} {:keys [synth-k params]}]
   (let [sy (get db synth-k)]
     (if-not sy
       (timbre/warn "Synth not found:" synth-k)
       {:fx {::ctl-synth {:synth sy :params params}}}))))

(reg-event-fx
 ::start-signal-analyzer
 (fn [{:keys [db]} _]
   (let [sy (:synth/signal-analyzer db)
         sy* (if (o/node-active? sy)
               sy
               (do
                 (timbre/info "Starting signal analyzer")
                 (:get-signal-pitches-synth (start-signal-analyzer! (ge.route/fl-i1 :in)))))]
     {:db (assoc db :synth/signal-analyzer sy*)})))

(reg-event-fx ::toggle-ps-freeze #'toggle-ps-freeze)

(reg-event-db
 ::on-ps-freeze-start
 (fn [db synth]
   (timbre/info "Star(t)ed: ps-freeze ")
   (assoc db :synth/ps-freeze synth)))

(reg-event-fx
 ::fade-main-bus
 (fn [_ {:keys [level]}]
   {:fx {::fade-main-bus {:level level}}}))

(comment
  (-> @live-state))

;;;;;;;;;;;;;;;;;;
;; * FX
;;;;;;;;;;;;;;;;;;

(reg-fx ::init.fx
        (fn [_ _]
          (timbre/info "(Re)initializing")
          (o/stop)
          (gp/stop)
          (ge.init/init!)
          (reaper/init)
          (add-watch live-state ::post-live-state
                     (fn [_key _ref _old-value new-value]
                       (post-live-state* new-value)))
          (dispatch [[::start-main-synth]
                     [::start-signal-analyzer]])))

(reg-fx ::stop-synth
        (fn [_ {:keys [synth]}]
          (timbre/info "Stopping synth:" synth)
          (when (o/node-active? synth)
            (o/ctl synth :gate 0))))

(reg-fx ::ctl-synth
        (fn [_ {:keys [synth params]}]
          (when (o/node-active? synth)
            (doseq [[k v] params]
              (o/ctl synth k v)))))

(reg-fx ::stop-sample-arp (fn [_ _] (stop-sample-arp!)))

(reg-fx ::start-sample-arp (fn [_ _] (start-sample-arp!)))

(reg-fx ::stop-harmonizer (fn [_ _] (stop-harmonizer!)))

(reg-fx ::start-harmonizer (fn [_ _] (start-harmonizer!)))

(reg-fx ::post-live-state.subfx
        (fn [{:keys [db]} _]
          ;; sometimes the server seems to choke with two posts in quick sucession
          ;; so we delay the subs update by a little bit
          (a/go
            (a/<! (a/timeout 100))
            (post-live-state* db))))

(reg-fx ::start-ps-freeze
        (fn [_ synth-params]
          (let [synth (ps-freeze (merge {:in (ge.route/fl-i1 :in)
                                         :amp 64
                                         :ps-amp 0.5
                                         :freezed-amp 16
                                         :r 5
                                         :out (outputs :harmonizer)}
                                        synth-params))]
            (dispatch {::on-ps-freeze-start synth}))))

(reg-fx ::fade-main-bus
        (fn [_ {:keys [level]}]
          (cb-interpolate
           {:id ::fade-main-bus
            :dur-ms 7777
            :tick-ms 100
            :init-val reaper/zero-db
            :target-val level
            :cb (fn [data]
                  (reaper/set-vol 1 (:val data)))})))

(comment
  (dispatch {::fade-main-bus {:level reaper/zero-db}}))

(reg-fx
 ::init-midi.fx
 (fn [_ _]
   (timbre/info "Initializing MIDI/Pacer")
   (try
      ;; NOTE: using Pacer's TIEMI config
     (midi-in-event
      :midi-input (get-pacer!)
      :note-on (fn [{:keys [note]}]
                 (cond
                     ;; set section
                   (= 2 note) (dispatch {::change-section {:inc? false}})
                   (= 3 note) (dispatch {::change-section {:inc? true}})
                     ;; arp
                   (= 4 note) (dispatch {::toggle-sample-arp {}})

                     ;; arp config
                   (= 5 note) (dispatch {::inc-arp-cps-index {}})
                   (= 6 note) (dispatch {::inc-arp-pattern-index {}})

                     ;; harmonizer
                   (= 7 note) (dispatch {::toggle-harmonizer {}})
                   (= 8 note) (dispatch {::inc-harmonizer-harmony-index {}}))))
     (catch Exception e (timbre/error (.getMessage e))))))

;;;;;;;;;;;;;;;;;;
;; * Subs
;;;;;;;;;;;;;;;;;;

(def post-live-state-fx {::post-live-state (fn [_ _] {::post-live-state.subfx nil})})

(defn get-section-data [section]
  (-> section
      (max 0)
      (min (count sections))
      (wrap-at sections)))

(defn arp-cps-data-sub
  [{:keys [arp/cps-index section] :as _db}]
  (get-arp-scale-data cps-index (get-section-data section)))

(reg-sub ::arp-cps-data
         #'arp-cps-data-sub
         post-live-state-fx)

(defn arp-pattern-data-sub
  [{:keys [arp/pattern-index section] :as _db}]
  (get-arp-pattern-data pattern-index (wrap-at section sections)))

(reg-sub ::arp-pattern-data
         #'arp-pattern-data-sub
         post-live-state-fx)

(defn harmonizer-data-sub
  [{:keys [harmonizer/harmony-index section] :as _db}]
  (get-harmonizer-data harmony-index (get-section-data section)))

(reg-sub ::harmonizer-data
         #'harmonizer-data-sub
         (merge post-live-state-fx
                {::restart-harmonizer (fn [{:keys [db]} _]
                                        (when (:harmonizer/on? db)
                                          {::start-harmonizer {}}))}))

(defn section-data-sub
  [{:keys [section] :as _db}]
  {:section/index section
   :section/notes (:notes (get-section-data section))})

(reg-sub ::section-data
         #'section-data-sub
         (merge post-live-state-fx
                {::fade-main-bus
                 (fn [_ {:keys [section/index]}]
                   (let [level (if (or (zero? index)
                                       (= (dec (count sections)) index))
                                 (do (timbre/info "Fading out main bus")
                                     0)
                                 reaper/zero-db)]
                     {::fade-main-bus {:level level}}))}))

(comment
  ;; TODO:
  ;; harmonizer data should be derived from arp-cps-index, in other words, that is the current harmony
  )

(comment
  (ræ/get-state ::db)
  (dispatch {::fade-main-bus {:level reaper/zero-db}})
  (dispatch {::init {:midi? false}})
  (dispatch {::change-section {:inc? false}})
  (dispatch {::change-section {:inc? true}})

  (get-subval ::arp-cps-data)
  (dispatch {::toggle-sample-arp {}})
  (dispatch {::inc-arp-cps-index {}})
  (dispatch {::inc-arp-pattern-index {}})

  (get-subval ::harmonizer-data)
  (dispatch {::inc-harmonizer-harmony-index {}})
  (dispatch {::toggle-harmonizer {}})

  (dispatch {::start-main-synth {}})
  (dispatch {::stop-main-synth {}})
  (dispatch {::ctl-synth {:synth-k :synth/main
                          :params main-synth-default-params}})

  (post-live-state* @live-state))

(comment
  (-> @live-state)
  (->> @live-state)
  (o/stop)

  (-> freq-history)
  (reset! freq-history nil)
  (-> @sc.rec.v1/bufs
      (get ["G#+42" :sample-arp 5])
      (->> (into {}))))
