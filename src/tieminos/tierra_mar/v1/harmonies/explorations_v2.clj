(ns tieminos.tierra-mar.v1.harmonies.explorations-v2
  (:require
   [clojure.pprint :as pp]
   [erv.cps.core :refer [+all-subcps]]
   [erv.scale.core :refer [deg->freq]]
   [erv.scale.scl :as scl]
   [erv.utils.conversions :refer [ratio->cents]]
   [erv.utils.ratios :refer [ratios->scale]]
   [overtone.core :as o]
   [taoensso.timbre :as timbre]
   [tieminos.blackhole :as bh]
   [tieminos.compositions.garden-earth.analysis :refer [find-subcps-intersections
                                                        find-supersets]]
   [tieminos.compositions.garden-earth.base :refer [+degrees base-freq eik
                                                    eik-ratios eik-sets
                                                    pitch-class->pr-fingering
                                                    subcps]]
   [tieminos.compositions.garden-earth.synths.general :refer [tuning-monitor]]
   [tieminos.midi.core :refer [get-oxygen! midi-in-event]]
   [tieminos.osc.surge :as surge]
   [tieminos.seq-utils.core :refer [rainseq]]
   [tieminos.seq-utils.utils :refer [repcat]]
   [tieminos.utils :refer [rrange throttle]]
   [time-time.dynacan.players.refrain.v2 :as rain.v2]))

;; * Harmonic Summary
(def olivo (subcps "3)5 of 3)6 1.3.5.9.11"))
(def olivo->lluvia (subcps "3)4 of 3)6 1.3.5.9"))
(def lluvia (subcps "3)5 of 3)6 1.3.5.7.9"))
(def lluvia->campos (subcps "2)4 of 3)6 5-1.3.7.9"))
(def campos (subcps "2)5 of 3)6 5-1.3.7.9.11"))
(def envio (subcps "1)4 of 3)6 1.5-3.7.9.11"))

(def scale-seq
  [olivo
   olivo->lluvia
   lluvia
   lluvia->campos
   campos
   envio])

(comment
  (-> eik)
  ;; make scl file that maps to the base freq 440 (which doesn't appear in the scale
  ;; ;; => 33/32
  (-> eik :scale first :bounded-ratio))

(def scl-base-freq (float (* base-freq 33/32))) ;; => 453.75 - MTS-ESP should set this as the freq
  ;; eik scale with the first note as 1/1

(def scl-eik (->> eik :scale (map (fn [note]
                                    (-> note
                                        (update :bounded-ratio / 33/32)
                                        (assoc-in [:pitch :base-freq] scl-base-freq))))))

(def eik-data {:meta {:scl/name "Wilson's 1.3.5.7.9.11 Eikosany"
                      :scl/description "Used for the Garden Earth project. The flute tunes the 1/1 to 453.75 (* 33/32 440)."}
               :scale scl-eik})

(def base-kbm-config
  {:path "/Users/diego/Music/tunings/wilson-1-3-5-7-9-11-eikosany_garden-earth"
   :scale-data eik-data
   :middle-note 60
   :middle-note-freq (/ scl-base-freq 2)})

(defonce current-subcps (atom campos))

(defn set-cps-kbm!
  [cps-name]
  (let [cps (subcps cps-name)
        degs (+degrees cps)]
    (surge/set-kbm
     (assoc base-kbm-config
            :kbm-name cps-name
            :scale-data eik-data
            :degrees degs))

    (reset! current-subcps cps)
    (timbre/info "Switching current-subcps to:" cps-name)))

;; ** Init Surge
(comment
  (surge/init)

  (scl/spit-file
   "/Users/diego/Music/tunings/wilson-1-3-5-7-9-11-eikosany_garden-earth/eik.scl"
   eik-data))

;; * Secciones
;; ** Cosmos: Nubosidades lorentzianas =libre? o quizá un 3)4=
;; *** cuerpo-tierra-quipu

;; ** Olivo =3)5 of 3)6 1.3.5.9.11=
(comment
  (set-cps-kbm! "3)5 of 3)6 1.3.5.9.11"))

;; *** Armonías encontradas

(def olivo-armonias
  ;; olivo
  [{:id :olivo-1
    :subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes '(48 55 60)}
   {:id :olivo-1.2
    :subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes '(48 55 60 65 67 68 70 72)}

 ;; sombra verde-oscuro
   {:id :sombra-verde-oscuro-1
    :subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes '(50 54 57 60 62 63 65)}
   {:id :sombra-verde-oscuro-2
    :subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes '(50 54 57 60 62 65 70)}
   {:id :sombra-verde-oscuro-3
    :subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes '(50 54 57 60 62 63 67 70)}

 ;; verde mate
   #_{:subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes (50 55 59 67)} ;; - tension
   {:id :verde-mate-1
    :subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes '(50 54 58 66)}
   {:id :verde-mate-2
    :subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes '(50 54 58 63)}
   {:id :verde-mate-3
    :subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes '(50 54 58 62)}
   {:id :verde-mate-4
    :subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes '(50 54 58 62 63 65 67 68)}

   {:id :unknown
    :subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes '(51 55 59 68)}

 ;; célula melódica
   {:id :celula-melodica
    :subcps "3)5 of 3)6 1.3.5.9.11", :midi-notes '(64 65 67 68 70 72)}])

;; **** Síntes
;; plasmonic: hale-bopp, olmedron

;; ** Intersección Olivo -> Lluvia =3)4 of 3)6 1.3.5.9=
;; El CPS intersectado es: =3)4 of 3)6 1.3.5.9=
(comment
  (def inter-sets
    (->> ["3)5 of 3)6 1.3.5.7.9"  ;; lluvia
          "3)5 of 3)6 1.3.5.9.11" ;; olivo
          ]
         (mapcat subcps)
         (map :set)
         (frequencies)
         (filter #(= 2 (second %)))
         (map first)))

  (->> (find-supersets (set inter-sets))
       (map first)))

;; ** Lluvia: ="3)5 of 3)6 1.3.5.7.9"=
(comment
  (set-cps-kbm! "3)5 of 3)6 1.3.5.7.9"))

(def lluvia-armonias
  [;; chord I
   {:id :chord-1
    :subcps "3)5 of 3)6 1.3.5.7.9" , :midi-notes '(53 56 58 61 62 63)}
   ;; `61` es nota opcional
   {:id :chord-1b
    :subcps "3)5 of 3)6 1.3.5.7.9", :midi-notes '(53 56 58 60 62 63)}

   ;; acorde II
   {:id :chord-2
    :subcps "3)5 of 3)6 1.3.5.7.9", :midi-notes '(52 55 57 59 61 65)}

   ;; un buen arpegio (descendente)
   {:id :down-arpegio
    :subcps "3)5 of 3)6 1.3.5.7.9", :midi-notes '(48 51 53 55 56 58 60 62 63 65 67)}

   ;; arpegio complementario
   {:id :down-arpegio-complement
    :subcps "3)5 of 3)6 1.3.5.7.9", :midi-notes '(48 50 51 53 55 57 59 61 65)}])

;; ** Transición-Intersección Lluvia -> Campos: ="interseccion 2)5 5-1.3.7.9.11 - 3)5 1.3.5.7.9"=
;; Este es el CPS que intersecta =3)4 of 3)6 1.3.5.9=

'(#{1 3 5} #{3 9 5} #{1 9 5} #{1 3 9})

;; Este cps no es la intersección, pero puede funcionar, contiene 3 de los conjuntos del =3)4= de arriba.
(def lluvia->campos-pseudo-intersection
  "2)4 of 3)6 5-1.3.7.9"
  [["G+88" #{1 3 5}]
   ["C+59" #{7 9 5}]
   ["A+92" #{3 9 5}]
   ["A#+55" #{7 1 5}]
   ["D+90" #{1 9 5}]
   ["F+56" #{7 3 5}]])

(comment
  (set-cps-kbm! "2)4 of 3)6 5-1.3.7.9"))

(comment
  (surge/set-kbm
   (let [name* "pseudo-interseccion 2)5 5-1.3.7.9.11 - 3)5 1.3.5.7.9"
         degs (->>
               (map second)
               (map eik-sets)
               (map :degree))]
     (assoc base-kbm-config
            :kbm-name name*
            :scale-data eik-data
            :degrees degs))))

(def lluvia->campos-armonias
  ;; en realidad cualquier cosa de esta armonia funciona, pero aquí unas cuantas seleciones
  [{:id :chord-1
    :subcps "2)4 of 3)6 5-1.3.7.9", :midi-notes [53 56 58 60 62 64 66 70]} ;; funciona bien como arpegio rápido
   {:id :chord-2
    :subcps "2)4 of 3)6 5-1.3.7.9", :midi-notes [54 56 60 63 67]}
   {:id :chord-3
    :subcps "2)4 of 3)6 5-1.3.7.9", :midi-notes [55 57 59 62 65 66 68 69]}])

;; *** Cálculo
;; "3)5 of 3)6 1.3.5.7.9"

(def campo-lluvia-inter-sets
  (->> ["3)5 of 3)6 1.3.5.9.11" ;; campos
        "3)5 of 3)6 1.3.5.7.9"  ;; lluvia
        ]
       (mapcat subcps)
       (map :set)
       (frequencies)
       (filter #(= 2 (second %)))
       (map first)))

(-> campo-lluvia-inter-sets)
;; => (#{1 3 5} #{3 9 5} #{1 9 5} #{1 3 9})

(->> (find-supersets (set campo-lluvia-inter-sets))
     (map first))

;; ** Campos Electromágneticos  ="2)5 of 3)6 5-1.3.7.9.11"=
;; Partimos de esta dekany como armonía principal: =3)5 of 3)6 1.3.5.9.11=. Es brillante y funcion bien
;; El propósito será construir una secuencia que no lleve a esta armonía
(comment
  (set-cps-kbm! "2)5 of 3)6 5-1.3.7.9.11"))

;; *** Ideas sobre 2)5 of 3)6 5-1.3.7.9.11" - acorde brillante
;; 1. Acorde para la parte más "climática"
;;    Usando el Oxygen en la  =octava + 1=
;; =Ab C F Ab' C' Eb' Ab'' Bb'' (B'' A'')=

;; =Ab C F Ab' B' D'...=

;;   - Hay un bonito juego entre C-Eb y B-D...

;; 2. =B'-D'-A''= también funciona bien, es un contraste, podría pertenecer a uno de los lados del campo y también ser una armonía en sí misma

(def campo-armonias
  [;; chord 1
   {:id :chord-1
    :subcps "2)5 of 3)6 5-1.3.7.9.11", :midi-notes [56 60 65 68 72 75 80 82]}
   ;; chord 1.1
   {:id :chord-1.1
    :subcps "2)5 of 3)6 5-1.3.7.9.11", :midi-notes [56 60 65 68 72 75 81 83]}

   ;;chord 1.2
   {:id :chord-1.2
    :subcps "2)5 of 3)6 5-1.3.7.9.11", :midi-notes [56 60 65 68 71 74 83]}
   ;;chord 1.2.1
   {:id :chord-1.2.1
    :subcps "2)5 of 3)6 5-1.3.7.9.11", :midi-notes [56 60 65 68 71 74 82]}
   ;;chord 1.2.2
   {:id :chord-1.2.2
    :subcps "2)5 of 3)6 5-1.3.7.9.11", :midi-notes [56 60 65 68 71 74 82 83]}

   ;; chord 1.3
   {:id :chord-1.3
    :subcps "2)5 of 3)6 5-1.3.7.9.11", :midi-notes [56 60 65 68 71 74 81]}

   ;; chord 1.4 - brightest chord
   {:id :chord-1.4
    :subcps "2)5 of 3)6 5-1.3.7.9.11", :midi-notes [62 66 68 72 74 75 78 80 82]}

   ;; meloharmonic interplay (can work within the chords above) 
   {:id :meloharmony-a
    :subcps "2)5 of 3)6 5-1.3.7.9.11", :midi-notes [71 74]}
   {:id :meloharmony-b
    :subcps "2)5 of 3)6 5-1.3.7.9.11", :midi-notes [72 75]}

   ;; very beautiful, a bit dark (darker than the others) like a starry sky
   {:id :starry-sky
    :subcps "2)5 of 3)6 5-1.3.7.9.11", :midi-notes [71 74 79 80 81 82]}])

#_(-> @selections)

;; **** Hexany (intuición inicial) =2)4 of 3)6 5-1.3.9.11=
;; Contenida en la dekany. Fue una primera intuición.

;; candidata para `campos`
(-> eik :subcps
    (get "2)4 of 3)6 5-1.3.9.11")
    (+all-subcps)
    :subcps
    #_(->> (filter (fn [[_ d]]
                     (-> d :meta :size (= 4))))))

;; Los subsets =3)4= que ya conozco y me gustan
;; => 3)4
'("3)4 of 3)5 1.3.5.9" ;; conocida (grabada)
  "3)4 of 3)5 3.5.9.11" ;; desierto - explorada conjuntamente con la de abajo - fue difícil percibir su caracter el día que las toqué, pero escuchando la grabación sugiere un espacio vacío y desierto
  "3)4 of 3)5 1.3.9.11" ;; difícil, quizá muy neutro o aún no le encuentro mucho
  "3)4 of 3)5 1.5.9.11"
  "3)4 of 3)5 1.3.5.11" ;; pro
  )
;; Los supersets en los que aparece
(->> "2)4 of 3)6 5-1.3.9.11"
     subcps
     (map :set)
     set
     find-supersets
     keys)

;; =3)5 of 3)6 1.3.5.9.11= Me gusta, contiene la hexany a la vez que es más o menos brillante

;; **** Intersecciones

(->> (find-subcps-intersections
      "2)5 of 3)6 5-1.3.7.9.11"
      #{"3)5"})
     (map first))

;; Estas funcionan bien:
;; "3)5 of 3)6 1.3.5.9.11" `olivo`
;; "3)5 of 3)6 1.3.5.7.9" `lluvia`

;; La intersección entre =lluvia= y =campos=. La hexany =5-1.3.7.9= (cuasi-archytas)
'(["G+88" #{1 3 5}] ["C+59" #{7 9 5}] ["A+92" #{3 9 5}] ["A#+55" #{7 1 5}] ["D+90" #{1 9 5}] ["F+56" #{7 3 5}])
[1 9/8 7/6 21/16 3/2 7/4 2]

;; **** Síntes
;; plasmonic: particules

;; Podemos empezar con el modo en posición menor e ir subiendo hasta hacerlo mayor. Y =campo= "comienze" en muy brillante. 
;; ** Envío del canto-armonía: nuevamente tierra-cosmos-mar =1)4 of 3)6 1.5-3.7.9.11=

;; * Capture MIDI notes
;; 1. Imprime en el REPL los acordes sostenidos, tanto los ratios originales como los usados por las =.kbm= en Surge.
;; 2. Guarda acordes (en el átomo =selections=) cuando se toca la nota MIDI =40= mientras se sostiene un acorde.

(defonce midi-notes (atom {}))
(defn capture-note
  [note]
  (swap! midi-notes assoc note true))

(defn remove-note
  [note]
  (swap! midi-notes dissoc note))

;; **** Captured midi notes processing

(do
  (defn midi-notes->chord
    [subcps midi-notes]
    (let [midi (-> midi-notes sort)
          actual (->> midi
                      (map #(deg->freq subcps 1 (- % 60))))
          octave-reduced (map :ratio (mapcat ratios->scale (partition 1 1 actual)))
          set* (-> octave-reduced set sort)

          ;; mapped to surge's kbm
          surge (map #(/ % 33/32) actual)
          octave-reduced-surge (map :ratio (mapcat ratios->scale (partition 1 1 surge)))]
      {:size (format "%s/%s" (count set*) (count actual))
       :midi midi
       :actual actual
       :set set*
       :actual-surge surge
       :set-surge (-> octave-reduced-surge set sort)
       :octave-reduced octave-reduced
       :octave-reduced-surge octave-reduced-surge}))
  (midi-notes->chord olivo (keys {64 true, 57 true, 62 true, 60 true})))

(defn print-data*
  [current-subcps midi-notes]
  (println)
  (->> midi-notes
       (midi-notes->chord current-subcps)
       pp/pprint))

(def print-data (throttle print-data* 400))

(defonce selections (atom []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize midi watch
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (-> @selections)
  (reset! selections [])

  (midi-in-event
   :midi-input (get-oxygen!)
   :note-on (fn [{:keys [note channel velocity] :as data}]
              (capture-note note))
   :note-off (fn [{:keys [note channel]}]
               (remove-note note)))

  (add-watch midi-notes ::chord
             (fn [_ _ _ data]
               (print-data current-subcps (keys data))
               (when (get data 40)
                 (let [subcps* (:subcps/name (meta current-subcps))]
                   (println (format "Saving chord from %s to `selections` atom" subcps*))
                   (->> (swap! selections conj {:subcps subcps*
                                                :midi-notes (-> data (dissoc 40) keys sort vec)})
                        (last)
                        (println)))))))

;; * Análisis
;; ** Intersets

;;;;;;;;;;;;;;;;;;
;; inter sets
;;;;;;;;;;;;;;;;;;

(defn get-intersets
  [cps-1 cps-2]
  (->> [cps-1 cps-2]
       (mapcat #(map :set %))
       (frequencies)
       (filter #(= 2 (second %)))
       (map first)))

(get-intersets
 olivo
 olivo->lluvia)

;; transitions appear as duplicate sets, so that is good
(->> scale-seq
     (partition 2 1)
     (map #(apply get-intersets %)))

;;;;;;;;;;;;;;;;;;
;; total notes
;;;;;;;;;;;;;;;;;;

(->> scale-seq
     flatten
     (map :set)
     set
     count)
;; => 17

;; ** Armonías capturadas
;; Toma las armonías guardadas (={:subcps :string :midi-notes :num-seq}=) y devuelve información útil

(defn selected-harmony->chord
  [{subcps-name :subcps midi :midi-notes}]
  (midi-notes->chord (subcps subcps-name) midi))

#_(->> (selected-harmony->chord (nth olivo-armonias 1))
       :actual
       #_(map eik-ratios)
       #_(map (comp :set))
       #_(map (comp :class :pitch))
       #_(map #(pitch-class->pr-fingering
                (-> % :pitch :class))))

(defn harmonies-by-id
  "harmonies are the maps with :midi-notes, such as `lluvia-armonias`"
  [armonias]
  (reduce
   (fn [m {:as armonia :keys [id]}]
     (when-not id
       (timbre/warn "`:id` not found"))
     (assoc m id armonia))
   {}
   armonias))

(defn harmony->scale
  "An element from an `x-armonias` vector, such as from `lluvia-armonias`"
  [armonia]
  (->> armonia
       (selected-harmony->chord)
       :octave-reduced
       (map eik-ratios)
       (sort-by :bounded-ratio)))

;; * Trainer

(defn harmony-id->scale
  [id]
  (with-meta (->> (harmonies-by-id olivo-armonias)
                  id
                  harmony->scale)
    {::id id}))

(def olivo-seq
  (->> [[10 :olivo-1]
        [10 :olivo-1.2]
        [7 :sombra-verde-oscuro-1]
        [5 :olivo-1.2]
        [10 :sombra-verde-oscuro-2]
        [5 :olivo-1.2]
        [7 :sombra-verde-oscuro-3]
        [7 :verde-mate-1]
        [7 :verde-mate-2]
        [10 :olivo-1.2]
        [7 :verde-mate-3]
        [7 :verde-mate-4]]
       (map (juxt first (comp harmony-id->scale second)))
       (apply repcat)))

(comment
  (rain.v2/stop)

  (map (juxt :id (comp :size selected-harmony->chord)) campo-armonias)

  (let [scales olivo-seq
        #_[(->> (harmonies-by-id campo-armonias)
                :chord-1
                harmony->scale)]
        dur-ratio 1
        period nil
        last-note (atom nil)]
    (rain.v2/ref-rain
     :id ::trainer
     :durs (fn [_] (* dur-ratio (rand-nth [5 8 10])))
     :on-event
     (rain.v2/on-event
      (let [scale (at-i scales)
            note (rand-nth scale)
            {:keys [degree bounded-ratio set pitch]} note]

        (println #_#_(:class pitch) set (::id (meta scale)))
        (println (pitch-class->pr-fingering
                  (-> note :pitch :class))
                 "\n\n")
        (when @last-note
          (println (format "Intervalo: %s cents | %s"
                           (int (ratio->cents (/ bounded-ratio @last-note)))
                           (/ bounded-ratio @last-note))))
        (println)

        (reset! last-note bounded-ratio)

        (tuning-monitor
         :freq (deg->freq (:scale eik)
                          (/ 440 2)
                          degree
                          :period (or period
                                      (rainseq {0 4
                                                -2 1
                                                -1 2
                                                1 1})))
         :a (rrange 6 10)
         :r (rrange 6 10)
         :pan (rrange -0.5 0.5)
         :amp (o/db->amp (rrange -6 -3))
         :out (bh/bus 1) #_0))))))

