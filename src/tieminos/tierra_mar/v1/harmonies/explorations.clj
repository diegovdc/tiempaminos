(ns tieminos.tierra-mar.v1.harmonies.explorations
  (:require
   [clojure.set :as set]
   [erv.cps.core :refer [+all-subcps]]
   [erv.scale.core :refer [+names]]
   [erv.scale.scl :as scl]
   [erv.utils.conversions :refer [ratio->cents]]
   [erv.utils.ratios :refer [ratios->scale]]
   [tieminos.compositions.garden-earth.analysis :refer [find-subcps-intersections
                                                        find-supersets]]
   [tieminos.compositions.garden-earth.base :refer [base-freq eik eik-sets
                                                    subcps]]
   [tieminos.osc.surge :as surge]
   [tieminos.utils :refer [wrap-at]]))

(defn +degrees [scale]
  (map (comp :degree eik-sets :set)
       scale))

(comment
  ;; Make scl and kbm files compatible with the current tuning used by the flute
  ;;
  (-> eik)
  ;; make scl file that maps to the base freq 440 (which doesn't appear in the scale
  (-> eik :scale first :bounded-ratio) ;; => 33/32
  (def scl-base-freq (float (* base-freq 33/32))) ;; => 453.75 - MTS-ESP should set this as the freq
  ;; eik scale with the first note as 1/1
  (def scl-eik (->> eik :scale (map (fn [note]
                                      (-> note
                                          (update :bounded-ratio / 33/32)
                                          (assoc-in [:pitch :base-freq] scl-base-freq))))))
  ;; ensure that the names are still correct, that is that the pitches haven't changed
  (= scl-eik (+names 453.75 scl-eik))

  (scl/make-scl-file {:meta {:scl/name "Wilson's 1.3.5.7.9.11 Eikosany"
                             :scl/description "Used for the Garden Earth project. The flute tunes the 1/1 to 453.75 (* 33/32 440)."}
                      :scale scl-eik})
  (def eik-data {:meta {:scl/name "Wilson's 1.3.5.7.9.11 Eikosany"
                        :scl/description "Used for the Garden Earth project. The flute tunes the 1/1 to 453.75 (* 33/32 440)."}
                 :scale scl-eik})
  (scl/spit-file
   "/Users/diego/Music/tunings/wilson-1-3-5-7-9-11-eikosany_garden-earth/eik.scl"
   eik-data)

  ;; kbm files
  (let [cps-size 6]
    (let [data (->> eik :subcps
                    (filter (fn [[_ {:keys [meta]}]]
                              (= cps-size (:size meta))))
                    (map (fn [[k data]]
                           [k (map (comp :degree eik-sets :set) (:scale data))])))]

      (doseq [[k degs] data]
        (scl/spit-kbm {:filepath
                       (format "/Users/diego/Music/tunings/wilson-1-3-5-7-9-11-eikosany_garden-earth/%s.kbm" k)
                       :scale-data eik-data
                       :middle-note 60
                       :middle-note-freq (/ scl-base-freq scl-base-freq)
                       :comments? false
                       :degrees degs}))))

  (def kbm-data
    (let [cps-size 6]
      (->> eik :subcps
           (filter (fn [[_ {:keys [meta]}]]
                     (= cps-size (:size meta))))
           (map (fn [[k data]]
                  [k (+degrees (:scale data))])))))

  (+degrees (subcps "2)5 of 3)6 5-1.3.7.9.11"))

  (defonce kbm-index (atom 0))
  (first (wrap-at @kbm-index kbm-data))
  (surge/init)
  ;; interesting:
  ;; 2)4 of 3)6 1-3.5.7.9
  ;; 2)4 of 3)6 11-3.5.7.9
  ;; 2)4 of 3)6 9-1.3.7.11
  ;; 2)4 of 3)6 3-1.5.7.11
  ;; 2)4 of 3)6 11-1.3.5.7 - no me acaba de convencer
  ;; 2)4 of 3)6 1-5.7.9.11
  ;; 2)4 of 3)6 5-1.3.9.11 - triste
  ;; 2)4 of 3)6 7-3.5.9.11
  ;; 2)4 of 3)6 5-1.3.7.9 - brillante, bello
  (do
    (swap! kbm-index dec)
    (surge/set-kbm
     (let [[name* degs] (wrap-at @kbm-index kbm-data)]
       {:path
        "/Users/diego/Music/tunings/wilson-1-3-5-7-9-11-eikosany_garden-earth"
        :kbm-name name*
        :scale-data eik-data
        :middle-note 60
        :middle-note-freq (/ scl-base-freq 2)
        :comments? false
        :degrees degs}))))

(comment
  ;; candidata para `campos`
  (-> eik :subcps
      (get "2)4 of 3)6 5-1.3.9.11")
      (+all-subcps)
      :subcps
      #_(->> (filter (fn [[_ d]]
                       (-> d :meta :size (= 4)))))
      keys
      sort)
  ;; =>
  '("3)4 of 3)5 1.3.5.9" ;; conocida (grabada)
    "3)4 of 3)5 3.5.9.11" ;; desierto - explorada conjuntamente con la de abajo - fue difícil percibir su caracter el día que las toqué, pero escuchando la grabación sugiere un espacio vacío y desierto
    "3)4 of 3)5 1.3.9.11" ;; difícil, quizá muy neutro o aún no le encuentro mucho
    "3)4 of 3)5 1.5.9.11"
    "3)4 of 3)5 1.3.5.11" ;; pro
    )
  (->> "2)4 of 3)6 5-1.3.9.11"
       subcps
       (map :set)
       set
       find-supersets
       keys)
  ;; "2)5 of 3)6 5-1.3.7.9.11" - puede ser interesante, contiene el subset "1)4 of 3)6 1.5-3.7.9.11"

  (find-subcps-intersections
   "2)4 of 3)6 5-1.3.9.11"
   #{"1)4"})
  ;;  "1)4 of 3)6 1.5-3.7.9.11" 3 notas de interseccion

  ;; `campos`
  (find-subcps-intersections
   "2)5 of 3)6 5-1.3.7.9.11"
   #{"3)5"})
  ;; "3)5 of 3)6 3.5.7.9.11": muy bonito, misterioso (error en eik22)
  ;; "3)5 of 3)6 1.5.7.9.11" funciona bien pero la interseccion es muy 6edo
  ;; "3)5 of 3)6 1.3.5.9.11" `olivo`
  ;; "3)5 of 3)6 1.3.5.7.9" `lluvia` (?)
  ;; interseccion luuvia-campos: 
    ;; '(["G+88" #{1 3 5}] ["C+59" #{7 9 5}] ["A+92" #{3 9 5}] ["A#+55" #{7 1 5}] ["D+90" #{1 9 5}] ["F+56" #{7 3 5}])  
    ;; [1 9/8 7/6 21/16 3/2 7/4 2]: empezar en ese modo y poco a poco subir para hacerlo mayor
  (->> ["3)5 of 3)6 1.3.5.9.11" "2)5 of 3)6 5-1.3.7.9.11"]
       (map (fn [cps] (->> (find-subcps-intersections
                            cps
                            #{"3)5" "2)5"})
                           (map first)
                           set)))))
(comment
  (surge/set-kbm
   (let [name* #_"2)5 of 3)6 5-1.3.7.9.11"
         degs (+degrees (subcps name*))]
     {:path
      "/Users/diego/Music/tunings/wilson-1-3-5-7-9-11-eikosany_garden-earth"
      :kbm-name name*
      :scale-data eik-data
      :middle-note 60
      :middle-note-freq (/ scl-base-freq 2)
      :comments? false
      :degrees degs}))

  ;; "2)5 of 3)6 5-1.3.7.9.11" - acorde birllante
  ;; oct+1: Ab C F Ab' C' Eb' Ab'' Bb'' (B'' A'')
  ;; oct+1: Ab C F Ab' B' D' - bonito juego entre C-Eb y B-D... B'-D'-A'' funciona bien, contraste, podría pertenecer a uno de los lados del campo

;; un acorde bonito en "3)5 of 3)6 3.5.7.9.11":... pero que es de eik22 :( (mi error, pero es una hayazgo bonito)
  ;;           + oct         +oct
  ;; F      B      D      F      Ab     B
  ;; 10     3      8      13     1      6
  ;; 15/11  12/11  14/11  3/2    45/44  105/88
  )

(comment
  (surge/set-kbm
   (let [name* "join 2)5 of 3)6 5-1.3.7.9.11 -  3)5 of 3)6 3.5.7.9.11"  #_"3)5 of 3)6 3.5.7.9.11"
         degs (-> (concat (+degrees (subcps "2)5 of 3)6 5-1.3.7.9.11"))
                          (+degrees (subcps "3)5 of 3)6 3.5.7.9.11")))
                  set
                  sort)]
     {:path
      "/Users/diego/Music/tunings/wilson-1-3-5-7-9-11-eikosany_garden-earth"
      :kbm-name name*
      :scale-data eik-data
      :middle-note 60
      :middle-note-freq (/ scl-base-freq 2)
      :comments? false
      :degrees degs})))

(comment
  (surge/set-kbm
   (let [name* "interseccion 2)5 5-1.3.7.9.11 - 3)5 1.3.5.7.9"
         degs (->> '(["G+88" #{1 3 5}]
                     ["C+59" #{7 9 5}]
                     ["A+92" #{3 9 5}]
                     ["A#+55" #{7 1 5}]
                     ["D+90" #{1 9 5}]
                     ["F+56" #{7 3 5}])
                   (map second)
                   (map eik-sets)
                   (map :degree))]
     {:path
      "/Users/diego/Music/tunings/wilson-1-3-5-7-9-11-eikosany_garden-earth"
      :kbm-name name*
      :scale-data eik-data
      :middle-note 60
      :middle-note-freq (/ scl-base-freq 2)
      :comments? false
      :degrees degs}))

  (->> '(["G+88" #{1 3 5}]
         ["C+59" #{7 9 5}]
         ["A+92" #{3 9 5}]
         ["A#+55" #{7 1 5}]
         ["D+90" #{1 9 5}]
         ["F+56" #{7 3 5}])
       (map second)
       (map eik-sets)
       (map :ratio)
       (map #(/ % 15 #_33/32))
       (ratios->scale)
       (mapv :ratio)
       (#(conj % 2))
       #_(map ratio->cents)
       #_(partition 2 1)
       #_(map (fn [[a b]] (- b a))))
  (/ 1200.0)
  (/ 35/22 20/11)
  (ratio->cents 22/21))

(comment

  ;; también muy bonita (desafortunadamente en eik22- mi error)
  (surge/set-kbm
   (let [name* "interseccion 2)5 5-1.3.7.9.11 - 3)5 3.5.7.9.11"
         degs (->> '(["G+88" #{1 3 5}]
                     ["G#+42" #{11 9 5}]
                     ["C#+40" #{3 11 5}]
                     ["F#+38" #{1 11 5}]
                     ["A+92" #{3 9 5}]
                     ["D+90" #{1 9 5}])
                   (map second)
                   (map eik-sets)
                   (map :degree))]
     {:path
      "/Users/diego/Music/tunings/wilson-1-3-5-7-9-11-eikosany_garden-earth"
      :kbm-name name*
      :scale-data eik-data
      :middle-note 60
      :middle-note-freq (/ scl-base-freq 2)
      :comments? false
      :degrees degs}))

  (->> '(["G+88" #{1 3 5}]
         ["G#+42" #{11 9 5}]
         ["C#+40" #{3 11 5}]
         ["F#+38" #{1 11 5}]
         ["A+92" #{3 9 5}]
         ["D+90" #{1 9 5}])
       (map second)
       (map eik-sets)
       (map :ratio)
       (map #(/ % 15 #_33/32))
       (ratios->scale)
       (mapv :ratio)
       (#(conj % 2))
       (map ratio->cents)
       (partition 2 1)
       (map (fn [[a b]] (- b a))))
  (/ 1200.0)
  (ratio->cents 22/21))
