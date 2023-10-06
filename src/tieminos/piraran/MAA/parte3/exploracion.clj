(ns tieminos.piraran.maa.parte3.exploracion
  (:require
   [clojure.string :as str]
   [erv.scale.core :as scale]
   [overtone.midi :as midi]
   [tieminos.core]
   [tieminos.midi.plain-algo-note :as pa :refer [algo-note malgo-note]]
   [tieminos.polydori.analysis.dorian-hexanies
    :refer [dorian-hexanies-in-polydori]]
   [tieminos.polydori.scale :refer [polydori]]
   [tieminos.synths :as s]
   [tieminos.utils :refer [cps->tidal-scale]]
   [time-time.dynacan.players.gen-poly :as gp :refer [on-event ref-rain]]))

(comment
  (do (defn rename-cps-name [cps-name]
        (-> cps-name
            (str/replace #"\)" "oo")
            (str/replace #" "  "x")
            (str/replace #"\."  "i")
            (str/replace #"-"  "t")))
      (rename-cps-name "1)2 1.9.15-7.19"))
  (do
    #dbg
     (defn subcps-map->tidal-scale-record-string
       [subcps-map]
       (->> subcps-map
            (mapv
             (fn [[k v]]
               (format "(\"%s\", %s)"
                       (rename-cps-name (str/join " "
                                                  ((juxt #(nth % 0) #(nth % 3))
                                                   (str/split k #" "))))
                       (-> v
                           cps->tidal-scale
                           vec
                           str
                           (str/replace #" "  ",")))))

            (str/join ",")
            (format "[%s]")
            (spit "resources/polidori.txt")))

    #_(subcps-map->tidal-scale-record-string (-> polydori :subcps)))
  (subcps-map->tidal-scale-record-string (->> polydori-v2 :subcps (take 2)))
  (->> dorian-hexanies-in-polydori
       (map #(->> % ((juxt (comp (fn [fs] (str/join "." fs)) sort :common-factors)
                           (comp (fn [fs] (str/join "." fs)) sort :unique-factors)))
                  (apply format "2)4 of 4)7 %s-%s")
                  rename-cps-name))

       (map-indexed (fn [i v]
                      (list v, (format "dorian%sv%s" (quot i 3) (inc (mod i 3)))))))

  (-> polydori :subcps (get "4)6 of 4)7 1.3.7.15.19.21") :scale
      (->> (map :bounded-ratio)))
  (gp/stop)
  (def scale-seq (-> polydori :subcps keys))
  (def scale-history (atom ["1)3 of 4)7 1.7.15-9.19.21"]))
  (-> @scale-history)
  (defn rand-scale []
    (let [s (rand-nth scale-seq)]
      (swap! scale-history conj s)
      (println s)
      (get-in polydori
              [:subcps s :scale])))
  (rand-scale)
  (tieminos.core/rec "MAA-scales-sampler")
  (overtone.core/recording-stop)
  (let [scale (rand-scale)]
    (ref-rain
     :id :exploration
     :durs [3 2 2]
     :tempo 180
     :ratio 1/4
     :on-event (on-event
                (s/soft-saw
                 :freq (scale/deg->freq scale 200
                                        (at-i [5 1 0 3 -2 2 3 4 0 -1]))
                 :amp (at-i [0.2 0.5 0.3 0.7 0.2]))
                (when (odd? i)
                  (s/soft-saw
                   :freq (scale/deg->freq scale 200
                                          (- (at-i [0 -2 -3])
                                             (at-i [5 1 0 3 -2 2 3 4 0 -1])))
                   :amp (at-i [0.2 0.5 0.3 0.7 0.2])
                   :dcy 2))))))

(comment

  (s/soft-saw (scale/deg->freq
               (:scale polydori)
               440
               0)
              :dcy 10)

  (def sink (midi/midi-out "VirMIDI"))

  (algo-note {:sink sink
              :dur  10
              :note 69
              :vel 80
              :chan 6})
  (malgo-note {:sink sink
               :dur  10
               :vel 80
               :chan 6
               :scale-size 64
               :base-midi-deg 69
               :base-midi-chan 6
               :deg -64}))

