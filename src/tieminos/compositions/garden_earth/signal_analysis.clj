(ns tieminos.compositions.garden-earth.signal-analysis
  (:require
   [tieminos.math.utils :refer [avg]]
   [tieminos.utils :refer [normalize-amp]]))

(defn most-frequent-val [coll]
  (->> coll frequencies
       (sort-by second >)
       first
       first))

(def test-data [{:diff-hz -5.0426025390625,
                 :pitch-path "/receive-pitch-5",
                 :pitch-class "G#+73",
                 :eik-freq 433.125,
                 :eik-freq-ref 866.25,
                 :amp 0.05532864,
                 :original-freq 430.6037,
                 :diff-cents -10.107282553690984,
                 :transp 1/2,
                 :timestamp 1725500324873,
                 :input-bus 20}
                {:diff-hz -7.638916015625,
                 :pitch-path "/receive-pitch-5",
                 :pitch-class "G#+73",
                 :eik-freq 433.125,
                 :eik-freq-ref 866.25,
                 :amp 0.011205906,
                 :original-freq 429.30554,
                 :diff-cents -15.334380361722287,
                 :transp 1/2,
                 :timestamp 1725500324798,
                 :input-bus 20}
                {:diff-hz 6.56182861328125,
                 :pitch-path "/receive-pitch-5",
                 :pitch-class "G#+42",
                 :eik-freq 425.390625,
                 :eik-freq-ref 850.78125,
                 :amp 0.035813615,
                 :original-freq 428.67154,
                 :diff-cents 13.301276507137073,
                 :transp 1/2,
                 :timestamp 1725500324766,
                 :input-bus 20}])

(defn analyze*
  "Will do a frequency/pitch and amp analysis.
  Takes a `data-seq` from `erv-fib-synth.compositions.garden-earth.synths.live-signal/freq-history``
  Frequency is assummed an required to be present an all the `data-seq` maps.


  With regards to frequency/pitch, it will try to get the most frequent
  `pitch-class` in a `freq-history` segment. Might just return a random frequency
  if all pitch-classes in a segment appear an equal number of times.
  This is a heuristic function and might not be accurate enough."
  [data-seq]
  (let [analysis (->> data-seq
                      (reduce (fn [acc {:keys [amp original-freq pitch-class transp]}]
                                (println pitch-class)
                                (-> acc
                                    (update :min-amp min amp)
                                    (update :max-amp max amp)
                                    (update :amps conj amp)
                                    ;; NOTE, it's important to know  that `freq-data` only
                                    ;; contains analysis for sounds that are pitched.
                                    ;; So noise-related data will not be present here.
                                    (update :freqs conj original-freq)
                                    (update :pitch-classes conj pitch-class)
                                    (update-in [:transps pitch-class] conj transp)))

                              {:min-amp 0
                               :max-amp 0
                               :amps ()
                               :freqs ()
                               :pitch-classes ()
                               :transps {}}))
        pitch-class  (most-frequent-val (:pitch-classes analysis))
        transp (-> analysis
                   :transps
                   (get pitch-class)
                   most-frequent-val)]
    (-> analysis
        (dissoc :amps :freqs :pitch-classes :transps)
        (assoc :pitch-class pitch-class
               :transp transp
               :avg-freq (avg (:freqs analysis))
               :avg-amp (avg (:amps analysis))
               :amp-norm-mult (normalize-amp (:max-amp analysis))))))
(analyze* test-data)

;; TODO left here, figure out how to plug it in to the recorder.
(defn analyze
  [freq-history start-time end-time]
  (let [data (->> freq-history
                  (take-while #(>= (:timestamp %) start-time))
                  (drop-while #(> (:timestamp %)  end-time)))]
    (analyze* data)))
