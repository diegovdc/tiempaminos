(ns tieminos.compositions.7D-percusion-ensamble.base-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.utils.core :refer [period-reduce]]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [deg->freq
                                                             freq->out]]
   [tieminos.polydori.analysis.dorian-hexanies :refer [dorian-hexanies-in-polydori-2]]))

(deftest freq->out-test
  (let [chan-map {200 0
                  800 1
                  2000 2}]
    (is (= 0 (freq->out chan-map 50)))
    (is (= 0 (freq->out chan-map 200)))
    (is (= 1 (freq->out chan-map 201)))
    (is (= 1 (freq->out chan-map 800)))
    (is (= 2 (freq->out chan-map 801)))
    (is (= 2 (freq->out chan-map 1999)))
    (is (= 2 (freq->out chan-map 2000)))
    (is (= 2 (freq->out chan-map 5000)))))

(deftest deg->freq-test
  (testing "`:original` and `:modal` play the same notes on the same degrees, but the `:original` note may be 1 octave above the `:modal` note."
    (is (every? true?
                (mapcat (fn [degree]
                          (map (fn [scale-index]
                                 (let [o (deg->freq :base-freq 1 :scale scale-index :degree degree
                                                    :type :original)
                                       m (deg->freq :base-freq 1 :scale scale-index :degree degree
                                                    :type :modal)]
                                   (or (= (/ o 2) m)
                                       (= o m))))
                               (range (count dorian-hexanies-in-polydori-2))))
                        (range -60 60)))))
  (testing "`:original` and  `:modal` play the same notes as `:sorted`, but perhaps not inn the same order (depending on the `scale-index`). The `frequencies` of every note is 3 (disregarding the octave) as it is played once in each `:type`."
    (is (every?
          #(= % [3 3 3 3 3 3])
          (->> (range (count dorian-hexanies-in-polydori-2))
               (map
                 (fn [scale-index]
                   (->> (range 6)
                        (mapcat (fn [degree]
                                  (let [o (deg->freq :base-freq 1 :scale scale-index :degree degree
                                                     :type :original)
                                        m (deg->freq :base-freq 1 :scale scale-index :degree degree
                                                     :type :modal)
                                        s (deg->freq :base-freq 1 :scale scale-index :degree degree
                                                     :type :sorted)]
                                    (map period-reduce [o m s]))))
                        (frequencies)
                        vals))))))))
