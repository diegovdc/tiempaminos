(ns tieminos.compositions.7D-percusion-ensamble.base-test
  (:require
   [clojure.test :refer [deftest is]]
   [tieminos.compositions.7D-percusion-ensamble.base :refer [freq->out]]))

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
