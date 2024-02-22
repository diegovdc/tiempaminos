(ns tieminos.lumatone.utils-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tieminos.lumatone.utils :refer [multichan-mapper]]))

(deftest multichan-mapper-test
  (testing "Basic transpositions"
    (is (= 0
           (multichan-mapper
            {:period/size 12}
            {:channel 1 :note 0})))
    (is (= 12
           (multichan-mapper
            {:period/size 12}
            {:channel 2 :note 0})))
    (is (= 36
           (multichan-mapper
            {:period/size 12}
            {:channel 2 :note 24})))
    (is (= 31
           (multichan-mapper
            {:period/size 31}
            {:channel 2 :note 0})))
    (is (= 62
           (multichan-mapper
            {:period/size 31}
            {:channel 3 :note 0}))))
  (testing "offset"
    (is (= 24
           (multichan-mapper
            {:period/size 12 :offset -12}
            {:channel 2 :note 24})))))
