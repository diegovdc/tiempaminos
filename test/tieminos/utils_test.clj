(ns tieminos.utils-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tieminos.utils :refer [map-subscale-degs wrap-at]]))

(deftest wrap-at-test
  (testing "Get element within bounds"
    (is (= 0 (wrap-at 0 [0 1 2 3])))
    (is (= 3 (wrap-at 3 [0 1 2 3]))))

  (testing "Get element above bounds"
    (is (= 0 (wrap-at 4 [0 1 2 3])))
    (is (= 3 (wrap-at 7 [0 1 2 3]))))

  (testing "Get element below bounds"
    (is (= [3 2 1 0 3]
           (map #(wrap-at % [0 1 2 3])
                [-1 -2 -3 -4 -5])))))

(deftest map-subscale-degs-test
  (testing "Will properly wrap down the scale"
    (is (= [-22 -25 -35]
           (mapv #(map-subscale-degs 29
                                     [4 7 12 16 22 23]
                                     %)
                 [-5 -6 -7]))))
  (testing "Works with negative degrees in the subscale (useful for modal work)"
    (is (= [-1 7 28]
           (mapv #(map-subscale-degs 29
                                     [-1 7 12 16 22 23]
                                     %)
                 [0 1 6])))
    (is (= [-1 -6 -30]
           (mapv #(map-subscale-degs 29
                                     [-1 7 12 16 22 23]
                                     %)
                 [0 -1 -6]))))
  (testing "Resulting degrees are properly ordered"
    (is (true? (apply >
                      (mapv
                       #(map-subscale-degs 29
                                           [4 7 12 16 22 23]
                                           %)
                       (range 60 -60 -1)))))
    (is (true? (apply >
                      (mapv
                       #(map-subscale-degs 29
                                           [-4 7 12 16 22 23]
                                           %)
                       (range 60 -60 -1)))))
    (is (true? (apply <
                      (mapv
                       #(map-subscale-degs 29
                                           [4 7 12 16 22 23]
                                           %)
                       (range -60 60 1)))))
    (is (true? (apply <
                      (mapv
                       #(map-subscale-degs 29
                                           [-4 7 12 16 22 23]
                                           %)
                       (range -60 60 1)))))))
