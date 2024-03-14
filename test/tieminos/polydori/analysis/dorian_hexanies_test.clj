(ns tieminos.polydori.analysis.dorian-hexanies-test
  (:require
   [tieminos.polydori.analysis.dorian-hexanies :as dorian-hexanies :refer [dorian-hexanies-in-polydori]]
   [clojure.test :refer [deftest is testing]]
   [tieminos.polydori.scale :refer [polydori-v2]]
   [tieminos.utils :as utils]))

(deftest modal-sort-test
  (testing "Will transpose in descendingly the mode so that all degrees are in ascending order"
    (is (= [-10 -8 -4 2 6 9]
           (#'dorian-hexanies/modal-sort 29 [19 21 25 2 6 9]))))
  (testing "Resulting ratios are equal between the original mode and the sorted mode"
    (let [check-modal-sort-ratio-equality
          (fn [degrees]
            (=
             (map (fn [deg]
                    (:bounded-ratio (utils/wrap-at deg (:scale polydori-v2))))
                  degrees)
             (map (fn [deg]
                    (:bounded-ratio (utils/wrap-at deg (:scale polydori-v2))))
                  (#'dorian-hexanies/modal-sort 29 degrees))))]
      (is (true? (check-modal-sort-ratio-equality [19 21 25 2 6 9])))
      (is (every? true? (map #(check-modal-sort-ratio-equality %)
                             (map :degrees dorian-hexanies-in-polydori)))))))
