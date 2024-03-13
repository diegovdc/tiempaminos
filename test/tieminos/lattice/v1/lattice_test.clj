(ns tieminos.lattice.v1.lattice-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tieminos.lattice.v1.lattice :refer [add-played-ratio*
                                        remove-played-ratio*]]))

(deftest add-played-ratio*-test
  (is (= {:played-notes {1 [{:ratio 1 :color "#112233" :index 0}
                            {:ratio 1 :color "#552233" :index 1}]
                         3/2 [{:ratio 3/2 :color "#333333" :index 0}]}}
         (add-played-ratio*
           {:played-notes {1 [{:ratio 1 :color "#112233" :index 0}]
                           3/2 [{:ratio 3/2 :color "#333333" :index 0}]}}
           {:ratio 1 :color "#552233"})))
  (is (= {:played-notes {1 [{:ratio 1 :color "#112233" :index 0}]
                         3/2 [{:ratio 3/2 :color "#333333" :index 0}]
                         7/4 [{:ratio 7/4 :color "#552233" :index 0}]}}
         (add-played-ratio*
           {:played-notes {1 [{:ratio 1 :color "#112233" :index 0}]
                           3/2 [{:ratio 3/2 :color "#333333" :index 0}]}}
           {:ratio 7/4 :color "#552233"}))))

(deftest remove-played-ratio*-test
  (testing "works a group-id"
    (is (= {:played-notes {1 [{:ratio 1 :color "#112233" :index 0 :group-id 0}]
                           3/2 [{:ratio 3/2 :color "#333333" :index 0 :group-id 0}]}}
           (remove-played-ratio*
             {:played-notes {1 [{:ratio 1 :color "#112233" :index 0 :group-id 0}
                                {:ratio 1 :color "#552233" :index 1 :group-id 1}]
                             3/2 [{:ratio 3/2 :color "#333333" :index 0 :group-id 0}]}}
             {:ratio 1 :group-id 1}))))
  (testing "works without a group-id"
    (is (= {:played-notes {1 []
                           3/2 [{:ratio 3/2 :color "#333333" :index 0}]}}
           (remove-played-ratio*
             {:played-notes {1 [{:ratio 1 :color "#112233" :index 0}]
                             3/2 [{:ratio 3/2 :color "#333333" :index 0}]}}
             {:ratio 1})))))
