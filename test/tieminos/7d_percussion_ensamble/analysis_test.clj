(ns tieminos.7d-percussion-ensamble.analysis-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tieminos.7d-percussion-ensamble.analysis
    :refer [diatonic-hexanies-by-id
            diatonic-hexanies-intersection-map
            diatonic-hexanies-intersection-summary
            intersect-diatonic-hexanies
            named-diatonic-hexanies]]
   [tieminos.polydori.scale :refer [polydori-v2]]
   [tieminos.utils :refer [map-subscale-degs]]))

(deftest named-diatonic-hexanies-test
  (testing "diatonic hexanies have id and names and are properly ordered"
    (is (= [[:hx1.1 :hx1.1-1.3.9.19*7.15]
            [:hx1.2 :hx1.2-1.3.9.19*7.21]
            [:hx1.3 :hx1.3-1.3.9.19*15.21]
            [:hx2.1 :hx2.1-1.3.9.15*7.21]
            [:hx2.2 :hx2.2-1.3.9.15*7.19]
            [:hx2.3 :hx2.3-1.3.9.15*19.21]
            [:hx3.1 :hx3.1-3.7.19.21*1.15]
            [:hx3.2 :hx3.2-3.7.19.21*1.9]
            [:hx3.3 :hx3.3-3.7.19.21*9.15]
            [:hx4.1 :hx4.1-7.9.19.21*1.15]
            [:hx4.2 :hx4.2-7.9.19.21*1.3]
            [:hx4.3 :hx4.3-7.9.19.21*3.15]
            [:hx5.1 :hx5.1-1.7.9.21*3.15]
            [:hx5.2 :hx5.2-1.7.9.21*15.19]
            [:hx5.3 :hx5.3-1.7.9.21*3.19]
            [:hx6.1 :hx6.1-1.3.19.21*7.15]
            [:hx6.2 :hx6.2-1.3.19.21*7.9]
            [:hx6.3 :hx6.3-1.3.19.21*9.15]
            [:hx7.1 :hx7.1-1.3.7.9*15.21]
            [:hx7.2 :hx7.2-1.3.7.9*15.19]
            [:hx7.3 :hx7.3-1.3.7.9*19.21]]
           (map (juxt :id :full-name)
                named-diatonic-hexanies)))))

(deftest intersect-diatonic-hexanies-test
  (testing "has keys"
    (is (= #{:intersection/sets
             :intersection/pair
             :intersection/count
             :degrees/local
             :degrees/parent}
           (->> (intersect-diatonic-hexanies named-diatonic-hexanies)
                (mapcat keys)
                set)))))

(deftest diatonic-hexanies-intersection-summary-test
  (is (= {:hx3.2
          [[:hx4.2 {:count 4}]
           [:hx6.2 {:count 4}]
           [:hx5.3 {:count 3}]
           [:hx2.2 {:count 2}]
           [:hx2.3 {:count 2}]
           [:hx7.3 {:count 2}]
           [:hx1.2 {:count 1}]
           [:hx2.1 {:count 1}]]
          :hx6.2
          [[:hx3.2 {:count 4}]
           [:hx1.2 {:count 3}]
           [:hx7.3 {:count 3}]
           [:hx2.1 {:count 2}]
           [:hx2.2 {:count 2}]
           [:hx2.3 {:count 2}]
           [:hx4.2 {:count 2}]
           [:hx5.3 {:count 2}]]
          :hx3.1
          [[:hx1.1 {:count 4}]
           [:hx4.1 {:count 4}]
           [:hx6.1 {:count 4}]
           [:hx7.2 {:count 3}]
           [:hx1.3 {:count 2}]
           [:hx2.2 {:count 2}]
           [:hx5.1 {:count 2}]
           [:hx5.2 {:count 2}]
           [:hx7.1 {:count 2}]
           [:hx2.1 {:count 1}]
           [:hx2.3 {:count 1}]
           [:hx4.3 {:count 1}]]
          :hx2.3
          [[:hx2.2 {:count 4}]
           [:hx1.3 {:count 3}]
           [:hx5.2 {:count 3}]
           [:hx5.3 {:count 3}]
           [:hx7.3 {:count 3}]
           [:hx1.1 {:count 2}]
           [:hx3.2 {:count 2}]
           [:hx3.3 {:count 2}]
           [:hx4.3 {:count 2}]
           [:hx6.2 {:count 2}]
           [:hx7.2 {:count 2}]
           [:hx3.1 {:count 1}]
           [:hx4.1 {:count 1}]
           [:hx4.2 {:count 1}]
           [:hx6.1 {:count 1}]
           [:hx6.3 {:count 1}]]
          :hx7.2
          [[:hx5.2 {:count 4}]
           [:hx1.1 {:count 3}]
           [:hx2.2 {:count 3}]
           [:hx3.1 {:count 3}]
           [:hx4.1 {:count 3}]
           [:hx4.3 {:count 3}]
           [:hx1.3 {:count 2}]
           [:hx2.3 {:count 2}]
           [:hx3.3 {:count 2}]
           [:hx6.1 {:count 2}]
           [:hx6.3 {:count 2}]]
          :hx4.2
          [[:hx3.2 {:count 4}]
           [:hx5.3 {:count 3}]
           [:hx2.2 {:count 2}]
           [:hx6.2 {:count 2}]
           [:hx1.2 {:count 1}]
           [:hx2.1 {:count 1}]
           [:hx2.3 {:count 1}]
           [:hx7.3 {:count 1}]]
          :hx5.3
          [[:hx7.3 {:count 4}]
           [:hx2.2 {:count 3}]
           [:hx2.3 {:count 3}]
           [:hx3.2 {:count 3}]
           [:hx4.2 {:count 3}]
           [:hx6.2 {:count 2}]
           [:hx1.2 {:count 1}]]
          :hx4.1
          [[:hx1.1 {:count 4}]
           [:hx3.1 {:count 4}]
           [:hx1.3 {:count 3}]
           [:hx5.2 {:count 3}]
           [:hx6.1 {:count 3}]
           [:hx7.1 {:count 3}]
           [:hx7.2 {:count 3}]
           [:hx2.2 {:count 2}]
           [:hx4.3 {:count 2}]
           [:hx5.1 {:count 2}]
           [:hx6.3 {:count 2}]
           [:hx2.1 {:count 1}]
           [:hx2.3 {:count 1}]
           [:hx3.3 {:count 1}]]
          :hx5.1
          [[:hx7.1 {:count 4}]
           [:hx1.1 {:count 3}]
           [:hx1.3 {:count 3}]
           [:hx4.3 {:count 3}]
           [:hx6.3 {:count 3}]
           [:hx3.1 {:count 2}]
           [:hx3.3 {:count 2}]
           [:hx4.1 {:count 2}]
           [:hx6.1 {:count 2}]
           [:hx2.1 {:count 1}]]
          :hx6.3
          [[:hx3.3 {:count 4}]
           [:hx1.3 {:count 3}]
           [:hx4.3 {:count 3}]
           [:hx5.1 {:count 3}]
           [:hx4.1 {:count 2}]
           [:hx5.2 {:count 2}]
           [:hx7.1 {:count 2}]
           [:hx7.2 {:count 2}]
           [:hx1.1 {:count 1}]
           [:hx2.3 {:count 1}]]
          :hx2.1
          [[:hx1.2 {:count 3}]
           [:hx7.1 {:count 3}]
           [:hx6.1 {:count 2}]
           [:hx6.2 {:count 2}]
           [:hx3.1 {:count 1}]
           [:hx3.2 {:count 1}]
           [:hx3.3 {:count 1}]
           [:hx4.1 {:count 1}]
           [:hx4.2 {:count 1}]
           [:hx4.3 {:count 1}]
           [:hx5.1 {:count 1}]]
          :hx5.2
          [[:hx7.2 {:count 4}]
           [:hx1.1 {:count 3}]
           [:hx1.3 {:count 3}]
           [:hx2.2 {:count 3}]
           [:hx2.3 {:count 3}]
           [:hx4.1 {:count 3}]
           [:hx6.1 {:count 3}]
           [:hx3.1 {:count 2}]
           [:hx3.3 {:count 2}]
           [:hx4.3 {:count 2}]
           [:hx6.3 {:count 2}]]
          :hx7.1
          [[:hx5.1 {:count 4}]
           [:hx1.3 {:count 3}]
           [:hx2.1 {:count 3}]
           [:hx3.3 {:count 3}]
           [:hx4.1 {:count 3}]
           [:hx4.3 {:count 3}]
           [:hx1.1 {:count 2}]
           [:hx3.1 {:count 2}]
           [:hx6.1 {:count 2}]
           [:hx6.3 {:count 2}]]
          :hx2.2
          [[:hx2.3 {:count 4}]
           [:hx1.1 {:count 3}]
           [:hx5.2 {:count 3}]
           [:hx5.3 {:count 3}]
           [:hx7.2 {:count 3}]
           [:hx1.3 {:count 2}]
           [:hx3.1 {:count 2}]
           [:hx3.2 {:count 2}]
           [:hx4.1 {:count 2}]
           [:hx4.2 {:count 2}]
           [:hx4.3 {:count 2}]
           [:hx6.1 {:count 2}]
           [:hx6.2 {:count 2}]
           [:hx7.3 {:count 2}]
           [:hx3.3 {:count 1}]]
          :hx3.3
          [[:hx1.3 {:count 4}]
           [:hx4.3 {:count 4}]
           [:hx6.3 {:count 4}]
           [:hx7.1 {:count 3}]
           [:hx1.1 {:count 2}]
           [:hx2.3 {:count 2}]
           [:hx5.1 {:count 2}]
           [:hx5.2 {:count 2}]
           [:hx7.2 {:count 2}]
           [:hx2.1 {:count 1}]
           [:hx2.2 {:count 1}]
           [:hx4.1 {:count 1}]]
          :hx4.3
          [[:hx1.3 {:count 4}]
           [:hx3.3 {:count 4}]
           [:hx1.1 {:count 3}]
           [:hx5.1 {:count 3}]
           [:hx6.3 {:count 3}]
           [:hx7.1 {:count 3}]
           [:hx7.2 {:count 3}]
           [:hx2.2 {:count 2}]
           [:hx2.3 {:count 2}]
           [:hx4.1 {:count 2}]
           [:hx5.2 {:count 2}]
           [:hx6.1 {:count 2}]
           [:hx2.1 {:count 1}]
           [:hx3.1 {:count 1}]]
          :hx6.1
          [[:hx3.1 {:count 4}]
           [:hx1.1 {:count 3}]
           [:hx4.1 {:count 3}]
           [:hx5.2 {:count 3}]
           [:hx2.1 {:count 2}]
           [:hx2.2 {:count 2}]
           [:hx4.3 {:count 2}]
           [:hx5.1 {:count 2}]
           [:hx7.1 {:count 2}]
           [:hx7.2 {:count 2}]
           [:hx1.3 {:count 1}]
           [:hx2.3 {:count 1}]]
          :hx1.2
          [[:hx2.1 {:count 3}]
           [:hx6.2 {:count 3}]
           [:hx7.3 {:count 3}]
           [:hx3.2 {:count 1}]
           [:hx4.2 {:count 1}]
           [:hx5.3 {:count 1}]]
          :hx1.1
          [[:hx1.3 {:count 4}]
           [:hx3.1 {:count 4}]
           [:hx4.1 {:count 4}]
           [:hx2.2 {:count 3}]
           [:hx4.3 {:count 3}]
           [:hx5.1 {:count 3}]
           [:hx5.2 {:count 3}]
           [:hx6.1 {:count 3}]
           [:hx7.2 {:count 3}]
           [:hx2.3 {:count 2}]
           [:hx3.3 {:count 2}]
           [:hx7.1 {:count 2}]
           [:hx6.3 {:count 1}]]
          :hx7.3
          [[:hx5.3 {:count 4}]
           [:hx1.2 {:count 3}]
           [:hx2.3 {:count 3}]
           [:hx6.2 {:count 3}]
           [:hx2.2 {:count 2}]
           [:hx3.2 {:count 2}]
           [:hx4.2 {:count 1}]]
          :hx1.3
          [[:hx1.1 {:count 4}]
           [:hx3.3 {:count 4}]
           [:hx4.3 {:count 4}]
           [:hx2.3 {:count 3}]
           [:hx4.1 {:count 3}]
           [:hx5.1 {:count 3}]
           [:hx5.2 {:count 3}]
           [:hx6.3 {:count 3}]
           [:hx7.1 {:count 3}]
           [:hx2.2 {:count 2}]
           [:hx3.1 {:count 2}]
           [:hx7.2 {:count 2}]
           [:hx6.1 {:count 1}]]}
         diatonic-hexanies-intersection-summary)))

(deftest common-degrees-between-hexanies-test
  (testing "starting with a hexanie, i.e. `:hx3.2` one can select another related hexany and find that the common notes point to the same degrees in the parent scale (`polydori-v2`)"
    (let [ref-hexany :hx3.2
          related-hexany :hx4.2
          local-degrees-map (-> diatonic-hexanies-intersection-map
                                ref-hexany
                                related-hexany
                                :degrees/local)
          get-degrees (fn [hx-id] (map (fn [degree]
                                         (map-subscale-degs (count (:scale polydori-v2))
                                                            (-> diatonic-hexanies-by-id hx-id :degrees)
                                                            degree))
                                       (local-degrees-map hx-id)))]
      (is (= '(8 13 14 24)
             (get-degrees ref-hexany)
             (get-degrees related-hexany)))))
  (testing "one can reverse the `ref-hexany` and `related-hexany` and get the same results"
    (let [ref-hexany :hx4.2
          related-hexany :hx3.2
          local-degrees-map (-> diatonic-hexanies-intersection-map
                                ref-hexany
                                related-hexany
                                :degrees/local)
          get-degrees (fn [hx-id] (map (fn [degree]
                                         (map-subscale-degs (count (:scale polydori-v2))
                                                            (-> diatonic-hexanies-by-id hx-id :degrees)
                                                            degree))
                                       (local-degrees-map hx-id)))]
      (is (= '(8 13 14 24)
             (get-degrees ref-hexany)
             (get-degrees related-hexany))))))
