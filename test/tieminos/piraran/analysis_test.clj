(ns tieminos.piraran.analysis-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tieminos.polydori.analysis.dorian-hexanies
    :refer
    [dorian-hexanies-in-polydori]]))

(deftest dorian-hexanies-in-polydori-test
  (testing "Each `:hexany-note-factors` group should have 3 haxanies in there"
    (is (= [[[#{1 9} #{1 19} #{19 9} #{1 3} #{3 9} #{3 19}] 3]
            [[#{15 9} #{1 9} #{15 3} #{1 3} #{3 9} #{1 15}] 3]
            [[#{7 19} #{7 21} #{7 3} #{21 19} #{3 19} #{21 3}] 3]
            [[#{7 19} #{7 21} #{19 9} #{21 9} #{21 19} #{7 9}] 3]
            [[#{1 9} #{7 21} #{1 21} #{21 9} #{7 1} #{7 9}] 3]
            [[#{1 19} #{1 21} #{1 3} #{21 19} #{3 19} #{21 3}] 3]
            [[#{1 9} #{7 3} #{1 3} #{3 9} #{7 1} #{7 9}] 3]]
           (->> dorian-hexanies-in-polydori
                (group-by :hexany-note-factors)
                (map (juxt first (comp count second))))))))
