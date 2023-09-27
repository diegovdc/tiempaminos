(ns tieminos.lumatone.coord-system-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tieminos.lumatone.coord-system
    :refer
    [add-coords
     board-keys
     coord->board-key
     coord->scale-degree
     get-board-xy-intervals
     wcoord->lcoord]]))

(deftest board-keys-test
  (testing "All 56 keys in the board are present"
    (is (= (range 1 57) (sort (keys (board-keys 0))))))
  (testing "All coords are unique"
    (is (= 56 (count (set (vals (board-keys 0)))))))
  (testing "Boards' first keys"
    (is (= [0 10] ((board-keys 0) 1)))
    (is (= [5 8] ((board-keys 1) 1)))
    (is (= [10 6] ((board-keys 2) 1)))
    (is (= [15 4] ((board-keys 3) 1)))
    (is (= [20 2] ((board-keys 4) 1)))))

(deftest coord->board-key-test
  (testing "There are as many coords as keys on the lumatone"
    (is (= (* 5 56) (count coord->board-key)))))

(deftest wcoord->lcoord-test
  (is (= [5 -7] (wcoord->lcoord [7 5]))))

(deftest add-coords-test
  (is (= [6 -6] (add-coords [1 1] [5 -7]))))

(deftest get-board-xy-intervals-test
  (testing "12EDO Wilson-Bosanquet intervals"
    (is (= {:x 2 :y -1} (get-board-xy-intervals 7 [3 -1] 12 [5 -2])))))

(deftest coord->scale-degree-test
  (testing "12EDO"
    (let [xy-intervals {:x 2 :y -1}]
      (testing "C"
        (is (= 0 (coord->scale-degree xy-intervals [0 0])))
        (is (= 12 (coord->scale-degree xy-intervals [5 -2])))
        (is (=  12 (coord->scale-degree xy-intervals [6 0]))))
      (testing "D"
        (is (= 2 (coord->scale-degree xy-intervals [1 0]))))
      (testing "G"
        (is (= 7 (coord->scale-degree xy-intervals [3 -1])))))))

