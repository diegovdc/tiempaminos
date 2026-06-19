(ns lumatone.coord-system-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [lumatone.coord-system
    :refer
    [add-coords board-keys coord->board-key coord->scale-degree
     get-board-xy-intervals midi-chan-tranpose wcoord->lcoord]]))

(deftest board-keys-test
  (testing "All 56 keys in the board are present"
    (is (= (range 0 56) (sort (keys (board-keys 0))))))
  (testing "All coords are unique"
    (is (= 56 (count (set (vals (board-keys 0)))))))
  (testing "Boards' first keys"
    (is (= [1 10] ((board-keys 0) 1)))
    (is (= [6 8] ((board-keys 1) 1)))
    (is (= [11 6] ((board-keys 2) 1)))
    (is (= [16 4] ((board-keys 3) 1)))
    (is (= [21 2] ((board-keys 4) 1)))))

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

(deftest midi-chan-tranpose-test
  (is (= [{:key 125, :chan 1}
          {:key 126, :chan 1}
          {:key 127, :chan 1}
          {:key 97, :chan 2}
          {:key 98, :chan 2}]
         (mapv #(midi-chan-tranpose 31 (+  % 125))
               (range 5))))
  (is (= [{:key 126, :chan 2}
          {:key 96, :chan 3} ;; 96 + 31 = 127
          {:key 97, :chan 3}
          {:key 98, :chan 3}
          {:key 99, :chan 3}]
         (mapv #(midi-chan-tranpose 31 (+ 31 % 126))
               (range 5))))
  (testing "sanity check"
    (let [period (+ 20 (rand-int 30))
          res (->> (range 500)
                   (mapv #(midi-chan-tranpose period %)) ;; create the key-chan list
                   )
          max-key (apply max (mapv :key res))]
      (testing "max `key` should not exceed 127"
        (is (= 127 max-key)))
      (testing "should produce the resulting sequence should produce a continuous range"
        (is (= (range 500)
               (map (fn [{:keys [key chan]}]
                      ;; convert back to a numeric sequence
                      ;;  the sequence should be continuous with no gaps
                      (+ key (* period (dec chan))))
                    res)))))))
