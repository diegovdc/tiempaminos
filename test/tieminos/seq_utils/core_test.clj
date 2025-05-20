(ns tieminos.seq-utils.core-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [tieminos.seq-utils.core :refer [** ++ -- choose div lin mirror mirror2
                                    mseq op rev rev2 xo] :as su]
   [tieminos.utils :refer [wrap-at]]))

(defn- reset-states-fixture
  [f]
  (with-redefs [su/linear-state (atom {})]
    (f)))

(defn- test-seq
  [len mseq-instance]
  (mapv mseq-instance (range len)))

(use-fixtures :each reset-states-fixture)

(deftest xo-test
  (testing "Can parse an xo pattern en return a the matching index (in modulo form) or `nil` depending on the index provided"
    (is (= [0 nil nil 0 nil nil]
           (mapv #(xo "xoo" %)
                 (range 6)))))
  (testing "Can return an xo function that can then return a the matching index (in modulo form) or `nil` depending on the index provided"
    (let [xo3 (xo "xoo")]
      (is (= [0 nil nil 0 nil nil]
             (mapv #(xo3 %) (range 6)))))))

(deftest mseq-test
  (testing "Basic sequences"
    (testing "Given an index and a vector, returns the value, from the vector, that corresponds to the modulo from the index."
      (is (= [1 2 3 1 2 3]
             (mapv #(mseq % [1 2 3])
                   (range 6)))))
    (testing "Sequences can be nested. The modulo index for the nested value is the same as the on in the upper scope, therefore they are not necessarily returned in the order they appear in the vector."
      (is (= [1 3 1 2 1 4]
             (mapv #(mseq % [1 [2 3 4]])
                   (range 6))))
      (is (= [1 :b 1 :a 1 :c 1 :b 1]
             (mapv #(mseq % [1 [2 [:a :b :c]]])
                   (range 9))))
      (testing "Can substitute the use of `at-i` in `ref-rain`s"
        (let [at-i-degs (fn  [at-i] (at-i [0 2 0 (at-i [2 3]) (at-i [7 4]) (at-i [4 5]) 3]))
              degs [0 2 0 [2 3] [7 4] [4 5] 3]]
          (is (= (map
                  (fn [i] (at-i-degs (partial wrap-at i)))
                  (range 1000))
                 (map
                  (fn [i] (mseq i degs))
                  (range 1000))))))
      (testing "It is not necessary that all the items in the vector be returned."
        (is (= [1 3 1 3 1 3]
               (mapv #(mseq % [1 [2 3]])
                     (range 6)))))))
  (testing "`lin`: linear sequences"
    (testing "metadata"
      (with-redefs [random-uuid (fn [] "hola")]
        (is (= {:tieminos.seq-utils.core/linear? true, :linear/id '(1 2 3 4)}
               (meta (lin 1 2 3 4)))))
      (is (= {:tieminos.seq-utils.core/linear? true, :linear/id :id/a}
             (meta (lin :id/a 2 3 4)))))
    (testing "Ensures items are returned in the order they appear in the arge list."
      (is (= [1 2 1 3 1 4]
             (let [my-seq [1 (lin 2 3 4)]]
               (mapv #(mseq % my-seq)
                     (range 6))))))
    (testing "So different `mseqs` can use the same one and return different values for the same index."
      (let [my-lin (lin 2 3 4)]
        (is (= [[1 1] [2 3] [1 1] [4 2] [1 1] [3 4]]
               (mapv (fn [i] [(mseq i [1 my-lin])
                              (mseq i [1 my-lin])])
                     (range 6)))))

      (is (= [[1 1] [2 3] [1 1] [4 2] [1 1] [3 4]]
             (mapv (fn [i] [(mseq i [1 (lin 2 3 4)])
                            (mseq i [1 (lin 2 3 4)])])
                   (range 6)))))

    (testing "If they an id, they will behave independently."
      (is (= [[1 1] [2 2] [1 1] [3 3] [1 1] [4 4]]
             (mapv (fn [i] [(mseq i [1 (lin :id/a 2 3 4)])
                            (mseq i [1 (lin :id/b 2 3 4)])])
                   (range 6))))))

  (testing "`choose`: random items from a sequence"
    (testing "metadata"
      (is (= {:tieminos.seq-utils.core/rand? true}
             (meta (choose 1 2 3 4)))))
    (testing "all chosen values are in the `choices` set, but on every seq of the `chose-vals` they are different."
      (let [choices #{2 3 4}
            chosen-vals (map (fn [_] (let [[_ a _ b _ c] (->> (range 6)
                                                              (map #(mseq % [1 (choose 2 3 4)])))]
                                       [a b c]))
                             (range 20))]
        (is (every? true? (map (partial every? choices)
                               chosen-vals)))
        (is (apply not= chosen-vals))))
    (testing "Returns a value on the sequence"
      (is (= [1 2 1 2 1 2] (mapv #(mseq % [1 (choose 2)])
                                 (range 6))))))

  (testing "`map`: weighted sequences"
    (testing "Most weighted sequences represent the weights given."
      (let [weights {1 7, 2 2, 3 1}
            weight-representation-seqs (map (fn [_] (let [freqs (->> (range 100)
                                                                     (map #(mseq % weights))
                                                                     (frequencies))]
                                                      (> (get freqs 1 0)
                                                         (get freqs 2 0)
                                                         (get freqs 3 0))))
                                            (range 100))
            most-seqs-represent-weights (->> weight-representation-seqs
                                             (frequencies)
                                             ((fn [freqs]
                                                (> (get freqs true 0)
                                                   (get freqs false 0)))))]
        (is (true? most-seqs-represent-weights))))

    (testing "Returns a value on the sequence"
      (is (= [1 2 1 2 1 2] (mapv #(mseq % [1 {2 1}])
                                 (range 6)))))

    (testing "weighted map's keys can be sequences of different kinds"
      (is (= [1 2 1 2 1 2] (mapv #(mseq % [1 {[2] 1}])
                                 (range 6))))
      (is (= [1 2 1 2 1 2] (mapv #(mseq % [1 {(choose 2) 1}])
                                 (range 6))))
      (is (= [1 2 1 3 1 2] (mapv #(mseq % [1 {(lin 2 3) 1}])
                                 (range 6))))))

  (testing "`op`: operations"
    (testing "addition: `plus`, `+` or `++`"
      (is (= [3 3 3 3 3 3] (map #(mseq % (++ 1 2)) (range 6))))
      (is (= [3 4 3 4 3 4] (map #(mseq % (++ (lin 1 2) 2)) (range 6))))
      (is (= [3 5 5 4 4 6] (map #(mseq % (++ (lin 1 2) (lin 2 3 4))) (range 6))))
      (is (= [1 3 1 5 1 5] (map #(mseq % [1 (++ (lin 1 2) (lin 2 3 4))]) (range 6)))))
    (testing "substraction: `minus`, `-` or `--`"
      (is (=  [-1 -1 -1 -1 -1 -1] (map #(mseq % (-- 1 2)) (range 6))))
      (is (= [0 -1 0 -1 0 -1] (map #(mseq % (-- (lin 1 2) 2)) (range 6)))))
    (testing "multiplication: `mult`, `*` or `**`"
      (is (=  [2 2 2 2 2 2] (map #(mseq % (** 1 2)) (range 6))))
      (is (= [4 2 4 2 4 2] (map #(mseq % (** (lin 1 2) 2)) (range 6)))))
    (testing "division: `div`, `/`"
      (is (= [1/2 1/2 1/2 1/2 1/2 1/2] (map #(mseq % (div 1 2)) (range 6))))
      (is (= [1 1/2 1 1/2 1 1/2] (map #(mseq % (div (lin 1 2) 2)) (range 6)))))
    (testing "custom operation: `op`"
      (is (= [5 3 5 3 5 3] (map #(mseq % ((op (comp inc *)) (lin 1 2) 2)) (range 6)))))))

(deftest mirror-test
  (is (= [3 4] (mirror [3 4])))
  (is (= [3 4 5 4] (mirror [3 4 5])))
  (is (= [3 4 5 4] (mirror (lin 3 4 5))))
  (testing "a `choose` collection is returned as is"
    (is (= [1 2 3 4] (mirror (choose 1 2 3 4)))))
  (testing "metadata is preserved"
    (is (= {:tieminos.seq-utils.core/linear? true, :linear/id [1 2 [3 4] 2]}
           (meta (mirror (lin 1 2 [3 4])))))))

(deftest mirror2-test
  (let [res (mirror2 (lin (choose 1 2 3) [3 (lin 4 5 6)]))]
    (is (= [[1 2 3] [3 [4 5 6 5]]] res))
    (testing "metadata is preserved"
      (is (= {:tieminos.seq-utils.core/linear? true,
              :linear/id [[1 2 3] [3 [4 5 6 5]]]}
             (meta res)))
      (is (= {:tieminos.seq-utils.core/rand? true}
             (meta (first res))))
      (is (= {:tieminos.seq-utils.core/linear? true, :linear/id [4 5 6 5]}
             (meta (get-in res [1 1])))))
    (testing "can handle data with maps"
      (is (= [1 2 {3 1} 4 {3 1} 2] (mirror2 (lin 1 2 {3 1} 4)))))))

(deftest rev-test
  (let [res (rev (lin 1 2 (lin 3 4)))]
    (is (= [[3 4] 2 1] res))
    (testing "metadata is preserved"

      (is (= {:tieminos.seq-utils.core/linear? true, :linear/id [[3 4] 2 1]}
             (meta res)))
      (is (= {:tieminos.seq-utils.core/linear? true, :linear/id [3 4]}
             (meta (first res)))))))

(deftest rev2-test
  (let [res (rev2 (lin 1 2 (lin 3 4)))]
    (is (= [[4 3] 2 1] res))
    (testing "metadata is preserved"
      (is (= {:tieminos.seq-utils.core/linear? true, :linear/id [[4 3] 2 1]}
             (meta res)))
      (is (= {:tieminos.seq-utils.core/linear? true, :linear/id [4 3]}
             (meta (first res)))))))


