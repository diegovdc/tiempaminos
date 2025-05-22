(ns tieminos.seq-utils.utils-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [taoensso.timbre :as timbre]
   [tieminos.seq-utils.utils :refer [bigraph invert-graph seq->graph subgraph]]))

(deftest invert-graph-test
  (is (= {3 #{1 2}
          2 #{1}
          1 #{3}}
         (invert-graph
          {1 #{2 3}
           2 #{3}
           3 #{1}}))))

(deftest bigraph-test
  (is (= {1 #{3 2}
          2 #{1 3}
          3 #{1 2}}
         (bigraph
          {1 #{2 3}
           2 #{3}
           3 #{1}})))
  (testing "The bigraph of a bigraph is the same as the input"
    (let [bg {1 #{3 2}
              2 #{1 3}
              3 #{1 2}}]
      (is (= bg (bigraph bg))))))

(deftest subgraph-test
  (is (=  {1 #{2} 2 #{1}}
          (subgraph {1 #{2 3}
                     2 #{1 3}
                     3 #{1 2}}
                    #{1 2})))
  (testing "Prints a warning if the graph has empty edges that were previously not empty.."
    (let [warn-called? (atom false)]
      (with-redefs [timbre/-log! (fn [& s] (reset! warn-called? true))]
        (is (=  {1 #{2} 2 #{}}
                (subgraph {1 #{2 3} 2 #{3}}
                          #{1 2})))
        (is (true? @warn-called?))

        (testing "Wont' print a warning if edge was already empty."
          (reset! warn-called? false)
          (subgraph {1 #{2 3} 2 #{}}
                    #{1 2})
          (is (false? @warn-called?)))))))

(deftest seq->graph-test
  (is (= {1 #{2}, 2 #{3}, 3 #{1}}
         (seq->graph [1 2 3])))

  (is (= {1 #{2 3}, 2 #{1}, 3 #{1}}
         (seq->graph [1 2 1 3]))))

