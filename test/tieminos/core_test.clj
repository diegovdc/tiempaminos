(ns tieminos.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tieminos.core :as subject]))

(deftest refresh-test
  (testing "Project compiles"
    (is (= :ok (subject/refresh)))))
