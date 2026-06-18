(ns tieminos.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [tieminos.core :as subject]))

(deftest refresh-test
  (is (= :ok (subject/refresh))))
