(ns lumatone.colorizer-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [erv.mos.mos :as mos]
   [lumatone.colorizer :refer [color-fn colorize-ltn degs-rings-with-gradient
                               mos-degs-rings-with-gradient
                               mos-degs-rings-with-gradient
                               mos-degs-rings-with-gradient-old
                               note-in-degs-seq?]]
   [thi.ng.color.gradients :as grad]))

(deftest mos-degs-rings-with-gradient-test
  (testing "The deprecated version and the new version work the same"
    (is (= (mos-degs-rings-with-gradient-old (grad/cosine-schemes :red-blue)
                                             (mos/make 31 12))
           (mos-degs-rings-with-gradient (grad/cosine-schemes :red-blue)
                                         (mos/make 31 12))))))

(deftest degs-rings-with-gradient-test
  (is (= '([(0 12) "ff0000"]
           [(0 7 12) "e70018"]
           [(0 2 7 12) "a70058"]
           [(0 2 4 7 9 12) "5800a7"]
           [(0 2 4 6 7 9 11 12) "1800e7"]
           [(0 1 2 3 4 5 6 7 8 9 10 11 12) "111111"])
         (degs-rings-with-gradient (grad/cosine-schemes :red-blue)
                                   '((0 12)
                                     (0 7 12)
                                     (0 2 7 12)
                                     (0 2 4 7 9 12)
                                     (0 2 4 6 7 9 11 12) ;; lidian mode
                                     (0 1 2 3 4 5 6 7 8 9 10 11 12)))))
  (testing "Produces the same results as `mos-degs-rings-with-gradient-old`"
    (is (= '([(0 12) "ff0000"]
             [(0 7 12) "e70018"]
             [(0 2 7 12) "a70058"]
             [(0 2 4 7 9 12) "5800a7"]
             [(0 2 4 6 7 9 11 12) "1800e7"]
             [(0 1 2 3 4 5 6 7 8 9 10 11 12) "111111"])
           (mos-degs-rings-with-gradient-old (grad/cosine-schemes :red-blue)
                                             (mos/make 12 7))
           (degs-rings-with-gradient (grad/cosine-schemes :red-blue)
                                     '((0 12)
                                       (0 7 12)
                                       (0 2 7 12)
                                       (0 2 4 7 9 12)
                                       (0 2 4 6 7 9 11 12) ;; lidian mode
                                       (0 1 2 3 4 5 6 7 8 9 10 11 12)))))))

(deftest color-fn-test
  (let [deg-colors-p12-g7 '([(0 12) "ff0000"]
                            [(0 7 12) "e70018"]
                            [(0 2 7 12) "a70058"]
                            [(0 2 4 7 9 12) "5800a7"]
                            [(0 2 4 6 7 9 11 12) "1800e7"]
                            [(0 1 2 3 4 5 6 7 8 9 10 11 12) "111111"])]
    (testing "Returns the corresponding color for a key: the first `deg-seq` to contain the given degree."
      (is (= "ff0000"
             (color-fn deg-colors-p12-g7
                       0
                        ;; NOTE full data structure
                       {:chan-val 1,
                        :chan "Chan_0",
                        :key-val 0,
                        :color "Col_0",
                        :key "Key_0",
                        :key-type nil,
                        :color-val "000000",
                        :key-type-val nil})))
      (is (= "5800a7"
             (color-fn deg-colors-p12-g7
                       0
                       {:key-val 4,
                        :color-val "000000"
                        :key-type-val nil}))))

    (testing "If `key-val` is greater than period interval (12 in the example), then it wraps around the color scheme"
      (is (= "a70058"
             (color-fn deg-colors-p12-g7
                       0
                       {:key-val 14,
                        :color-val "000000",
                        :key-type-val nil}))))
    (testing "If `key-type-val` is \"0\" returns \"111111\" "
      (is (= "111111"
             (color-fn deg-colors-p12-g7
                       0
                       {:key-val 14,
                        :color-val "000000",
                        :key-type-val "0"}))))))

(deftest note-in-degs-seq?-test
  (testing "Returns `true` if `midi-note` is in the `degs-seq`"
    (is (true? (note-in-degs-seq?
                '(0 7 12)
                0
                0 ;; midi-note NOTE that 0 is not a valid midi note... check if this causes problems
                ))))
  (testing "Returns `false` if `midi-note` is not in the `degs-seq`"
    (is (false? (note-in-degs-seq? '(0 7 12) 0 1))))

  (testing "Returns `true` because `base-midi` shifts the `degs-seq`"
    (is (true? (note-in-degs-seq? '(0 7 12) 1 1))))
  (testing "A whole range of midi notes (two octaves) and their values"
    (is (= [true ;; 0
            false
            false
            false
            false
            false
            false
            true ;; 7
            false
            false
            false
            false
            true ;; 12
            false
            false
            false
            false
            false
            false
            true ;; 19
            false
            false
            false
            false
            true ;; 24
            ]
           (map #(note-in-degs-seq? '(0 7 12) 0 %)
                (range 25)))))
  (testing "A whole range of midi notes shifted by 1"
    (is (= [false
            true
            false
            false
            false
            false
            false
            false
            true
            false
            false
            false
            false]
           (map #(note-in-degs-seq? '(0 7 12) 1 %)
                (range 13))))))

(deftest colorize-ltn-test
  (testing "Updates the colors of a parsed-ltn file"
    (is (= '(["[Board0]"
              ({"Key_0" 0, "Chan_0" 1, "Col_0" "ff0000", "CCInvert_0" nil}
               {"Key_1" 2, "Chan_1" 1, "Col_1" "a70058", "CCInvert_1" nil})]
             ["[Board1]"
              ({"Key_0" 20, "Chan_0" 1, "Col_0" "111111", "CCInvert_0" nil}
               {"Key_1" 22, "Chan_1" 1, "Col_1" "111111", "CCInvert_1" nil})])
           (colorize-ltn (partial color-fn
                                  '([(0 12) "ff0000"]
                                    [(0 7 12) "e70018"]
                                    [(0 2 7 12) "a70058"]
                                    [(0 2 4 7 9 12) "5800a7"]
                                    [(0 2 4 6 7 9 11 12) "1800e7"]
                                    [(0 1 2 3 4 5 6 7 8 9 10 11 12) "111111"])
                                  0)
                         '({"[Board0]" ({"Key_0" 0, "Chan_0" 1, "Col_0" "000000", "CCInvert_0" nil}
                                        {"Key_1" 2, "Chan_1" 1, "Col_1" "000000", "CCInvert_1" nil})}
                           {"[Board1]" ({"Key_0" 20, "Chan_0" 1, "Col_0" "000000", "CCInvert_0" nil}
                                        {"Key_1" 22, "Chan_1" 1, "Col_1" "000000", "CCInvert_1" nil})}))))))
