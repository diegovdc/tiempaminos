(ns lumatone.workfile
  (:require
   [erv.mos.mos :as mos]
   [lumatone.colorizer
    :refer [color-fn colorize-ltn degs-rings-with-gradient
            mos-degs-rings-with-gradient]]
   [lumatone.coord-system :as gral-kb]
   [lumatone.ltn :as ltn]
   [thi.ng.color.gradients :as grad]))

(comment
  ;; generate the keyboards

  (do
    (def gen 11)
    (def period 36)
    (def keyboards (gral-kb/generate-keyboard-types->xy-intervals gen period))
    (def kbd-number 4)
    (-> keyboards))

  ;; generate keyboard data
  (def kbd-data
    ;; config
    (let [offset (+ 10  (* 19 2)) ;; NOTE: explore different offsets for the lowest midi note
          ]
      (->> (gral-kb/make-ltn-data
            {:offset offset
             :period period
             :xy-intervals (-> keyboards
                               vals
                               (nth kbd-number))}))))
  (->> kbd-data
       (map (juxt :chan-value :key-value))
       (sort-by second)
       reverse)
  [;; lowest midi note
   (->> kbd-data (map :key-value) (apply min))
   ;; highest midi note
   (->> kbd-data (map :key-value) (apply max))]

  ;; print to choose mos rings
  (mos/make period gen)

  (do
    (def use* :mos)
    (def mos-rings [[11 11 11 3]
                    [8 3 8 3 8 3 3]
                    [5 3 3 5 3 3 5 3 3 3]
                    [2 3 3 3 2 3 3 3 2 3 3 3 3]
                    [2 2 1 2 1 2 1 2 2 1 2 1 2 1 2 2 1 2 1 2 1 2 1]])

    (def deg-rings [[4 7 9 11 12 16 0 2]
                    [11 14 16 18 0 4 7 9]
                    [0 3 5 7 8 12 15 17]
                    [12 15 17 0 1 5 8 10]])

    (def mos-colors (mos-degs-rings-with-gradient
                     (grad/cosine-schemes :yellow-green-blue)
                     mos-rings))
    (def deg-colors (degs-rings-with-gradient
                     (grad/cosine-schemes :yellow-green-blue)
                     deg-rings
                     period)))
  ;; make lumatone file
  (let [zeroth-degree-midi-note 60
        root "/Users/diego/Music/diego/lumatone"
        filenote "" ;; a description appened near the end of the filename
        filename (format "%st-gen%s_%s-%skb%s.ltn"
                         period
                         gen
                         (first (nth (keys keyboards) kbd-number))
                         (second (nth (keys keyboards) kbd-number))
                         (if (seq filenote) (str "__" filenote) ""))
        path (format "%s/%s" root filename)]

    (->> kbd-data
         gral-kb/ltn-data->ltn
         ltn/parse-ltn
         (colorize-ltn (partial color-fn period (if (= use* :degs) deg-colors mos-colors) zeroth-degree-midi-note))
         ltn/make-ltn
         ltn/add-gral-config
         (spit path))
    filename))
