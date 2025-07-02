(ns lumatone.workfile
  (:require
   [erv.mos.mos :as mos]
   [lumatone.colorizer
    :refer [color-fn colorize-ltn degs-rings-with-gradient
            mos-degs-rings-with-gradient]]
   [lumatone.coord-system :as gral-kb]
   [lumatone.ltn :refer [make-ltn parse-ltn]]
   [thi.ng.color.gradients :as grad]))

(comment
  ;; generate the keyboards

  (do
    (def gen 11)
    (def period 19)
    (def keyboards (gral-kb/generate-keyboard-types->xy-intervals gen period))
    (def kbd-number 2)
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
  (-> kbd-data)
  [;; lowest midi note
   (->> kbd-data (map :key-value) (apply min))
   ;; highest midi note
   (->> kbd-data (map :key-value) (apply max))]

  ;; print to choose mos rings
  (mos/make period gen)

  (do
    (def use* :degs)
    (def mos-rings [[4 7 4 7 7]
                    [4 4 3 4 4 3 4 3]
                    [1 3 1 3 3 1 3 1 3 3 1 3 3]
                    [1 1 2 1 1 2 1 2 1 1 2 1 1 2 1 2 1 1 2 1 2]])

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
        filenote "for-17o7_19t-cs-from-27t_no84" ;; a description appened near the end of the filename
        filename (format "%st-gen%s_%s-%skb%s.ltn"
                         period
                         gen
                         (first (nth (keys keyboards) kbd-number))
                         (second (nth (keys keyboards) kbd-number))
                         (if (seq filenote) (str "__" filenote) ""))
        path (format "%s/%s" root filename)]

    (->> kbd-data
         gral-kb/ltn-data->ltn
         parse-ltn
         (colorize-ltn (partial color-fn period (if (= use* :degs) deg-colors mos-colors) zeroth-degree-midi-note))
         make-ltn
         (spit path))
    filename))
