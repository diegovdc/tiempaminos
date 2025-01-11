(ns tieminos.lumatone.workfile
  (:require
   [erv.mos.mos :as mos]
   [thi.ng.color.gradients :as grad]
   [tieminos.lumatone.coord-system :as gral-kb]
   [tieminos.lumatone.ltn :refer [make-ltn parse-ltn]]
   [tieminos.lumatone.mos-colorizer :refer [color-fn2
                                            mos-degs-rings-with-gradient update-key-colors]]))

(comment
  ;; generate the keyboards

  (do
    (def gen 9)
    (def period 23)
    (def keyboards (gral-kb/generate-keyboard-types->xy-intervals gen period))
    (def kbd-number 2)
    (-> keyboards))

  ;; generate keyboard data
  (def kbd-data
    ;; config
    (let [offset 31 ;; NOTE explore different offsets for the lowest midi note
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
  (def mos-rings [[9 9 5]
                  [4 5 4 5 5]
                  [4 4 1 4 4 1 4 1]
                  [3 1 3 1 1 3 1 3 1 1 3 1 1]
                  [2 1 1 2 1 1 1 2 1 1 2 1 1 1 2 1 1 1]])

  ;; make lumatone file
  (let [root "/Users/diego/Music/diego/lumatone"
        filenote "" ;; a description appened near the end of the filename
        filename (format "%sEDO-gen%s_%s-%skb.ltn"
                         period
                         gen
                         (first (nth (keys keyboards) kbd-number))
                         (second (nth (keys keyboards) kbd-number))
                         (if (seq filenote) (str "_" filenote) ""))
        path (format "%s/%s" root filename)]

    (->> kbd-data
         gral-kb/ltn-data->ltn
         parse-ltn
         (update-key-colors (partial color-fn2
                                     (mos-degs-rings-with-gradient
                                      (grad/cosine-schemes :yellow-green-blue)
                                      mos-rings)
                                     0))
         make-ltn
         (spit path))))
