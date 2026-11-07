(ns tieminos.compositions.7d-percusion-ensamble.dreams.dream-2.snippets
  {:clj-kondo/ignore true})

(comment
  ;; one
  (let [scale 12]
    (rain.v2/ref-rain
     :id :jam
     :durs [1]
     :ratio 1/9
     :on-event (rain.v2/on-event

                ((s) {:freq (:freq (*7d-base/deg->data
                                    :base-freq (* 1/2 root)
                                    :scale scale
                                    :degree (rainseq [0])))
                      :amp (rrange 0.5 0.9)
                      :dcy (rrange 0.5 2)
                      :atk (rrange 0.01 0.03)
                      :mod-freq (rrand 3000 5000)
                      :out (mseq i (mancha :mancha-1 space/main-graph-2d))})
                ((s) {:freq (:freq (*7d-base/deg->data
                                    :base-freq (* root)
                                    :scale scale
                                    :degree (rainseq (conj (repeat 10 2)
                                                           (repeat 10 5)))))
                      :amp (rrange 0.5 0.9)
                      :dcy (rrange 0.2 0.6)
                      :atk (rrange 0.01 0.03)
                      :mod-freq (rrand 3000 5000)
                      :out (mseq i (mancha :mancha-2 space/main-graph-2d))})))))
