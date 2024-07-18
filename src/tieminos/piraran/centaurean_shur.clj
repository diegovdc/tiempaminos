(ns tieminos.piraran.centaurean-shur
  (:require
   [erv.constant-structures.graphics :refer [init-cs-tool!]]
   [erv.cps.core :as cps]
   [erv.utils.conversions :as conv]
   [erv.utils.core :refer [period-reduce rotate]]
   [tieminos.lattice.v1.lattice :refer [draw-lattice]]))

(def timeknot-shur
  [{:cents 0 :ratio 1}
   {:cents 182 :ratio 49/44}
   {:cents 296 :ratio 19/16} ;; actually between 32/27 and 19/16
   {:cents 500 :ratio 4/3} ;; + 2 cents
   {:cents 704 :ratio 3/2} ;; +2 cents
   {:cents 794 :ratio 19/12} ;; actually between 128/81 and 19/12
   {:cents 886 :ratio 5/3} ;; + 2 cents
   {:cents 998 :ratio 16/9}] ;; + 2 cents
  )

(def centaurean-shur
  [{:cents 0 :ratio 1 :degree 0}
   {:cents 204 :ratio 9/8 :degree 2}
   {:cents 266 :ratio 7/6 :degree 3}
   {:cents 498 :ratio 4/3 :degree 5}
   {:cents 702 :ratio 3/2 :degree 7}
   {:cents 764 :ratio 14/9 :degree 8}
   {:cents 886 :ratio 5/3 :degree 9}
   {:cents 968 :ratio 7/4 :degree 10}])

(comment
  (-> (map :ratio
           centaurean-shur))
  (def lattice-data (draw-lattice
                     {:ratios (map :ratio
                                   centaurean-shur)})))

(def centaura
  [1
   33/32
   9/8 ;; could be 12/11
   7/6
   5/4
   4/3
   11/8
   3/2
   14/9
   5/3
   7/4
   15/8])

(do
  (defn rotate-scale
    [rotation sorted-ratios]
    (let [rotated (rotate sorted-ratios rotation)
          f (first rotated)]
      (sort (mapv #(period-reduce (/ % f)) rotated))))

  (defn scale-modes
    [ratios]
    (map #(rotate-scale % ratios)
         (range (count ratios)))))

(comment
  (->> (scale-modes centaura)
       (map-indexed (fn [i %]
                      [i (map conv/ratio->cents %)])))

  (map-indexed (fn [i r] [i r (conv/ratio->cents r)]) centaura)

  (->> (cps/make 2 [1 3 5 7])
       :scale
       (map :bounded-ratio))

  (comment
    (def lattice-data (draw-lattice
                       {:ratios (sort (concat centaura
                                              (->> (cps/make 2 [1 3 5 7 11])
                                                   :scale
                                                   (map :bounded-ratio))))}))
    (def lattice-data (draw-lattice
                       {:ratios (sort (concat centaura
                                              #_(->> (cps/make 2 [1 3 7])
                                                     :scale
                                                     (map :bounded-ratio))))}))

    (->> (cps/make 2 [1 3 7])
         :scale
         (map :bounded-ratio))

    (defn- ratios->scale [ratios]
      (map (fn [r] {:bounded-ratio r
                    :bounding-period 2})
           ratios))

    (count (sort (set (concat centaura
                              (->> (cps/make 2 [1 3 7])
                                   :scale
                                   (map :bounded-ratio))))))

    (count (sort (set (concat centaura
                              #_(->> (cps/make 2 [1 7 11])
                                     :scale
                                     (map :bounded-ratio))
                              (->> (cps/make 2 [1 3 11])
                                   :scale
                                   (map :bounded-ratio))
                              (->> (cps/make 2 [1 3 7])
                                   :scale
                                   (map :bounded-ratio))
                              (->> (cps/make 2 [1 3 5])
                                   :scale
                                   (map :bounded-ratio))
                              (->> (cps/make 2 [3 5 b])
                                   :scale
                                   (map :bounded-ratio))
                              #_(->> (cps/make 2 [1 5 11])
                                     :scale
                                     (map :bounded-ratio))))))
    (init-cs-tool!
     (ratios->scale (sort (set (concat centaura
                                       #_(->> (cps/make 2 [1 7 11])
                                              :scale
                                              (map :bounded-ratio))
                                       (->> (cps/make 2 [1 3 11])
                                            :scale
                                            (map :bounded-ratio))
                                       (->> (cps/make 2 [1 3 7])
                                            :scale
                                            (map :bounded-ratio))))))
     []
     #_[(conv/cents->ratio 450)
        (conv/cents->ratio 600)]))

  (conv/ratio->cents (/ 11/8 5/4))
  (/ 7/6 49/44)

  (map (fn [r] [r (conv/ratio->cents r)]) centaura))
