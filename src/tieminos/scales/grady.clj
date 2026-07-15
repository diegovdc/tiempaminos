(ns tieminos.scales.grady
  (:require
   [erv.utils.ratios :refer [ratios->scale]]))

(def scales {:centaur {:meta {:scl/name "centaur.scl"
                              :scl/description "Centaur scale by Kraig Grady. 7-limit."}
                       :scale (ratios->scale [1 21/20 9/8 7/6 5/4 4/3 7/5 3/2 14/9 5/3 7/4 15/8])}
             :centaura {:meta {:scl/name "centaura.scl"
                               :scl/description "Centaura scale by Kraig Grady. 11-limit."}
                        :scale (ratios->scale [1 33/32 9/8 7/6 5/4 4/3 11/8 3/2 14/9 5/3 7/4 15/8])}
             :metaslendro-12 {:meta {:scl/name "metaslendro-12.scl"
                                     :scl/description "Metalslendro scale by Grady-Wilson."}
                              :scale (ratios->scale [49/48 25/24 7/6 19/16 4/3 65/48 3/2 37/24 151/96 7/4 43/24 2/1])}})
