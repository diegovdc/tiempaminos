(ns tieminos.compositions.garden-earth.moments.two.sections-v1)

(do
  (def sections
    [[:fondo-oceánico
      [[:fondo-oceánico 2]
       [:erupciones-submarinas 2]
       [:fuente-hydrotermal 2]
       [:quimio-síntesis 1]
       [:ecosistema-submarino 1 [:sucesión, :vida :muerte]]]]

     [:formación-de-tierra
      [[:movimientos-tectónicos 2]
       [:magma 0.5 [:secundario]]
       [:lava-solidificada 1 [:submarino]]
       [:emergiendo-del-océano 2]
       [:temblores 0.5 [:sumando/secciones "pequeña tierra formándose"]]
       [:magma 0.5 [:secundario :viene-y-va]]
       [:terremotos 1 [:sumando/secciones "tierra ya formada"]]
       [:ecosistema 1 [:sumando/secciones "vida ya formada"]]
       [:lava 0.5 [:secundario :viene-y-va "fluyendo al mar"]]
       [:estrátos 2 [:sumando/secciones :secundario "fondo lento"]]]]

     [:erupción
      [[:a-interior-de-la-tierra 1 [:incrementos]]
       [:movimientos-tectónicos 1 [:incrementando]]
       [:movimientos-magmáticos 1 [:incrementando]]
       [:ecosistema 1 [:sumando/secciones :secundario "retorno filtrado como escuchado desde la tierra"]]
       [:plume 1 [:crecimiento-ascenso]]
       [:erupción 1 ["humo rocas"]]
       [:lava 3 [:creciendo]]
       [:ecosistema-en-pánico 1 ["nuevamente al exterior"]]
       [:lava-descendiendo 1 ]
       [:temblores 1 [:disminuyendo "magma endureciéndose"]]
       [:cuasi-silencio 2 ["nuevamente al exterior"]]]]

     [:totalidad/vida-regalo-de-las-profundidades
      [[:formación-de-ecosistema 4 [:sucesión]]
       [:movimientos-tectónicos 0.5 [:sumando/secciones] ]
       [:formación-de-montañas/estratos 2 [:sumando/secciones "en mezcla con ecosistemas"]]
       [:multiplicación-de-ecosistemas 2 [:sumando/secciones :alejamiento]]
       [:algún-lugar-hermoso->hermosura-en-general 0.5 [:sumando/desaparece-al-final :meta-slendro]]
       [:ecosistema-oceánico 1]
       [:magma/movimientos-tectónicos 1 [:superposición/convolución-con-ecosistemas]]
       [:todo 1 [:superposición/desvanecimiento]]]]])

  [:MAIN-DURATIONS
   (->> sections
        (mapv (fn [[section subsections]]

                [section (apply + (map second subsections))]))
        ((fn [sections-data]
           (conj sections-data
                 [:total (apply + (map second sections-data))]))))
   :SECTION-LIST
   (->> sections
        (mapcat second)
        (map first))
   :TOTAL-SECTIONS
   (->> sections
        (mapcat second)
        (map first)
        count)]

  )
