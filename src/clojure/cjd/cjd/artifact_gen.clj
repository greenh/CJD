
#_ (* Adds support for CJD-specific artifacts to the base CJD HTML generator.
      @p This is an extension to the base CDJ state of affairs, and is intended
      to be loaded by the "--requires" option (command-line version) or 
      using the :requires option @(link cjd.exome/cjd-gen).)
(ns cjd.cjd.artifact-gen
  (:use
    [cjd.cjd.cjd-artifacts]
    [cjd.artifact-base]
    [cjd.core-artifacts]
    [cjd.generate]
    [cjd.parser]
    [cjd.context]
    [cjd.util.extensomatic]
    [cjd.resolver]
    [cjd.resource]
    [hiccup.core]
    )
  (:import
    [cjd.cjd.cjd_artifacts Extenso Constructo Artifactoid Subartifactoid])
  )

(add-css "extensomatic.css")
(copy-resource
  (fn [context]
    (if (= (context-theme context) :dark)
      ["extensomatic.css" "/cjd/resources/extensomatic-r.css"]
      ["extensomatic.css" "/cjd/resources/extensomatic-f.css"])))

#_ (* Generates a tree of extensos. This does a preorder traversal of 
      the extenso tree rooted at @(arg extenso), outputting useful 
      stuff as it goes.
      @arg extenso A @(b var) denoting the extenso to expand.
      @arg uninit-fields A set of symbols of names of uninitialized 
      fields of the top-level extenso, used for decorative purposes.
      @arg context Current context.
      @returns A string of the resulting HTML.
      )
(defn gen-extenso-tree [extenso uninit-fields context]
  #_(prn 'gen-extenso-tree extenso uninit-fields )
  (let [exti (extenso-info extenso)
        uri (link-resolve context (extenso-name-of exti))] 
    (html
      [:p.a1 
       (if uri
         [:a { :href uri} (nonbreak (extenso-name-of exti))]
         (nonbreak (extenso-name-of exti)))
       [:span.expr 
        " [" 
        (interpose 
          " "
          (map 
            (fn [field]
              (let [xfield (if (symbol? field) field
                             (first field))]
                (if (get uninit-fields xfield) 
                  (html [:span.n1 (name xfield)])
                  (html [:span.ex-init (name xfield)])))
              ) 
            (local-fields-of exti)))
        "] "]
       (if (not-empty (local-methods-of exti)) 
         (interpose "&emsp;"  
           (->> (local-methods-of exti)
             (filter #(not (symbol? %)))
             (sort-by first uncased-comparator)
             (map 
               (fn [method]
                 (let [[method-name _] method
                       method-uri (link-resolve context method-name)]
                   (html 
                     (if method-uri
                       [:a { :href method-uri } (nonbreak method-name)]
                       (nonbreak method-name)))))))))]
       (if (not-empty (composed-extensos-of exti)) 
         [:div.s
          (map 
            (fn [comp-extenso] 
              (gen-extenso-tree comp-extenso uninit-fields context))
            (composed-extensos-of exti))]))))

#_ (* Generates HTML content for an extenso.) 
(defn gen-extenso [artifact context]
  (let [flow (parse-comment context (doc-form-of artifact))
        pre-upcontext 
        (reduce 
          (fn [pre-up+ poioo]
            (reduce 
              (fn [pre-up++ method-impl]
                (context-item! pre-up++ (artifact-name-of method-impl) 1))
              pre-up+ (method-implementations-of poioo)))
          context (poioos-of artifact))
        [upcontext _ content] (gen-flow flow pre-upcontext false)
        info (extenso-info (artifact-name-of artifact) 
                           (artifact-name-of (namespace-of artifact)))
        comp-extensos (composed-extensos-of info)]
    (html
      [:p.decl 
       [:span.expr (declaration-form 
                        (list (artifact-name-of artifact) 
                              (local-fields-of info)) upcontext)]]
      [:div.desc content
       (if (not-empty (poioos-of artifact))
        (html
          [:div.s1 
           [:p.v1 [:span.k1 "Locally implemented protocols and interfaces"]]
           [:p.a1 
            (map 
              (fn [poioo] (str (gen-contextual-link poioo context) "&ensp; ")) 
              (poioos-of artifact))]]
          ))
       (if (or (not-empty (poioos-of artifact)) 
               (not-empty (method-implementations-of artifact)))
         (html 
           [:div.s1 
            [:p.v1 [:span.k1 "Locally implemented methods"]]
            (map #(gen-method-impl % context) 
                 (sort-by artifact-name-of uncased-comparator
                          (concat 
                            (mapcat method-implementations-of (poioos-of artifact))
                            (method-implementations-of artifact))))]))
       
       (if (not-empty comp-extensos)
         (html 
           [:div.s1
            [:p.v1 [:span.k1 "Composed extensos"]]
            (map 
              (fn [ex] (gen-extenso-tree ex (set (uninit-fields-of info)) context))
              comp-extensos)
            ]))
       
       ])))

#_ (* Generates HTML content for an extenso.) 
(defn gen-constructo [artifact context]
  (let [flow (parse-comment context (doc-form-of artifact))
        pre-upcontext 
        (reduce 
          (fn [pre-up+ poioo]
            (reduce 
              (fn [pre-up++ method-impl]
                (context-item! pre-up++ (artifact-name-of method-impl) 1))
              pre-up+ (method-implementations-of poioo)))
          context (poioos-of artifact))
        [upcontext _ content] (gen-flow flow pre-upcontext false)
        info (constructo-info (artifact-name-of artifact) 
                           (artifact-name-of (namespace-of artifact)))
        comp-extensos (composed-extensos-of info)]
    (html
      [:p.decl 
       [:span.expr (declaration-form 
                        (list (artifact-name-of artifact) 
                              (local-fields-of info)) upcontext)]]
      [:div.desc content
       (if (not-empty (poioos-of artifact))
        (html
          [:div.s1 
           [:p.v1 [:span.k1 "Locally implemented protocols and interfaces"]]
           [:p.a1 
            (map 
              (fn [poioo] (str (gen-contextual-link poioo context) "&ensp; ")) 
              (poioos-of artifact))]]
          ))
       (if (or (not-empty (poioos-of artifact)) 
               (not-empty (method-implementations-of artifact)))
         (html 
           [:div.s1 
            [:p.v1 [:span.k1 "Locally implemented methods"]]
            (map #(gen-method-impl % context) 
                 (sort-by artifact-name-of uncased-comparator
                          (concat 
                            (mapcat method-implementations-of (poioos-of artifact))
                            (method-implementations-of artifact))))]))
       
       (if (not-empty comp-extensos)
         (html 
           [:div.s1
            [:p.v1 [:span.k1 "Composed extensos"]]
            (map 
              (fn [ex] (gen-extenso-tree ex (set (uninit-fields-of info)) context))
              comp-extensos)
            ]))
       ])))


(mk-category :extenso "Extenso")
(mk-category :constructo "Constructo")

(mk-art Extenso gen-extenso :extenso)
(mk-art Constructo gen-constructo :constructo)

(mk-art Artifactoid gen-constructo :constructo)
(mk-art Subartifactoid gen-constructo :constructo)
