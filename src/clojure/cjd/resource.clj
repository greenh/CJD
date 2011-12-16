#_ (* Provides support for specifying resources, such as images or .css documents,
      to be included in the output.
      )
(ns cjd.resource)

(def css-docs* (ref #{}))
(def resource-fns* (ref #{}))

#_ (* Adds one or more CSS document name to the list of CSS documents to be referenced by
      generated HTML pages.
      @arg css-doc A string representing a CSS document name.
     )
(defn add-css [css-doc] 
  (dosync (alter css-docs* conj css-doc)))

#_ (* Adds a request to copy a resource to the output directory.
      @p CJD evaluates the requests when it's had a chance to fully establish
      its @(linki cjd.context.Context), and can pass it to requesting functions. This
      allows the functions to select resources appropriately, based, for 
      example, on the "theme" requested by the user.
      
      @(arg resource-fn A resource specification function. This is a function
            of the form @(fun (fn [context])), where
            @arg context The @(linki cjd.context.Context) object currently in use.
            @(returns 
               A tuple of the form @(form [doc-name resource-name]), where
               @arg doc-name A string containing the name of the document 
               (relative to the output directory) to which the resource will be
               copied. 
               @arg resource-name The name of the resource to copy. For Java,
               this is as defined by
               @(linkto "http://docs.oracle.com/javase/7/docs/api/java/lang/Class.html#getResource%28java.lang.String%29"
                        Class.getResource).
               @p Or, if nothing needs to be copied, nil can be returned.)
            )
      )
(defn copy-resource [resource-fn]
  (dosync (alter resource-fns* conj resource-fn)))


#_ (add-css "cjd.doc")
#_ (copy-resource 
  (fn [context]
    (if (= (context-theme context) :dark)
      ["cjd.css" "/cjd/resources/cjd-r.css"]
      ["cjd.css" "/cjd/resources/cjd.css"])))