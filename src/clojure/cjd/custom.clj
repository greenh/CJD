#_ ( Copyright (c) 2011 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Functions to support customization of CJD operation. 
      @p This includes specifying resources such as images or .css documents to be 
      included in the output, and for custom headers and footers.
      )
(ns cjd.custom)

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

(def header-fn* (ref nil))
(def footer-fn* (ref nil))

#_ (* Specifies a function to be invoked by CJD to create a header for each generated page.
      @(arg header-fn The header-generating function, which should have a form
            @(fun [context]), where\: 
            @arg context The @(il cjd.context.Context) object describing the current state
            of CJD processing.
            @returns A string containing HTML content to be used as the header.)
      )
(defn set-header [header-fn]
  (ref-set header-fn* header-fn))

#_ (* Specifies a function to be invoked by CJD to create a footer for each generated page.
      @(arg footer-fn The footer-generating function, which should have a form
            @(fun [context]), where\: 
            @arg context The @(il cjd.context.Context) object describing the current state
            of CJD processing.
            @returns A string containing HTML content to be used as the footer.)
      )
(defn set-footer [footer-fn]
  (ref-set footer-fn* footer-fn))


