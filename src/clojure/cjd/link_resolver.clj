#_ ( Copyright (c) 2011 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Provides mapping services from named entities to documentation URIs for those
      entities.
      
      @p When CJD needs to resolve
      a named entity into a URI that leads to documentation for that entity,
      it internally deals with all of the "obvious" cases, notably for entities
      that are defined within the namespaces specified by files being processed.
      To allow links to other bodies of documentation, the resolver calls 
      each function in the @(link link-resolvers*) list in sequence until one of the 
      functions responds with a URI. 

      @p CJD provides several functions by default, notably for the 
      for the @(c clojure.*) and @(c java.*) namespaces. Extensions can add additional
      mapping functions by calling the @(link add-link-resolvers) function.
      Note that added functions are always placed at the start of the list, so
      newer functions can easily override the mappings made by older ones.
      )
(ns cjd.link-resolver)

#_ (* Defines link resolver functions for a small collection of "well-known"
      naming systems.
      )
(def default-resolver-fns 
  (list
    (fn [ns sym]
      (if (re-matches #"clojure\..*" (name ns))
        (str "http://clojuredocs.org/clojure_core/" (name ns) 
             (if sym (str "/" (name sym))))))
    (fn [ns sym]
      (if (re-matches #"java\..*" (name ns))
        (let [cls (.replaceAll (name ns) "\\." "/")]
          (str "http://download.oracle.com/javase/7/docs/api/" cls ".html" 
               (if sym (str "/" (name sym))))))) ))


#_ (* Reference to the list of link resolver functions. 
      @p Any resolver functions are added to the list before the  
      are placed at the end of the sequence specified by the user, and hence
      can be overridden.
      )
(def link-resolvers* (ref default-resolver-fns))


#_ (* Adds resolver functions to the list of functions used to resolve entites
      named in @(c ~"@(link ...)") constructs into URIs.
      @p Note that order of the functions in @(arg resolver-fns) is preserved when they
      are added to the list of resolution functions. 
      
      @(arg resolver-fns One or more name resolution functions.  
            @p Each function must have the form @(fun (fn [ns-name sym])), where\:
            @arg ns-name A symbol designating the name of a namespace
            @arg sym The symbol designating a name within the namespace. This may be null,
            in which case the requested URI is to the @(arg ns-name) namespace.
            @returns A string containing the URI to the documentation for the
            specified entity, or nil if the function doesn't recognize the symbol or
            namespace.
          )
      @returns nil.
      )
(defn add-link-resolvers [& resolver-fns]
  (dosync
    (alter link-resolvers* #(concat resolver-fns %)))
  nil)

#_ (* Resets the resolver function list to its initial state.)
(defn reset-resolver-fns [] 
  (dosync (ref-set link-resolvers* default-resolver-fns)))

#_ (* Resolves a namespace / name pair into a URI.
      @arg ns-name A symbol designating the name of a namespace
      @arg sym The symbol designating a name within the namespace. This may be null,
      in which case the requested URI is to the namespace.
      @returns A string containing the URI to the documentation for the
      specified entity, or nil if no resolution was possible. 
      )
(defn resolve-link [ns-name sym] 
  (some (fn [linkfn] (linkfn ns-name sym)) @link-resolvers*))


