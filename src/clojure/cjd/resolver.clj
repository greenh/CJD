#_ ( Copyright (c) 2011 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* In some cases in CJD comments, symbols appear that refer to entities
      in the commented code. @name provides a few functions that 
      relate to figuring out what those particular symbols actually are. 
      As a rule, resolution takes place within a symbol's context of use.
      
      @p @(sc Abandon all hope, ye who enter here.)
      
      @see @(link cjd.link-resolver), which provides the mechanisms for extending the
      set of external documentation sources known to CJD.
      )
(ns cjd.resolver
  (:use 
    [cjd.context]
    [cjd.artifact-base]
    [cjd.link-resolver]
    )
  )

(def required-nss* (ref #{ }))
(def munged-nss-map* (ref {}))

(defn reset-resolver [] (dosync (ref-set required-nss* #{})))

#_ (* Returns the class object, given a symbol naming a class. 
      @arg class-sym A symbol putatively containing a class name,
      e.g. @c java.lang.Object .
      @returns The corresponding class object, or nil if it wasn't found.)
(defn find-class [class-sym] (Class/forName (str class-sym)))

#_ (* Tries to resolve a name in the context of the namespace of its use.
      
      @p This turns into a remarkably exciting proposition because Clojure 
      really manages names in two worlds\: vars, and the native, e.g., Java,
      namespace. @(i Most) named artifacts have names in the Clojure world,
      but some—notably records and types—are named in the native naming space, 
      the fact that they originiate in the Clojure world notwithstanding.
      
      @p Thus, a name can legitimately resolve to a Clojure var or
      the name of a class. Even if it resolves to a class, @name makes
      a heroic attempt to reframe the classname into a Clojure-like form.
      A record "Fum" defined in namespace "foo-bar" will have
      a (munged) class name of "foo_bar.Fum"\; @name will effectively resolve this
      to "foo.bar/Fum" (though it actually returns the namespace and var-name 
      components separately).
      @arg sym The symbol containing the name to resolve.
      @arg use-ns The namespace name (a symbol) to resolve against.
      @(returns 
         A tuple of the form @(form [ns-name var-name]), where\:
         @arg ns-name A symbol denoting the namespace, @(i OR) name of a Java class.
         @arg var-name If @(arg ns-name) denotes a Clojure namespace, 
         this is the name of the var within the namespace 
         (i.e., with all prefixes scraped off). If @(arg sym) denotes
         a class or namespace, this is nil.)
      )
(defn resolve-symbol [sym use-ns]
  (try 
    (when-not (get @required-nss* use-ns)
      #_(prn 'resolve-symbol 'adding use-ns)
      (require use-ns)
      (let [nss (all-ns)
            munge-map (zipmap (map namespace-munge nss) (map ns-name nss))]
        (dosync 
          (alter required-nss* conj use-ns)
          (ref-set munged-nss-map* munge-map))))
    (if-let [ns-obj (find-ns use-ns)]
      (let [sym-var (ns-resolve ns-obj sym)]
        (cond
          (class? sym-var) 
          (let [cname (.getName sym-var)
                li (.lastIndexOf cname (int \.))
                csym (subs cname (inc li))
                munged-ns-name (subs cname 0 li)
                real-ns-name (get @munged-nss-map* munged-ns-name)]
            #_ (println "rs:" cname li csym munged-ns-name real-ns-name @munged-nss-map*)
            (if real-ns-name
              [(symbol real-ns-name) (symbol csym)]
              [(symbol cname) nil]))
          
          (var? sym-var)
          (let [{ sym-ns :ns sym-name :name} (meta sym-var)]
            [(ns-name sym-ns) sym-name])  
          
          :else nil
          )))
    ; ClassNotFoundException naturally occurs whenever we resolve a namespace name
    (catch ClassNotFoundException e)
    ; Other exceptions, however...
    (catch Exception e
      (println "resolve-symbol exception:\n" (.getMessage e)))))

#_ (* Given a symbol that names an entity, attempts to generate a URI that
      locates that entity's documentation. 
      @p There are several possibilities addressed here\: 
      @(ul 
         @li the entity could be in the context namespace 
         (i.e., is in the file currently being processed)
         @li it could be in a "local" namespace, a namespace that's part of the current 
         CJD-doc run.
         @li it could be in a namespace known by virtue of an externally supplied 
         mapping function.
         @li The entity could @(i be) a namespace. 
         @li or, none of the above, in which case it's clueless, and admits it.)
      @arg context The current context map.
      @arg item The entity of interest.
      @(returns 
         A tuple of the form [uri ns-sym sym-sym], where
         @arg uri A string containing the URI of the item of interest, or nil if the
         entity couldn't be located.
         @arg item-ns A symbol that identifies the namespace. 
         @arg item-sym A symbol that identifies the entity within the namespace. Note that 
         this will be nil if @(arg item) designates a namespace or class.
         @p Or, if @(arg item) can't be resolved, returns nil.)
      )
(defn link-resolvex [context item]
  #_(prn 'resolvex item #_context )
  (let [local-ns (context-ns context)
        local-namespaces (context-namespaces context)
        [sym-ns sym-name] (resolve-symbol item local-ns)]
    #_(prn  'resolvex item local-ns sym-ns sym-name  local-namespaces)
    (letfn [(resfn [xns xname] 
              (cond 
                (= xns local-ns)
                [(if xname (str "#" xname) "#top") xns xname]
                
                (get local-namespaces xns)
                [(str (name xns) ".html" (if xname (str "#" xname))) xns xname]
                
                :else
                (if-let [link (resolve-link xns xname)]
                  [link xns xname])))]
      (if sym-ns
        ; lookup succeeded in finding something
        (resfn sym-ns sym-name)
        
        ; else, no cigars on the lookup; see if maybe it's a namespace ID
        (if-let [alt-ns (find-ns item)]
          (resfn (ns-name alt-ns) nil)
          ; or, maybe it's a record/type name.
          ; This is a big pain the behind, as record names are officially
          ; names of java classes, but are only implicitly imported into the 
          ; namespace of definition. Generate the namespace and name into
          ; a class name, and see if we can resolve that...
          #_(let [fqsym (symbol (str local-ns "/" item))
                art (get-artifact fqsym)]
            (println "resolver -- looking for" fqsym)
            (resfn local-ns item))
          
          ; Finally, assume that it might be a name or namespace name not
          ; declared or used anywhere in scope. In this case, we try
          ; and see if one of resolve-link's routines knows and loves it.
          (let [nns (namespace item)
                nname (if nns (name item) nil)
                #_ (prn 'trying item nns nname)
                link (if nns 
                       (resolve-link nns nname)
                       (resolve-link item nil))]
            (if link
              [link nns nname])))))))

#_ (* Same as @(link link-resolvex), but only returns the URI. )
(defn link-resolve [context item]
  (let [[uri _ _] (link-resolvex context item)]
    uri))

