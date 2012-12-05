#_ ( Copyright (c) 2011 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* CJD data model for artifacts defined in @(link clojure.core). 
      
      @p Defines a set of "artifacts" (in essence, data structures or object classes) 
      that collectivly constitute a data model for what is, in effect, a very rough
      abstract syntax tree for a clojure file.
      
      @p An "artifact" models a specific kind of Clojure entity, such as a function, 
      macro, variable, protocol, record... Because the set of such entities is obviously
      extensible (starting with @(link defartifact)), it follows that the set
      of artifacts must be exensible as well.
      
      @p Note that there are several additional extensos— @(link ArtifactBase) and 
      @(link HasCJDoc) —
      that are incorporated by @(link defartifact) into every artifact. 
      Similarly, the @(link SubartifactBase) extenso
      forms the basis for all subartifacts.

      )
(ns cjd.core-artifacts
  (:use
    [extensomatic.extensomatic]
    [cjd.artifact-base]
    [cjd.util.string-utils]
    [clojure.pprint]
    )
  (:import 
    [cjd CJDException]
    )
  )

(defn parse-exception [form exception what]
  (throw (CJDException. 
           (str "Unable to parse " what " at " 
                (if-let [file (get (meta form) :file)] file "?file?")
                ":"
                (if-let [line (get (meta form) :line)] line "?line?"))
           exception)))

#_ (* Throws an exception with a supplied error message in response to
      errors detected while parsing.
      @arg form The Clojure source form within which the error was detected.
      @arg whats Zero or more strings that are concatenated to form an error messagge.
      )
(defn parse-error [form & whats]
  (throw (CJDException. 
           (str (apply str whats) " at "
                (let [{ file :file line :line } (meta form)])))))


(defn parse-method-prototype [])

(defn parse-protocol [form] 
  (make-artifact))


#_ (* An extenso incorporated by artifacts that can have docstrings @(i and) are
      willing to exert the effort to parse and record them.
      @p This is provided on the theory that docstrings may be pressed into service
      as an archaic, pitiful, sorry, decrepit fallback for artifacts that are not blessed
      with the clearly superior virtues of CJD documentation ~":-)" .)
(defextenso HasDocString [] [docstring]
  (explicit-docstring? [this] (boolean docstring))
  (explicit-docstring-of [this] docstring)
  )

#_ (* Returns the docstring of an artifact.
      @arg artifact The artifact to extract the docstring from.
      @returns The docstring, if there is one, or nil otherwise.)
(defn docstring-of [artifact]
  (or (and (satisfies? HasDocString artifact) (explicit-docstring-of artifact))
      (:doc (meta (artifact-name-of artifact)))))

#_ (* Tests whether an artifact has a docstring.
      @arg artifact The artifact to test.
      @returns True if @(arg artifact) has a docstring.)
(defn has-docstring? [artifact]
  (boolean (docstring-of artifact)))

#_ (* Protocol implemented by artifacts that have subartifacts that engender names 
      within their namespace. 
      @p For example, a protocol itself gives rise to a name within its namespace\; but
      so do its methods. As a consequence a protocol artifact would implement
      @(name), and return a list of those names on request. 
      A record implemeting the protocol, however, does not induce any new names, and
      therefore an artifact representign the record would not need to implement @(name).)
(defprotocol HasNamedSubartifacts
  #_ (* Returns a list of named subartifacts.)
  (named-subartifacts-of [this]))

#_ (* Predicate that tests whether an artifact implements @(link HasNamedSubartifacts).)
(defn has-named-subartifacts? [artifact] (satisfies? HasNamedSubartifacts artifact))

#_ (* Does a minimal parsing of a namespace definition. )
(defn parse-ns [form]
    (let [[_ name & stuff] form
          [_ & usages] (some (fn [x] (if (and (coll? x) (= (first x) :use)) x)) stuff)]
      (make-artifact usages)))

#_ (* Extenso for artifacts representing a Clojure namespace. 
      @p In a canonical state of affairs,
      a @name object exists at the root of a rough abstract syntax tree of a 
      Clojure source file. As such, most other artifacts 
      (functions, defs, macros, etc., etc.) appear as children of the
      @(name), and much of the CDJ processing is driven from here.
      )
(defextenso NamespaceBase [] [(artifacts* (ref [])) uses] 
  (artifacts-of [this] @artifacts*)
  (add-artifact [this artifact] (dosync (alter artifacts* conj artifact)))
  (set-artifacts [this artifacts] (dosync (ref-set artifacts* artifacts)))
  (uses-of [this] uses)
  )

#_ (* Artifact representing a namespace.)
(defartifact Namespace ns "namespace" [NamespaceBase] [] parse-ns)

#_ (* Artifact representing an @(il clojure.core/in-ns) declaration.
      @p We record these mostly as a method for keeping artifact namespace
      membership straight in the fact of @(il clojure.core/in-ns) declarations.
      However, @name objects are merged into their corresponding @(l Namespace)
      artifacts wherever such an artifact can be located. Consequently,
      they shouldn't generally show up, unless an undocumented external namespace
      is being extended.
      )
(defartifact In-Namespace in-ns "namespace" [NamespaceBase] [] parse-ns)

#_ (* Predicate that tests to see if an artifact is derived from @(link NamespaceBase).)
(defn namespace-derivative? [artifact] (satisfies? NamespaceBase artifact))

#_ (* Predicate that tests to see if an artifact is a @(link Namespace).)
(defn namespace-artifact? [artifact] (= (type artifact) Namespace))

#_ (* Predicate that tests to see if an artifact is a @(link In-Namespace).)
(defn in-ns-artifact? [artifact] (= (type artifact) In-Namespace))

#_ (* Artifact that represents a var, as defined by the @(c def) special form. )
(defartifact Var def "var" [] [] simple-artifact-parser)

#_ (* Artifact that represents a define-once var, as defined by @(link defonce). )
(defartifact OnceVar defonce "define-once var" [] [] simple-artifact-parser)

#_ (* Parses a "functional" form, which includes artifacts like macros in addition
      to proper functions. Functional forms include at least one, and possibly
      multiple, sets of parameters.
      @p @name is called by the driver upon encountering a top-level functor 
      that declares an artifact of appropriate type. 
      @arg form The form to be parsed.
      @returns An artifact of the type defined by the driver-defined @(c make-artifact)
      function.
      )
(defn parse-functional [form]
  (let [[_ name & contents] form
        [?doc? & after-doc] contents
        docstr (if (string? ?doc?) ?doc? nil)
        postdoc (if (string? ?doc?) after-doc contents)
        [?attrs? & after-attrs] postdoc
        attrs (if (map? ?attrs?) ?attrs? nil)
        post-attrs (if (map? ?attrs?) after-attrs postdoc)
        [?params? & body] post-attrs]
    #_(prn '--> name docstr attrs ?params?)
    (make-artifact docstr
      (if (vector? ?params?) 
          [?params?]
         (reduce 
           (fn [param-sets+ ps]
             (let [[param-set & _] ps] 
               (if (vector? param-set) 
                 (conj param-sets+ param-set)
                 (parse-error form "parse-functional: expecting parameter vector" ))))
           [] post-attrs))))
  ) ; parse-function

#_ (* Incorporated by artifacts that can have multiple sets of parameters, such as 
      functions or macros.
      )
(defextenso HasParameterSets [] [parameter-sets]
  (parameter-sets-of [this] parameter-sets))

#_ (* Artifact that describes a function, as defined by @(link defn).)
(defartifact Function defn "function" 
  [(HasDocString docstring) (HasParameterSets parameter-sets)] []  parse-functional)

#_ (* Artifact that describes a macro, as defined by @(link defmacro).)
(defartifact Macro defmacro "macro" 
  [(HasDocString docstring) (HasParameterSets parameter-sets)] []  parse-functional)

#_ (* Artifact that describes a function with optional keyed parameters, such as are used
      by @(linkto http://clojuredocs.org/clojure_contrib/clojure.contrib.def/defnk defnk).)
(defartifact KeyedFunction defnk "keyed function" 
  [(HasDocString docstring) (HasParameterSets parameter-sets)] []  parse-functional)

#_ (* Artifact that describes a multimethod declaration, as 
      defined by @(link defmulti).)
(defartifact MultimethodDef defmulti "multimethod" [] [] simple-artifact-parser)

#_ (* Artifact that describes a multimethod instance, as defined by
      @(link defmethod).)
(defartifact MultimethodMethod defmethod "multimethod implementation" [] [] simple-artifact-parser)

#_ (* A component of a @(link Protocol) artifact that describes a method of the protocol.)
(defsubartifact ProtocolMethod [HasCJDoc HasDocString] [signatures] 
  (fn [parent form doc]
    (let [[name & sigs+docstr] form
          [sigs docstr] 
          (reduce (fn [[sigs+ docstr+] item]
                    (cond
                      (vector? item) [(conj sigs+ item) docstr+]
                      (string? item) [sigs+ item]
                      :else [sigs+ docstr+]))
                  [[] nil] sigs+docstr)]
      (if name
        (make-subartifact name doc docstr sigs)))
    )
  (signatures-of [this] signatures))

#_ (* An extenso implemented by artifacts (such as protocols) that define methods—or, 
      to be more precise, method prototypes.  )
(defextenso HasMethods [] [(methods* (ref []))]
  (methods-of [this] @methods*)
  (add-method [this method] (dosync (alter methods* conj method)))
  (set-methods [this methods] (dosync (ref-set methods* methods)))
  )

#_ (* Artifact that describes a protocol, as defined by an instance of @(link defmethod).)
(defartifact Protocol defprotocol "protocol" [HasDocString HasMethods] [] 
  (fn [form] 
    (let [[_ name & opts+sigs ] form
          [?docstr? & after-doc] opts+sigs
          docstr (if (string? ?docstr?) ?docstr? nil)
          protocol (make-artifact docstr) 
          methods
          (loop [remaining-forms (if docstr after-doc opts+sigs)
                 methods+ []] 
            (let [[?doc? & ?post-doc? ] remaining-forms
                  doc (if (cjd-doc? ?doc?) ?doc? nil)
                  [method-form & post-method] (if doc ?post-doc? remaining-forms)]
              (if (empty? method-form)
                methods+
                (let [m (parse-ProtocolMethod protocol method-form doc )]
                  (if m
                    (recur
                      post-method
                      (conj methods+ m))
                    ((parse-error method-form "Expecting a method declaration: "
                                  (enquote (maxstr 50 method-form)))))))))
          ]
      (set-methods protocol methods)
      protocol))
  HasNamedSubartifacts
  (named-subartifacts-of [this] (methods-of this))
  )   ; Protocol

#_ (* An extenso that's included by artifacts that contain fields, such as 
      records or types (or extensos, for that matter).
      )
(defextenso HasFields []  [fields]
  (fields-of [this] fields)
  )

(defn method-signature [method params] (str method "//" (count params)))

#_ (* Component representing the implementation of a specific signature of 
      a method, as in a type or record. 
      @field parameters A vector of the parameter symbols in the method signature.
      @field implementer The artifact that effectively implements the methods
      (e.g., a type or record) which
      (as with a @(link Poioo)) is not necessarily the same as a 
      @(link MethodImplementation)'s parent object.
     )
(defsubartifact MethodImplementation [HasCJDoc] [parameters implementer] 
  (fn [parent implementer forms]
    (let [[?doc? & remaining] forms
          doc (if (cjd-doc? ?doc?) ?doc? nil)
          post-doc (if doc remaining forms)
          [[name params & _] & unparsed-forms] (if (seq? (first post-doc)) post-doc nil)]
      (if (and (symbol? name) (vector? params))
        [(make-subartifact name doc params implementer) unparsed-forms]
        [nil forms])))
  (parameters-of [this] parameters)
  (implementer-of [this] implementer)
  (signature-of [this] (method-signature name parameters)))

#_ (* An extenso used when a component (as a type or record) multiple 
       method implementations.
      
      @field method-implementations* A reference to a collection of 
      @(link MethodImplementation) objects.
      )
(defextenso HasMethodImplementations []  [(method-implementations* (ref []))]
  (method-implementations-of [this] @method-implementations*)
  (add-method-implementation [this m-i] 
    (dosync (alter method-implementations* conj m-i))))

#_ (* A subartifact that represents a "protocol-or-interface-or-Object", such as is
      specified in a record or type definition.)
(defsubartifact Poioo [HasMethodImplementations] []
  (fn [parent forms]
    (let [[poioo-name & more-forms] forms
          poioo (make-subartifact poioo-name)]
      (if (symbol? poioo-name)
        (loop [unparsed-forms more-forms]
          (let [[m-i unparsed-forms*] 
                (parse-MethodImplementation poioo parent unparsed-forms)]  
            (if m-i
              (do
                (add-method-implementation poioo m-i)
                (recur unparsed-forms*))
              [poioo unparsed-forms])))
        [nil forms])))
  ) ; Poioo 

#_ (* An extenso incorporated by artifacts that have a "protocol-or-interface-or-Object"
      substructure, such as a record or type.)
(defextenso HasPoioos [] [(poioos* (ref []))]
  (poioos-of [this] @poioos*)
  (add-poioo [this poioo] (dosync (alter poioos* conj poioo)))
  (set-poioos [this poioo-seq] (dosync (ref-set poioos* poioo-seq))))


#_ (* Parses a sequence of protocol or interface or object names followed by 
      method implementations, as is common to records, types, and various
      derived constructions.
      @arg parent The parent artifact.
      @arg poioo-specs A list of forms to be parsed.
      @returns The collection of @(link Poioo) objects parsed from @(arg poioo-specs).)
(defn parse-poioos [parent poioo-specs]
  #_(println "parse-poioos:" parent poioo-specs) 
  (let [deopted-specs 
        (loop [[opt optval & more-specs :as deopted] poioo-specs]
          (if (keyword? opt)
            (recur more-specs)
            deopted))]
    (loop [unparsed-specs deopted-specs
           poioos+ []]
      (if (empty? unparsed-specs)
        poioos+
        (let [[poioo unparsed-specs*] (parse-Poioo parent unparsed-specs)]
          (if poioo
            (recur unparsed-specs* (conj poioos+ poioo))
            (parse-error unparsed-specs 
                         "Expecting a protocol, interface, or Object:" 
                         (enquote poioo))))))))

#_ (* Parses a record or type declaration, from a @(link defrecord) or @(link deftype) 
      declaration, with the ultimate intent of extracting protocol/interface 
      and method documentation and content. 
      @arg form The form containing the defrecord.
      @return A record or type object, as appropriate.
      )
(defn parse-record-or-type [form]
  (let [[_ name field-vec & poioo-specs] form
        rec (make-artifact field-vec)
        poioos (parse-poioos rec poioo-specs)]
    (set-poioos rec poioos)
    rec)) 

#_ (* Artifact that describes a record, as defined by an instance of @(link defrecord).)
(defartifact Record defrecord "record" [HasFields HasPoioos] [] parse-record-or-type)

#_ (* Artifact that describes a type, as defined by an instance of @(link deftype).)
(defartifact Type deftype "type" [HasFields HasPoioos] [] parse-record-or-type)


