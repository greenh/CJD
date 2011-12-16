#_ ( Copyright (c) Howard Green 2011. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )

#_ (* Provides the basis for defining @(i artifacts), objects that represent 
      Clojure constructs susceptible to carrying CJD comments.
      @p @(i Artifact) has two major interpretations.
      From one perspective, artifacts are representations of
      various defined Clojure constructs, such as vars, functions, macros, records, etc.,
      and contain content describing the construct, among which is the CJD comment 
      for the form. The definition of an artifact may include second-level structures in
      the form of @(i subartifacts). These are used, for example, to represent
      methods.
      @p From another perspective, artifacts are nodes in a rough abstract 
      syntax tree parsed from a Clojure source file. This tree (or forest of trees,
      in the case of a multi-file CJD run) inform all subsequent steps of 
      generating the finished documentation.
      )
(ns cjd.artifact-base
  (:use
    [cjd.util.extensomatic]
    [cjd.util.string-utils]
    [clojure.pprint]
    )
  (:import 
    [cjd CJDException]
    )
  )

(defn- error [& why-bits]
  (throw (CJDException. (apply str why-bits))))

;(def msgstate false)

#_ (* Output function for artifact definition process. 
      @p This does nothing by default, but can be rebound to something
      more interesting if desired.
      )
(defn artifact-msg [& things])

;(defmacro artifact-msg [& things]
;  `(if msgstate (println (str ~@things))))

(def all-artifacts* (ref { }))
(defn init-artifacts []  (dosync (ref-set all-artifacts* { })))

#_ (* Given a fully-qualified artifact name, fetches the corresponding artifact.
      @arg fq-sym A symbol containing the fully-qualified artifact name.
      @returns Tha named artifact, or nil if there ain't one.
      )
(defn get-artifact [fq-sym] (get @all-artifacts* fq-sym))

#_ (* Predicate that tests whether a form is a CJD documentation form.
      @arg form The form to test.
      @return True if the form is (or appears to be) CJD documentation.
      )
(defn cjd-doc? [form] (and (seq? form) (= (first form) 'cjd!doc)))


#_ (* Multimethod for dispatching top-level Clojure forms to artifact-specific 
      methods for parsing and AST construction. Note that specific methods
      for @(name) are normally constructed automatically by @(link defartifact).
      )
(defmulti cjd-artifact (fn [[type-sym & _] ns-ent doc-form] type-sym) :default nil)

(defmethod cjd-artifact nil [_ _ _] nil)

#_ (* Instantiates an artifact within an artifact parsing function. 
      @(p @name is rebound dynamically—usually with additional 
          parameters—within the @(link defartifact) 
          dispatch mechanism, and @(i is only meaningful in an artifact-parsing context.) 
          See @(link defartifact) for details.)
      @returns A new artifact object of a type determined by the context 
      in which it is called.
     )
(defn make-artifact [] 
  (throw (CJDException. "make-artifact called outside of defartifact")))

#_ (* Instantiates a subartifact within a subartifact parsing function.
      
      )
(defn make-subartifact [] 
  (throw (CJDException. "make-subartifact called outside of defsubartifact")))


(def parent nil)

#_ (* Minimal parsing function for use with @(link defartifact).
      
      @p In situations where an artifact type has no documented internal 
      structure (or when you're going to implement it all "later", 
      but would like the documentation for the top level 
      element @(i now)), @(name) is a handy way to get artifact objects 
      appropriately instantiated without further ado.
      
      @p Note that this only works if the artifact is declared with no
      extensos or local fields.
      
      @returns An appropriate artifact. The specific artifact type is determined
      from the context of use by @(link defartifact).
     )
(defn simple-artifact-parser [_] (make-artifact))

#_ (* Protocol implemented by all artifact types. Note that this is implicitly
      included in the artifact definition in @(link defartifact).)
(defprotocol Artifact 
  #_ (* Returns the artifact representing the namespace within which the
        artifact is defined.)
  (namespace-of [this] )
  #_ (* Returns the name of the artifact. This is a @(i symbol), not a string.)
  (artifact-name-of [this] )
  #_ (* Returns a string containing the name of the file in which the 
        artifact was defined.)
  (defined-in [this] )
  #_ (* Returns the line number where the artifact was defined, as 
        an integer.)
  (defined-at [this] )
  )

#_ (* Extenso defining a default implementation of the @(link Artifact) protocol.)
(defextenso ArtifactBase [] [artifact-name artifact-namespace def-file def-line]
  Artifact
  (namespace-of [this] artifact-namespace)
  (artifact-name-of [this] artifact-name)
  (defined-in [this] def-file)
  (defined-at [this] def-line)
  )

#_ (* Protocol for accessing descriptive information about the implementing entity. 
      )
(defprotocol Descriptive
  #_ (* Returns A short string that describes what the implementing artifact 
        represents.
        @p For example, an artifact representing a function might return "Function".)
  (descriptive-of [this])
  )

#_ (* Extenso used for all artifacts that can potentially contain CDJ documentation.)
(defextenso HasCJDoc [] [doc-form]
  #_ (* Returns the form containing the CJD documentation for the artifact.)
  (doc-form-of [this] doc-form)
  #_ (* Returns true if the artifact has a CJD documentation comment.)
  (has-doc? [this] doc-form)
  )

#_ (* Defines a CJD "artifact", an object class that representes a specific kind 
      of entity declared in Clojure source code by a well-known symbol. 
      Defining an artifact wires the artifact type into the CJD 
      infrastructure so that will be recognized during processing, and  
      creates the necessary structures to record salient
      information about instances of the artifact as they're encountered. 
      As a consequence, @name represents a major extension point for CJD.
      @(p @(c clojure.core) specifies a large selection of entities that give
          rise to artifacts, such as variables (declared by @(c ~"def")), 
          functions (@(c ~"defn")), macros (@(c ~"defmacro")), and many others.
          These are mostly specified in @(link cjd.core-artifacts).)  
      
      @(p Invoking @(name) to define an artifact gives rise to several effects. 
          @(ul 
             @(li It defines a Clojure record type (using @(c defrecord)) which
                  has the name given by @(arg artifact-name).
                  The current implementation is based on the 
                  @(link defconstructo constructo) and @(link defextenso extenso) 
                  mechanisms to generate the record.)
             @(li It generates a method and adds it to the @(link cjd-artifact)
                  multimethod. This method handles the boilerplate processing for the 
                  artifact, and is called by the CJD system whenever it encounteres
                  a top-level form headed by @(arg keying-symbol) while processing
                  source files. Additionally, the method calls @(arg parse-fn) to do
                  any further parsing of the source form.)
             )
          )
      @arg artifact-name A symbol denoting the name of the artifact. 
      @arg keying-symbol The symbol used to identify instances of the artifact,
      e.g. def, defn, defmacro, defartifact, and the like.
      @arg descriptive A @(i very) short string that describes what this artifact 
      represents, e.g. "function" for defn, "macro" for defmacro, etc.
      @arg extensos A (possibly empty) sequence of extensos to be included in the 
      artifact, optionally including field specifications and initializations. 
      This uses the same specificational conventions as @(link defconstructo).
      @arg local-fields A (possibly empty) sequence of local fields, possibly
      including initialization values. This follows the same specificational 
      conventions as @(link defconstructo).
      @(arg parse-fn A function of the form @(fun (fn [form])), which
            the CJD infrastructure invokes whenever it encounters a top-level 
            form with the type declared by the artifact.
            @arg form A form with the functor given by @(arg keying-type).
            @returns An object of the type given by @(arg artifact-name).) 
      @arg local-additions Local method implementations,
      followed by local implementations of protocols, interfaces, and/or 
      @(link java.lang.Object) methods. This follows the same conventions as
      in @(link defconstructo).
      @return The class object generated for the artifact, ala @(c defrecord).
      @see The @(link cjd.core-artifacts) namespace contains many examples of 
      the use of @(name).
      )
(defmacro defartifact [artifact-name keying-symbol descriptive
                       [& extensos] [& local-fields] parse-fn & local-additions]
  (let [constructo-form
        `(defconstructo ~artifact-name 
           [ArtifactBase HasCJDoc ~@extensos]
           ~(vec local-fields)
           ~@local-additions
           Descriptive
           (descriptive-of [this] ~descriptive)
           java.lang.Object
           (~'toString [~'this] (str ~(str "#<" artifact-name " ") ~'artifact-name ">"))
           )
        
        _ (-ppp "defartifact constructo-form: " constructo-form) 
        
        [init-fields uninit-fields]
        (extenso-field-munger 'defartifact extensos local-fields)
        
        method-form 
        `(defmethod cjd-artifact (quote ~keying-symbol) [form# ns-artifact# doc-form#]
           (let [[~'_ name#] form#
                 {file# :file line# :line} (meta form#)] 
             (artifact-msg ~(str keying-symbol " ") name# 
                      (if doc-form# " with doc" " no doc"))
             (binding [make-artifact 
                       (fn ~uninit-fields 
;                         (~(symbol (str "make-" artifact-name)) name# ns-artifact# 
;                              file# line#
;                              doc-form#  
;                              ~@uninit-fields)
                         (let [art# 
                               (~(symbol (str "make-" artifact-name)) 
                                 name# ns-artifact# file# line#
                                 doc-form# ~@uninit-fields)
                               fqid# (if ns-artifact# 
                                       (symbol (str (artifact-name-of ns-artifact#) 
                                                    "/" name#))
                                       name#)]
                           (dosync (alter all-artifacts* assoc fqid# art#))
                           art#)
                         )]
               (~parse-fn form#))))
        
        _ (-ppp "defartifact method-form: " method-form) 
        ]
    `(do
       ~constructo-form
       ~method-form
       (defmethod clojure.core/print-method ~artifact-name [o#, ^java.io.Writer w#] 
         (.write w# (.toString o#)))
       (defmethod clojure.pprint/simple-dispatch ~artifact-name [x#] 
         (print (.toString x#)))
       ~artifact-name
       )))


#_ (* Base extenso for a subartifact.
      )
(defextenso SubartifactBase [] [parent subartifact-name source-line]
  #_ (* Returns the artifact or subartifact that is the parent or superior of
        this one.)
  (parent-of [this] parent)
  Artifact
  (namespace-of [this] (namespace-of parent))
  (artifact-name-of [this] subartifact-name)
  (defined-in [this] (defined-in parent))
  (defined-at [this] source-line)
  )


#_ (* Defines a CJD @(i subartifact), an object class that defines the representation
      of some type of documentable element subordinate to an artifact. 
      
      @p Artifacts are used to represent top-level forms such as defined 
      functions, macros, protocols, or many others. Subartifacts, on the other
      hand, are used to represent subordinate elements of artifacts, such 
      methods within a protocol.
      As such, a subartifact always has a parent object, which may be an 
      an instance of artifact or componnt, as appropriate.
      
      
      @arg subartifact-name The name of the subartifact type being defined.
      
      @arg extensos A set of extensos used to construct the subartifact.
      @arg local-fields A set of fields to be defined as part of the subartifact. These 
      fields are in addition to any defined by the extensos, and may include
      initializations, as defined for constructos.
      @(arg parse-fn A function of the form @(fun (fn [parent source-form & params])),
            where 
            @arg parent The parent artifact (or subartifact).
            @arg source-form A form from the source input stream to be parsed.
            A function that the @(c parse- :nb @(i subartifact-name)) 
            calls to parse source forms.
            @arg params Additional parameters as needed\; these are by agreement
            between the parent artifact's parsing function and the subartifact's.
            @returns An object of the subartifact's type.)
      @(arg local-adds A (possibly empty) set of methods to be defined for the 
            the subartifact. 
            @p These local methods may be followed by a sequence of protocol  
            interface names, or @(link java.lang.Object), each followed by 
            method implementations. Note that  
            this is exactly the same syntax used for record and type definitions!)
      @returns The class object for the record defined by the subartifact.)
(defmacro defsubartifact [subartifact-name [& extensos] [& local-fields] parse-fn & local-adds]
  (let [constructo-form
        `(defconstructo ~subartifact-name 
           [SubartifactBase ~@extensos] 
           ~(vec local-fields)
           ~@local-adds
           java.lang.Object
           (~'toString [~'this] (str ~(str "#<" subartifact-name " ") ~'subartifact-name ">"))
           )
        
       _ (-ppp "defsubartifact constructo-form: " constructo-form) 
        
        [init-fields uninit-fields]
        (extenso-field-munger 'defartifact extensos local-fields)
        
        parse-function-form
        `(defn ~(symbol (str "parse-" subartifact-name)) [~'parent ~'source-form & ~'params]
           (binding [parent ~'parent
                     make-subartifact 
                     (fn [~'name ~@uninit-fields]
                       (artifact-msg ~(str "cjd: --- " subartifact-name " ") ~'name)
                       (~(symbol (str "make-" subartifact-name))
                         ~'parent ~'name 
                         (get :line (meta ~'source-form))
                         ~@uninit-fields))]
             (apply ~parse-fn ~'parent ~'source-form ~'params)))
        
        _ (-ppp "defsubartifact parse-function-form: " parse-function-form) 
        ]
    `(do
       ~constructo-form
       ~parse-function-form
       (defmethod clojure.core/print-method ~subartifact-name [o#, ^java.io.Writer w#] 
         (.write w# (.toString o#)))
       (defmethod clojure.pprint/simple-dispatch ~subartifact-name [x#] 
         (print (.toString x#)))
       ~subartifact-name
       ))
  ) ; defsubartifact

