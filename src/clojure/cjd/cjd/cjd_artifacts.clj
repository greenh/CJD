#_ ( Copyright (c) 2011 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Extends the core CJD artifact model to include a few additional artifacts, 
      including extensos, constructos, and, of course, artifacts themselves.
      )

(ns cjd.cjd.cjd-artifacts
  (:use 
    [cjd.artifact-base]
    [cjd.core-artifacts]
    [extensomatic.extensomatic]
    )
  )

#_ (* Parses a sequence of method implementations @(i sans) any pooio declaration,
      as is found in the local methods of an extenso or constructo.
      @p Note that @name directly updates the method implementation list in
      @(arg parent)!
      @arg parent The parent artifact object, which must support the 
      @(link HasMethodImplementations) protocol.
      @arg forms The sequence of forms to parse from.
      @returns A sequence of unparsed forms at the tail of @(arg forms).)
(defn parse-method-implementations [parent forms]
  (loop [unparsed-forms+ forms]
    (let [[m-i unparsed-forms*] (parse-MethodImplementation parent parent unparsed-forms+)]  
      (if m-i
        (do
          (add-method-implementation parent m-i)
          (recur unparsed-forms*))
        unparsed-forms+))))

#_ (* Parsing function for constructos and extensos.
      @arg form The form to be parsed
      @returns An appropriate artifact object.
      )
(defn parse-extensomatics [form]
    (let [[_ name &composed-extensos &local-fields & local-protos-methods] form
          artifact (make-artifact)
          unparsed-forms (parse-method-implementations artifact local-protos-methods)
          
          poioos (parse-poioos artifact unparsed-forms)]
      (set-poioos artifact poioos)
      artifact))

#_ (* Artifact representing an extenso, as defined by @(link defextenso).
      )
(defartifact Extenso defextenso "extenso" 
  [HasMethodImplementations HasPoioos] [] parse-extensomatics
  HasNamedSubartifacts
  (named-subartifacts-of [this] (method-implementations-of this)))

#_ (* Artifact representing a constructo, as defined by @(link defconstructo).)
(defartifact Constructo defconstructo "constructo" 
  [HasMethodImplementations HasPoioos] [] parse-extensomatics)

#_ (* Parsing function for artifacts.
      @arg form The form to be parsed
      @returns An @(link Artifactoid) object.
      )
(defn parse-artifact [form]
    (let [[_ name keying-symbol descriptive &composed-extensos 
           &local-fields parse-fn & local-protos-methods] form
          artifact (make-artifact)
          unparsed-forms (parse-method-implementations artifact local-protos-methods)
          poioos (parse-poioos artifact unparsed-forms)]
      (set-poioos artifact poioos)
      artifact))

#_ (* Artifact representing a CJD artifact, as defined by @(link defartifact).)
(defartifact Artifactoid defartifact "artifact" 
  [HasMethodImplementations HasPoioos] [] parse-artifact)

#_ (* Parsing function for subartifacts.
      @arg form The form to be parsed
      @returns A @(link Subartifactoid) object.
      )
(defn parse-subartifact [form]
    (let [[_ name  &composed-extensos &local-fields parse-fn & local-protos-methods] form
          artifact (make-artifact)
          unparsed-forms (parse-method-implementations artifact local-protos-methods)
          poioos (parse-poioos artifact unparsed-forms)]
      (set-poioos artifact poioos)
      artifact))

#_ (* Artifact representing a CJD subartifact, as defined by @(link defsubartifact).)
(defartifact Subartifactoid defsubartifact "subartifact" 
  [HasMethodImplementations HasPoioos] [] simple-artifact-parser)
