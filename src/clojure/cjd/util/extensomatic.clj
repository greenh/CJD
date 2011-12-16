#_ ( Copyright (c) Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )

#_ (* Provides a set of macros for constructing data-containing objects in
      Clojure. It allows construction of two principle kinds of entities,
      @(i extensos) and @(i constructos), which are significantly enhanced
      analogs of Clojure's native protocol and record entities respectively.
      Extensos and constructos are defined using the @(link defextenso) 
      and @(link defconstructo) macros, and result in protocols and records that
      are in all respects fully compatible with those defined with 
      @(link clojure.core/defprotocol) and @(link clojure.core/defrecord).
      
      @(p An extenso is a fragment of a record. An extenso definition can 
          specify several kinds of content\:
          @(ul
             @li A set of field names, representing data elements that are 
             accessible by name within the scope of the extenso.
             @li Initialization values for selected fields. 
             @(li Method implementations. These fall into two categories\:
                  @(ul
                     @(li "local" methods define both interface and implementation 
                          at the same time, much like a noninherited method in Java.)
                     @(li Methods that are implementations of protocols or interfaces,
                          or overrides of @(link java.lang.Object)\; these abide
                          by the same rules as method implementations within
                          @(link defrecord).)))  
             @li Other extensos, to be incorporated or "composed" into the extenso 
             being defined. These may include initialization values for fields
             of the composed extensos that are not initialized.)
          )
      @p From an implementation perspective, each extenso gives rise to a standard
      Clojure protocol that includes method signatures for each of the extenso's 
      local methods. And much like a protocol, an extenso doesn't actually give
      rise to any sort of instantiable object, but merely serves as a place
      to record information about the extenso—which happens to include essentially
      the entire content of the extenso.
      
      @p The fact that previously defined extensos can be composed into new extensos
      gives rise to a hierarchy of sorts. An extenso accretes all of the fields,
      field initializations, local method implementations, and protocol and / or 
      interface-based method implementations of its composed extensos\: it is, quite
      literally, a copy-and-paste composition process.
      
      @p  
      A constructo is, for all intents and purposes, a Clojure record (or type)
      defined by alternate means. 
      
      
      Like an extenso, a constructo's definition can contain fields, field 
      initializations, locally-defined or protocol / interface / Object-derived method 
      implementations, and composed extensos.
      
      
      
      
      
      )

(ns cjd.util.extensomatic
  (:import
    [cjd.util ParseException]
    )
  (:use 
    [cjd.util.string-utils]
    [clojure.pprint]))

#_ (* Debug output flag for the extenso-constructo definition process. 
      @p Setting @name to true causes massive amounts of output to spew forth whenever
      one of the defintition macros is invoked.
      )
(defonce *-o-debug-print* false)

(defn extenso-debug [tf] (def *-o-debug-print* tf) tf)

(defn -pln [& stuff]  (if *-o-debug-print* (apply println stuff)) )
(defn -ppp [caption form] (if *-o-debug-print* (do (println caption) (println form))))


(defn- error [msg & why-bits]
  (throw (ParseException. (apply str msg (if-not (empty? why-bits) 
                                         (enquote (maxstr 80 why-bits)))))))

#_ (* Specifies the prefix used for creating constructor functions for constructos.
      @p By default, the prefix is "make-", and thus the constructor function for 
      a constructo named @(c MyConstructo) would be @(c make-MyConstructo).
      )
(def *new-prefix* "make-")

(defn var-sym [var]
  (let [{ sym-ns :ns sym-name :name} (meta var)]
    (symbol (name (ns-name sym-ns)) (name sym-name))))

(defn extenso-var [extenso namespace] 
  (var-get (ns-resolve namespace extenso)))

(defn extenso-get 
  ([extenso field]
    (if (var? extenso)
      (get (var-get extenso) field)
      (extenso-get extenso *ns* field)))
  ([extenso namespace field] 
    (if (symbol? namespace)
      (require namespace))
    (get (extenso-var extenso namespace) field)))

(defn extenso-init-fields [extenso] (extenso-get extenso :extenso-init-fields))
(defn extenso-fields [extenso] (extenso-get extenso :extenso-fields))
(defn extenso-methods [extenso] (extenso-get extenso :extenso-methods))

(defprotocol ExtensomaticInfo
  #_ (* The raw vector of composed extensos. Contains not only extensos, but
        any fields and initializations as well.)
  (raw-extensos-of [this])
  #_ (* Returns a sequence of composed extensos. This is a sequence of 
        fully qualified symbols.)
  (composed-extensos-of [this])
  #_ (* Returns a sequence of local field name symbols—just names, no initialization
        data.)
  (local-fields-of [this])
  #_ (* Returns a sequence of (field-name-symbol, initialization value) tuples.)
  (init-fields-of [this])
  #_ (* Returns a sequence of symbols of names of uninitialized fields.)
  (uninit-fields-of [this] )
  #_ (* Returns the local poioo + method text for the extenso. )
  (local-methods-of [this] )
)

(defprotocol Extenso-Info
  #_ (* Returns the extenso's name as a symbol.)
  (extenso-name-of [this])
  #_(* Returns the extenso's namespace as a symbol.)
  (extenso-namespace-of [this])
  #_ (* Returns the complete poioo + method text for the extenso. This is notionally 
        incorporated verbatim by a constructo or another extenso that 
        composes this one.)
  (all-methods-of [this] )
  )

#_ (* Given a name for an extenso, resolves the name and retrieves the 
      extenso's @(link ExtensoInfo) object.
      @arg extenso A symbol containing the extenso's name.
      @arg namespace An optional symbol containing namespace
      @returns The extenso's @(link ExtensoInfo) object.
      )
(defn extenso-info 
  ([extenso] (extenso-get extenso :extenso-info))
  ([extenso namespace] 
    #_(prn 'extenso-info extenso (type extenso) namespace (type namespace))
    (extenso-get extenso namespace :extenso-info)))


#_ (* Contains information describing an extenso, useful for reflection
      and for definition of constructos and further extensos.
      @p Notionally, @(link defextenso) generates an @name object
      for each extenso that's defined, and makes it accessible 
      via the :extenso-info key in the var named by the extenso
      (which is actually the protocol object defined by the 
       extenso).) 
(defrecord ExtensoInfo 
  [name namespace raw-extensos composed-extensos 
   local-fields initialized-fields uninitialized-fields 
   local-methods-text methods-text]
  ExtensomaticInfo
  (raw-extensos-of [this] raw-extensos)
  (composed-extensos-of [this] composed-extensos)
  (local-fields-of [this] local-fields)
  (init-fields-of [this] initialized-fields)
  (uninit-fields-of [this] uninitialized-fields)
  (local-methods-of [this] local-methods-text)
  
  Extenso-Info
  (extenso-name-of [this] name)
  (extenso-namespace-of [this] namespace)
  (all-methods-of [this] methods-text)
  
  java.lang.Object 
  (toString [this] ("#<ExtensoInfo " name )))


(def bogus-leader "__XXX!__")
(defn bogus-method? [method-sym] (.startsWith (name method-sym) bogus-leader))

#_ (* Checks to see if a symbol actually in fact represents an extenso.
      @arg extenso A symbol possibly representing the name of an extenso.
      @returns @(c true), if the symbol appears to represent an extenso.
      )
(defn extenso? [extenso] 
  (if-let [e-var (resolve extenso)]
    (if-let [e-val (var-get e-var)]
      (if (and (map? e-val)
               (get e-val :extenso-init-fields) 
               (get e-val :extenso-fields) 
               (get e-val :extenso-fields)) 
        true))))

#_ (* Examines a list of extensos, notionally from the "composed-extensos" parameter
      of an extenso or constructo declaration, and checks to see if each 
      extenso is in fact defined and appears to be a legitimate extenso.
      Terminates with an exception if anything in @(arg extenso-forms) is not
      an extenso.
      @arg source A symbol that indentifies the caller for error reporting purposes 
      @arg extenso-forms A sequence of extenso forms, each of which might be 
      just the extenso name, or a list headed by the extenso name and including
      fields.
      @returns A sequence of vars of the extensos in @(arg extenso-forms). 
      )
(defn check-extensos [source extenso-forms]
  (reduce
    (fn [extenso-vars+ extenso-form ]
      (let [extenso (cond (symbol? extenso-form) extenso-form
                          (and (seq? extenso-form)) (first extenso-form)
                          :else (error (str source ": expecting an extenso name or form: ") 
                                       extenso-form))]
        (if-not (symbol? extenso)
          (error (str source ": not a valid extenso name: ") extenso ))
        (if-not (extenso? extenso)
          (error (str source ": not an extenso: ") extenso))
        (conj extenso-vars+ (resolve extenso))))
    [] extenso-forms))

(defn- check-fields [source local-fields]
  (doseq [field local-fields]
    (if-not (symbol? field)
      (let [[field-name & glop] field]
        (if-not (and field-name (symbol? field-name) (> (count glop) 0))
          (error (str source ": unparseable field spec: ") (enquote field)))))))


#_ (* Develops a list of fields that are initialized, and another of fields that are 
      not, based 
      on a list of extenso-forms and local fields as supplied to @(link defconstructo) 
      or @(link defextenso).
      @p @name starts by processing the field lists (initialized and not) from each 
         composed-extenso, and merges any initializations spec'd as part of the local
         extenso.
         
      @arg composed-extensos A list of composed-extenso forms which specify an extenso,
      specify the extenso and repeat its uninitialized fields, and/or supply
      initialization values to those fields.
      
      @arg local-fields A list of local field forms, which declare a local field,
      optionally initialize it, and optionally include additional keyword-value pairs,
      which are ignored.
 
      @(returns @(form [init-fields uninit-fields]), where\: 
                @arg init-fields A sequence of @(form (field-name init-value)) pairs, where 
                @(arg field-name) is the name of the field, and @(arg init-value) is its 
                intialization value.
                @arg uninit-fields A sequence of names of uninitialized fields.) 
     )
(defn extenso-field-munger [source composed-extensos local-fields]
  (check-extensos source composed-extensos)
  (check-fields source local-fields)
  (let [[init-fields* uninit-fields*]
        (reduce  
          (fn [[init-fields+ uninit-fields+] extenso-form]
            (let [p-extenso (if (symbol? extenso-form) extenso-form (first extenso-form))
                  p-fields (if (symbol? extenso-form) nil (rest extenso-form))
                  ex-init-fields (extenso-init-fields p-extenso)
                  ex-uninit-fields (extenso-fields p-extenso)
                  
                  ;; compare fields supplied as parameters to the p-extenso's uninitialized
                  ;; parameter list. Output is a list of initialized fields and 
                  ;; sequence of uninitialized fields; these are added to the 
                  ;; corresponding lists for the extenso.
                  
                  [in* un*] 
                  (if (empty? p-fields) 
                    [ex-init-fields ex-uninit-fields]
                    (if (not= (count p-fields) (count ex-uninit-fields))
                      (error (str source ": expecting " 
                                  (count ex-uninit-fields) " fields for " 
                                  p-extenso ": ") extenso-form)
                      ;;;;; per-composed-extenso-field reduction
                      (reduce  
                        (fn [[in+ un+] [p-f u-f]]
                          (if-not (or (symbol? p-f) (and (seq? p-f) (symbol? (first p-f))))
                            (error (str source ": unrecognized field form "
                                        (enquote (maxstr 20) p-f) " in " 
                                        p-extenso ": ") extenso-form))
                          (let [uname (if (symbol? u-f) u-f (first u-f))
                                pname (if (symbol? p-f) p-f (first p-f))]
                            (if-not (= pname uname)
                              (error (str source ": field names don't match: " 
                                          pname " found, " uname " expected in " 
                                          p-extenso ": ") extenso-form))
                            (cond 
                              (symbol? p-f) [in+ (conj un+ u-f)]
                              
                              ; if the parameter-form contains an even number of
                              ; elements, the second is the init value. If the number
                              ; is odd, there's no init value.
                              (odd? (count p-f))
                              (if (symbol? u-f)
                                [in+ (conj un+ p-f)]
                                (let [[_ & p-opts] p-f
                                      [_ & u-opts] u-f]
                                  [in+ (conj un+ (concat (list uname) u-opts p-opts))]))
                              
                              :else  ; even number 
                              (if (symbol? u-f)
                                [(conj in+ p-f) un+]
                                (let [[_ p-init & p-opts] p-f
                                      [_ & u-opts] u-f]
                                  [(conj in+ (concat (list uname p-init) u-opts p-opts)) un+]))
                              )
                            ))
                        [ex-init-fields []] 
                        (partition 2 (interleave p-fields ex-uninit-fields)))))]
              [(vec (concat init-fields+ in*)) (vec (concat uninit-fields+ un*))]))
          [[] []] composed-extensos)
        
        [init-fields uninit-fields]
        (reduce 
          (fn [[init-fields+ uninit-fields+] field]
            (cond
              (or (symbol? field)
                  (and (seq? field) (symbol? (first field)) (odd? (count field)))) 
              [init-fields+ (conj uninit-fields+ field)]
              
              (and (seq? field) (symbol? (first field)) (even? (count field)))
              [(conj init-fields+ field) uninit-fields+]
                
              :else   
              (error (str source ": unrecognized field form -- ") field)))
          [init-fields* uninit-fields*]
          local-fields)]
    [init-fields uninit-fields]))


#_ (* Defines an @(i extenso), a fragment of a record containing fields,
      field initializations, method implementations, and specifications of
      previously defined extensos to be incorporated ("composed") into the extenso at hand. 
      An extenso, once defined, can be composed into subsequently defined
      extensos, or into @(i constructos), instantiable data objects defined
      by means of @(link defconstructo).
      
      
      @(arg extenso-name The name of the extenso, which is equivalently the
      name of the Clojure protocol defined by the extenso.)
      @(arg &composed-extensos 
            A (possibly empty) vector of extenso specifications to be composed
            into the extenso. Each extenso spec is either an extenso name, or
            list of the form @(form (comp-extenso-name & comp-field-specs)), where
            @arg comp-extenso-name The name of the composed extenso.
            @(arg comp-field-specs 
                  A sequence of field specs. Each field spec is either
                  the name of an uninitialized field of the composed extenso, 
                  or a tuple of 
                  the form @(form (field-name init-value)), where
                  @arg field-name The name of the field
                  @arg init-value The value to which the field is to be initialized.)
            Note that if field specs are provided at all, a field spec @(b must) be
            provided for @(i every) uninitialized field of the composed extenso, or
            @name will generate an error. ) 
      @(arg &local-fields A (possibly empty) vector of fields to be added to any defined 
      (implicitly or explicitly) by @(arg &composed-extensos). Each field is either
      the name of a field, or a tuple of the form @(form (field-name init-value)),
      where\:
      @arg field-name The name of the field
      @arg init-value The value to which the field is to be initialized.)
      @arg local-protos-methods A (possibly empty) list of local method 
      implementations, followed by protocol-name / method implementation 
      sequences in the same format as @(link defrecord).
      )
(defmacro defextenso [extenso-name &composed-extensos &local-fields & local-protos-methods]
  (if-not (symbol? extenso-name) 
    (error "defextenso: Extenso name must be a symbol: " extenso-name))
  (if-not (vector? &composed-extensos)
    (error "defextenso: &composed-extensos must be a vector: " &composed-extensos))
  (if-not (vector? &local-fields)
    (error "defextenso: &local-fields must be a vector: " &local-fields))
  (let [[& composed-extensos] &composed-extensos
        [& local-fields] &local-fields
        composed-vars (check-extensos 'defextenso composed-extensos) 
        ]
    (check-fields 'defextenso local-fields)
    (let [pre-local-methods 
          (take-while (fn [item] (not (symbol? item))) local-protos-methods)
          
          local-methods pre-local-methods
          #_(if (empty? pre-local-methods) 
            [(list (symbol (str bogus-leader 
                               extenso-name "_"
                               (.replaceAll (str (ns-name *ns*)) "\\." "_")))
                  '[this])])
          #_ (-ppp "defextenso local-methods: " local-methods)
          
          proto-form 
          (concat 
            (list 'defprotocol extenso-name )
            (map (fn [method-form] 
                   (let [[method-name [& params] & _] method-form]
                     (if (zero? (count params))
                       (error "defextenso: Method " method-name "must have at least one parameter"))
                     (list method-name (vec params))))
                 local-methods))
          _ (-ppp (str ";;;;;;;;;;;; " extenso-name " proto-form ;;;;;;;;;;;") proto-form)
          
          [init-fields uninit-fields] (extenso-field-munger 'defextenso composed-extensos local-fields)
          
          protos-methods
          (concat 
            (reduce
              (fn [protos-methods+ extenso-form]
                (let [p-extenso (if (symbol? extenso-form) extenso-form (first extenso-form))]
                  (concat protos-methods+ (extenso-methods p-extenso))))
              [] composed-extensos)
            [extenso-name]
            local-protos-methods)
          
          proto-info-form
          `(def ~extenso-name 
             (assoc ~extenso-name 
                    :extenso-info
                    (ExtensoInfo.
                      ~(list 'quote extenso-name)
                      ~(list 'quote (ns-name *ns*))
                      ~(list 'quote composed-extensos)
                      ~(list 'quote composed-vars)
                      ~(list 'quote 
                             (reduce 
                               (fn [local-fields+ field-spec]
                                 (conj local-fields+
                                       (if (symbol? field-spec)
                                         field-spec (first field-spec))))
                               [] &local-fields))
                      ~(list 'quote (vec init-fields))
                      ~(list 'quote (vec uninit-fields))
                      ~(list 'quote local-protos-methods)
                      ~(list 'quote protos-methods)
                      )
                    :extenso-init-fields ~(list 'quote (vec init-fields)) 
                    :extenso-fields ~(list 'quote (vec uninit-fields))
                    :extenso-methods ~(list 'quote protos-methods)
                    ))
          _ (-ppp (str "; " extenso-name " proto-info-form: ") proto-info-form)
          ]
      `(do
         ~proto-form
         ~proto-info-form
         )))
  ) ; defextenso

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn constructo-protocol-name [constructo-name]
  (symbol (str "-!" constructo-name "!-")))

#_ (* Given a symbol representing a constructo, returns a new symbol 
      that designates the constructo-protocol.
      @arg csym A symbol denoting the constructo.
      @returns A symbol denoting the corresponding constructo-protocol.
      This will have the same namespace (or lack thereof) as @(arg csym).)
(defn constructo-protocol-name [csym]
  (if-let [nsn (namespace csym)]
    (symbol (str nsn "/-!" (name csym) "!-"))
    (symbol (str "-!" csym "!-"))))

#_ (* Resolves a constructo name, fetches its var, and returns it.
      @arg constructo A symbol containing the name of the constructo. The
      name is assumed to either be in the current namespace or to be
      fully-qualified, i.e. there is no resolution performed on it.
      )
(defn constructo-var 
  ([constructo] (constructo-var constructo *ns*))
  ([constructo namespace] 
    (var-get (ns-resolve namespace (constructo-protocol-name constructo)))))

(defn constructo-get 
  ([constructo field] 
    (get (constructo-var constructo) field))
  ([constructo namespace field] 
    (if (symbol? namespace)
      (require namespace))
    (get (constructo-var constructo namespace) field)))

#_ (* Protocol describing the interface for constructor-specific parts of a
      @(link ConstructoInfo) object.)
(defprotocol Constructo-Info 
  #_ (* Returns the constructo's name as a symbol.)
  (constructo-name-of [this])
  #_(* Returns the constructo's namespace as a symbol.)
  (constructo-namespace-of [this])
  #_ (* Returns a constructo's constructor prototype.)
  (constructor-proto-of [this])
  )

#_ (* Compendium of information about a constructo retained for reflection
      purposes.
      @p For each constructo encountered, @(link defconstructo) stores
      a @name object in the constructo-specific protocol's map, 
      where it can be retrieved using @(link constructo-info).)
(defrecord ConstructoInfo
  [name namespace raw-extensos composed-extensos local-fields local-methods 
   initialized-fields uninitialized-fields constructor-proto]
  ExtensomaticInfo
  (raw-extensos-of [this] raw-extensos)
  (composed-extensos-of [this] composed-extensos)
  (local-fields-of [this] local-fields)
  (init-fields-of [this] initialized-fields )
  (uninit-fields-of [this] uninitialized-fields)
  (local-methods-of [this] local-methods)
  Constructo-Info
  (constructo-name-of [this] name)
  (constructo-namespace-of [this] namespace)
  (constructor-proto-of [this] constructor-proto)
  )

#_ (* Retrieves a constructo's @(link ConstructoInfo) object.
      @arg csym A symbol denoting the constructor's name
      @arg ns-sym Optional symbol of a namespace used to resolve @(arg csym)\;
      @(link *ns*) is used by default.
      @returns The constructo's @(link ConstructoInfo) object.
      )
(defn constructo-info 
  ([csym] (constructo-get csym *ns* :constructo-info))
  ([csym ns-sym] 
    #_(prn 'constructo-info csym ns-sym)
    (constructo-get csym ns-sym :constructo-info)))

#_ (* Defines a @(i constructo), a Clojure record that's composited from
      some number of @(link defextenso extenso) components, possibly with 
      additional locally defined fields and methods added as well.
      
      @(arg record-name The name of the constructo, which is equivalently the
      name of the Clojure record that implements the constructo.
      @p Note that, like all records, this gives rise to a native (e.g., Java)
      class name, and not a Clojure var.)
      @(arg &composed-extensos 
            A (possibly empty) vector of extenso specifications to be composed
            into the constructo. Each extenso spec is either an extenso name, or
            list of the form @(form (comp-extenso-name & comp-field-specs)), where
            @arg comp-extenso-name The name of the composed extenso.
            @(arg comp-field-specs 
                  A sequence of field specs. Each field spec is either
                  the name of an uninitialized field of the extenso, or a tuple of 
                  the form @(form (field-name init-value)), where
                  @arg field-name The name of the field
                  @arg init-value The value to which the field is to be initialized.)
            Note that if field specs are provided, @(i a field spec @(b must) be
            provided for  every uninitialized field of the composed extenso), or
            @name will generate an error. ) 
      @(arg &local-fields A (possibly empty) vector of fields to be added to any defined 
      (implicitly or explicitly) by @(arg &composed-extensos). Each field is either
      the name of a field, or a tuple of the form @(form (field-name init-value)),
      where\:
      @arg field-name The name of the field
      @arg init-value The value to which the field is to be initialized.)
      @arg protos-methods A (possibly empty) list of local method 
      implementations, followed by protocol-name / method implementation 
      sequences in the same format as @(link defrecord).
      )
(defmacro defconstructo [record-name &composed-extensos &local-fields & protos-methods]
  (if-not (symbol? record-name) 
    (error "defconstructo: Record name must be a symbol: " record-name))
  (if-not (vector? &composed-extensos)
    (error "defconstructo: &composed-extensos must be a vector: " &composed-extensos))
  (if-not (vector? &local-fields)
    (error "defconstructo: &local-fields must be a vector: " &local-fields))
  (let [[& composed-extensos] &composed-extensos
        [& local-fields] &local-fields
        composed-vars (check-extensos 'defconstructo composed-extensos)]
    (check-fields 'defconstructo local-fields)
  
    (let [local-methods 
          (take-while (fn [item] (not (symbol? item))) protos-methods)
          #_ (-ppp "defconstructo local-methods: " local-methods)
        
          constructo-proto-name (constructo-protocol-name record-name)
          #_ (-pln "defconstructo constructo-proto-name:" constructo-proto-name)

          local-proto-form 
          (if-not (empty? local-methods)
            `(defprotocol ~constructo-proto-name 
               ~@(map (fn [method-form] 
                        (let [[method-name [& params] & _] method-form]
                          (if (zero? (count params))
                            (error "defconstructo: Method " 
                                   method-name "must have at least one parameter"))
                          (list method-name (vec params))))
                      local-methods))
            `(defprotocol ~constructo-proto-name))
          _ (-ppp (str "; " record-name " local-proto-form: ") local-proto-form)
        
        
        [init-fields uninit-fields] (extenso-field-munger 'defconstructo composed-extensos local-fields)
          
        record-def 
        `(defrecord ~record-name 
           ~(vec (concat (map first init-fields) uninit-fields))
           ~@(if (empty? local-methods) [] [constructo-proto-name])
           ~@protos-methods
           ~@(mapcat (fn [extenso-form] 
                       (extenso-methods (if (symbol? extenso-form) 
                                          extenso-form 
                                          (first extenso-form)))) 
                     composed-extensos)
           )
        _ (-ppp (str "; " record-name " record-def:") record-def)
        
        new-def 
        (list 'defn (symbol (str *new-prefix* record-name)) 
              (str "Returns a new " record-name " object.")
              (vec (map (fn [x] (if (symbol? x) x (first x))) uninit-fields)) 
              (concat (list 'new record-name) (map second init-fields) uninit-fields))
        _ (-ppp (str "; " record-name " new-def:") new-def)
        
        proto-info-form
        `(def ~constructo-proto-name 
           (assoc ~constructo-proto-name 
                  :constructo-info
                  (ConstructoInfo.
                      ~(list 'quote record-name)
                      ~(list 'quote (ns-name *ns*))
                      ~(list 'quote composed-extensos)
                      ~(list 'quote composed-vars)
                      ~(list 'quote 
                             (reduce 
                               (fn [local-fields+ field-spec]
                                 (conj local-fields+
                                       (if (symbol? field-spec)
                                         field-spec (first field-spec))))
                               [] &local-fields))
                      ~(list 'quote (vec init-fields))
                      ~(list 'quote (vec uninit-fields))
                      ~(list 'quote protos-methods)
                      ~(list 'quote 
                             (let [[_ n _ v] new-def]
                               (list n v)))
                      )
                  :constructo-init-fields ~(list 'quote (vec init-fields)) 
                  :constructo-fields ~(list 'quote (vec uninit-fields))
                  ))
        _ (-ppp (str "; " constructo-proto-name " proto-info-form: ") proto-info-form)

        ]
    `(do
       ~local-proto-form
       ~proto-info-form
       ~record-def
       ~new-def))))


















