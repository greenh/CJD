#_ ( Copyright (c) 2011 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* HTML generation package for CJD.
      @p The CJD HTML generation process starts with a forest of the rough 
      abstract syntax trees constructed in the first phase of CJD processing.
      Roots of the trees correspond to namespaces, and lower nodes represent
      artifacts such as functions, vars, protocols, records, and the like, and 
      then their components where applicable, such as methods. Each artifact
      or component contains its CJD documentation, if it existed in the source.
      @(p @(target gen-fun) The large bulk of the functions here are 
          @(i generator functions) of the form 
          @(fun (gen-something [node context])), where
          @(arg node 
            Depending on the generation routine, this is usually either 
            a @(link Node) derivative object from the AST parsed from the CJD comment,
            or an @(link Artifact) derivative, parsed from a Clojure source file. 
            Other variants are generally documented.)
          @arg context The current @(il cjd.context.Context) object. 
          @returns Unless stated otherwise, a string of HTML text.)
      @p Depending on the nature of the node, a generation function may
      make calls directly or indirectly (via a map) to other generation routines,
      possibly modifying the version of the context object it passes down. 
      @p It's worth noting that this is just one possible generator, and that there's 
      no bar to generating others as well.
      )
(ns cjd.generate
  (:use
    [cjd.util.string-utils]
    [extensomatic.extensomatic]
    [cjd.core-elements]
    [cjd.context]
    [cjd.parser]
    [cjd.artifact-base]
    [cjd.core-artifacts]
    [cjd.precis]
    [cjd.link-resolver]
    [cjd.resolver]
    [hiccup.core]
    [hiccup.page-helpers]
    )
  (:import 
    [cjd CJDException]
    [cjd.core_elements 
     Bold Italic Code SmallCaps Superscript Subscript Underline Deleted
     Name NameSpace NameUse Link LinkTo Form Fun Target
     Text Paragraph FlowContainer Preformatted Example Image
     SeeAlso Since Deprecation Author
     Argument Field Option Return 
     UnorderedList OrderedList ListItem
     ]
    [cjd.core_artifacts
     Namespace Function Macro KeyedFunction 
     Var OnceVar
     MultimethodDef MultimethodMethod 
     Protocol ProtocolMethod MethodImplementation 
     Poioo Record Type ]
    [org.apache.commons.lang StringEscapeUtils]
    )
   )

(defn- html-encode [string] (StringEscapeUtils/escapeHtml string))

(defn level-tag [tag level] (str tag level))

#_ (* A general-purpose @(linki java.util.Comparator) that does its comparisons
      on an uncased basis.
      )
(def uncased-comparator 
  (proxy [java.util.Comparator] [] 
    (compare [a b] (.compareToIgnoreCase (name a) (name b)))))

#_ (* Handly little thing that translates all "-" characters in a string
      into nonbreaking hyphens @(c ~"&#8209;").
      @arg sym Anything that renders a string (it gets run through @(link str).)
      @returns The re-hyphenated string.
      )
(defn nonbreak [sym] (.replaceAll (str sym) "-" "&#8209;"))

(declare gen-map) 

(defn gen-content [node context]
  #_(println "gen-content: " (class node))
  (let [node-fn (get gen-map (class node))]
    (if node-fn 
      (node-fn node context)
      (do
        (throw (CJDException. (str "No function for generating nodes of type " (class node) "!!")))
        (str " ?" (class node) "? ")))))

#_ (defn gen-seq [nodes context]
  (reduce 
    (fn [[new-context+ content+] node]
      (let [node-type (first node)
            node-fn (get gen-map node-type)]
        (if node-fn 
          (let [[new-context* node-content] (node-fn node new-context+)]
            [new-context* (str content+ node-content)])
          (do
            (warn (str "No function for generating nodes of type " node-type "!!"))))))
    ))

(defn gen-seq [nodes context]
  #_(println "gen-seq:" nodes)
  (reduce 
    (fn [content+ node]
      (let [node-fn (get gen-map (class node))]
        (str content+ (gen-content node context))))
    "" nodes))


(defn gen-paragraph [node context]
  (html [:p (gen-seq (content-of node) context)]))

#_ (* Generates an HTML string from a form that declares or uses variables,
      such as function prototypes, forms appearing in @(c ~"@(form ...)"),
      or names in @(c ~"@(arg arg-name)").
      @p @name is the party responsible for determing what "level" within the 
      comment text a name was "declared" at, and appropriately tagging the
      name to ensure correct colorization.
      
      @arg form The form for which generation is to be made.
      @arg context The operative context object.
      @returns The generated HTML string.
      )
(defn declaration-form [form context]
  (let [argmap (context-items context)]
    (cond
      (symbol? form) 
      (if-let [level (get argmap form)]
        (html [:span { :class (str "n" level)} (nonbreak (html-encode (name form)))])
        ;; engage in a crude bit of identifier-coloring, just for "fn" for now
        (if (= form 'fn)
          (html [:span { :class "fn" } (nonbreak (html-encode (name form)))])
          (nonbreak (html-encode (name form)))))
      
      (keyword? form) 
      (if-let [level (get argmap form)]
        (html [:span { :class (str "n" level)} (nonbreak (html-encode (str form)))])
        (nonbreak (html-encode (str form))))
      
      (vector? form)
      (str "[" (apply str (interpose " " (map (fn [x] (declaration-form x context)) form))) "]")
      
      (list? form)
      (str "(" (apply str (interpose " " (map (fn [x] (declaration-form x context)) form))) ")")
      
      (map? form)
      (str "{" 
         (apply str 
                (interpose " " 
                           (map (fn [[k v]] (str (declaration-form k context) " " (str v))) form))) 
         "}")
      :else
      (str form)
      )))


(def header-text {
		SeeAlso "See also"
		Since "Since"
		Deprecation "Deprecated"
		Author "Author"
		Argument "Arguments"
		Field "Fields"
		Option "Options"
		Return "Returns"
    })


(defn gen-text [node context] (content-of node))

(defn gen-bold [node context] (html [:b (gen-seq (content-of node) context) ]))
(defn gen-italic [node context] (html [:i (gen-seq (content-of node) context) ]))
(defn gen-code [node context] (html [:code (gen-seq (content-of node) context) ]))
(defn gen-smallcaps [node context] (html [:span { :style "font-variant: small-caps"} (gen-seq (content-of node) context) ]))
(defn gen-superscript [node context] (html [:sup (gen-seq (content-of node) context) ]))
(defn gen-subscript [node context] (html [:sub (gen-seq (content-of node) context) ]))
(defn gen-underline [node context] (html [:u (gen-seq (content-of node) context) ]))
(defn gen-del [node context] (html [:del (gen-seq (content-of node) context) ]))
(defn gen-name [node context] 
  (let [nm (context-name context)] (html [:span.n0 (if nm nm "!?NO NAME?!")])))
(defn gen-ns [node context] 
  (let [nms (context-ns context)] (html [:span.n0 (if nms nms "!?NO NAMESPACE?!")])))
(defn gen-use [node context]
  (let [name (.name node)
        level (context-item-level context name)]
    (html [:span {:class (str "n" (if level level "x"))} (str name)])))
(defn gen-form [node context]
  (html [:code (declaration-form (.form node) context)]))
(defn gen-fn [node context]
  (let [f (.form node)
        form (if (vector? f) (list 'fn f) f)]
    (html [:code (declaration-form form context)])))

#_ (* A @(linkto "#gen-fun" generator function) for @(c ~"@link") and
      @(c ~"@linki").
      )
(defn gen-link [node context]   
  (let [uri (link-resolve context (.target node) )
        text 
        (if (.condense node)
          ;; condense a name, from @il = @linki
          (let [[_ xtext] (re-matches #".*\.([^\.]+)" (name (.target node)))]
                 (if (empty? xtext) (.target node) xtext))
          ;; else, see if the node has text, and if so use it
          (if-let [node-text (.text node)]
            (do
              #_ (prn 'gen-link (type node-text) node-text)
              ;; this test is a crude hack to distinguish parsed structures
              ;; from simple forms.
              (if (vector? node-text)
                (gen-seq node-text context)
                node-text))
            ;; else, use the node's target
            (name (.target node))) )]
    (if uri
      (html [:a { :href uri } text])
      (do
        (warn context (.target node) "unable to resolve link")
        (html [:span.nolink text])))))

#_ (* A @(linkto "#gen-fun" generator function) for @(c ~"@linkto") .
      )
(defn gen-linkto [node context]
  (let [text 
        (if-let [ntext (.text node)]
          (if (vector? ntext)
                (gen-seq ntext context)
                ntext)
          (.target node))]
    (html [:a { :href (.target node) } text 
           #_(if-let [text (.text node)] text (.target node))])))

#_ (* A @(linkto "#gen-fun" generator function) for user-specified targets,
      e.g. @(c ~"@target").
      )
(defn gen-target [node context]
  (html [:span { :id (.target node)}]))

#_ (* Generates an HTML class name from a single-letter "code" and a numeric
      level.)
(defn- lstr [code level]
  (if (> level 1)
    code
    (str code "1")))


#_ (* Generates HTML content for a CJD comment flow.
      @p The "flow" notion is defined implicitly\: fundamentally, it's a sequence 
      of flow-element nodes that are commonly contained within 
      some scope (e.g., a comment as a whole, or within some parent flow.)
      
      @arg flow The flow to generate HTML for. Operationally, this is simply
      a sequence of flow-element nodes.
      @arg context The context object for the flow.
      @arg extract-blurb If true, the HTML string for the first paragraph 
      of the flow is generated and returned separately for inclusion by the 
      caller in some higher-level structure.
      @(returns A tuple of the form @(form [upcontext blurb remainder]), where
          @arg upcontext @(arg context) updated to include any named elements
          (e.g. arguments, options, fields) described by the flow.
          @(arg blurb If @(arg extract-blurb) is true, and the first element of the
                flow is a paragraph, then this is the HTML string of
                the @(i content) of initial paragraph. Note that 
                this string @(b will not) be enclosed in paragraph tags!
                @p Otherwise, this will be an empty value (nil or empty string).)
          @arg remainder The HTML string of any content of flow not included 
          in @(arg blurb).
          )
      )
(defn gen-flow [flow context extract-blurb]
  #_(println "gen-flow:\n" (pprint-precis false flow))
  (if-not (vector? flow)
    (throw (CJDException. "gen-flow: not a flow")))
  (let [level (context-level context)
        named-subs (filter #(satisfies? HasName %) flow)
        upcontext (reduce 
                    (fn [context+ named] (context-item! context+ (name-of named) level))
                    context named-subs)
        ncontext (context-level! upcontext (inc level))
    
        [desc-nodes post-desc] 
        (split-with #(or (= (type %) Paragraph) (= (type %) FlowContainer)) 
                    (filter #(not (satisfies? Info %)) flow))
        
        [blurb desc] 
        (if (and extract-blurb (not-empty desc-nodes))
          (let [[first-par & remaining] desc-nodes]
            ;; either first-par is a paragraph, in which case we side-step
            ;; normal formatting and just acquire its content...
            (if (= (class first-par) Paragraph)
              [(gen-seq (content-of first-par) ncontext) 
               (gen-seq remaining ncontext)]
              ;; ... or it has to be a flow-container, in which case we
              ;; side-step normal formatting and recursively process the
              ;; flow that's its content.
              (let [[_ blurb* remainder] (gen-flow first-par ncontext)]
                [blurb* (str remainder (gen-seq remaining ncontext))])))
          ; Otherwise, no blurb. 
          [nil (gen-seq desc-nodes ncontext)])
        
        sects ; a sequence of (sequence of sections of a single type)
        (let [[sects- cur-sect- cur-type-]
              (reduce 
                (fn [[sects+ cur-sect+ cur-type] sub]
                  (cond
                    (nil? cur-type)
                    [sects+ [sub] (class sub)]
                    
                    (= (class sub) cur-type)
                    [sects+ (conj cur-sect+ sub) cur-type]
                    
                    :else
                    [(conj sects+ cur-sect+) [sub] (class sub)]
                    )
                  )
                [[] nil nil] post-desc)]
          (if cur-sect- (conj sects- cur-sect-) sects-))
          
        sect 
        (reduce 
          (fn [s+ sect-seq]
            (if (satisfies? ScopedName (first sect-seq)) 
              #_(instance? Return (first sect-seq))
              
              ; For ScopedName objects (args, opts, keys, etc.) we generate a 
              ; header here, then process individual items for their content.
              (let [header (header-text (class (first sect-seq)))
                  v (gen-seq sect-seq ncontext)]
                (str s+ 
                   (html [:div {:class (lstr "s" level )}
                          (if (= 1 level)   ; <<---------- remove to restore inner headers!
                            [:p {:class (lstr "v" level)} 
                             [:span {:class (lstr "k" level)} header]])
                          v])))
              ; if it's something else (e.g., a Return node), it's responsible 
              ; for all its own formatting, including header generation. 
              (str s+ (gen-seq sect-seq ncontext))))
          "" sects)
        
        info (gen-seq (filter #(satisfies? Info %) flow) ncontext)]
   [upcontext blurb (str desc sect info)]))

(defn gen-flow-container [node context] 
  (let [[_ _ stuff] (gen-flow (content-of node) context false)]
    stuff))

(defn gen-monadic [node context]
  (let [level (dec (context-level context))
        [_ blurb remainder] (gen-flow (content-of node) context true)]
    (html [:p { :class (lstr "a" level)} 
            (declaration-form (name-of node) context) " &mdash; " blurb]
           remainder)))

(defn gen-opt [node context]
  (let [level (dec (context-level context))
        [_ blurb remainder] (gen-flow (content-of node) context true)]
    (html [:p { :class (lstr "a" level)} 
            (declaration-form (name-of node) context) 
            (if (has-param? node)
              (html [:span.expr " " (html-encode (str (parameter-of node)))]))
            (if (has-default? node) 
              (str " (default: " (html [:span.expr (pr-str (default-value-of node))]) ")"))
            " &mdash; " blurb] 
           remainder)))

#_(defn gen-return [node context]
  (let [level (dec (context-level context))
        [_ _ remainder] (gen-flow (content-of node) context false)]
    (html [:p { :class (lstr "a" level) } 
           remainder])))

#_ (* The @(lt "#generator-function" generator function) for ~ "@return" elements. )
(defn gen-return [node context]
  (let [level (dec (context-level context))]
    (if (= level 1)
      
      (let [[_ _ remainder] (gen-flow (content-of node) context false)]
        (html [:div
               [:p {:class (lstr "i" level)} 
                [:span {:class (lstr "k" level)} "Returns"]]
               remainder])
        #_(html [:p { :class (lstr "a" level) } 
               remainder]))
      (let [[_ blurb remainder] (gen-flow (content-of node) context true)]
        (html [:div { :class "s" }
               [:p { :class (lstr "a" level) }
                [:span { :class (lstr "k" level) } "Returns"] " &mdash; " blurb]
               remainder])))))


(defn gen-info [node context infotype]
  (let [level (dec (context-level context))
        [_ _ remainder] (gen-flow (content-of node) context false)]
    (html [:div
           [:p {:class (lstr "i" level)} 
            [:span {:class (lstr "k" level)} infotype]]
           remainder])
    #_(html [:div.info
           [:p.infolead [:span.infotype infotype] blurb] 
           remainder])
    ))

(defn gen-since [node context] (gen-info node context "Since "))
(defn gen-dep [node context] (gen-info node context "Deprecated! "))
(defn gen-auth [node context] (gen-info node context "Author "))
(defn gen-see [node context] (gen-info node context "See also "))

(defn gen-ul [node context]
  (let [[_ _ stuff] (gen-flow (content-of node) context false)]
    (html [:ul stuff])))
(defn gen-ol [node context]
  (let [[_ _ stuff] (gen-flow (content-of node) context false)]
    (html [:ol stuff])))
(defn gen-li [node context]
  (let [[_ _ stuff] (gen-flow (content-of node) context false)]
    (html [:li stuff])))
(defn gen-pre [node context]
  (html [:div.spacer [:pre.pre (.preform node)]]))
(defn gen-image [node context]
  (html [:img { :src (.source node)}]))
(defn gen-example [node context]
  (let [[goop _] 
        (reduce 
          (fn [[stuff u?] item]
            (if u? 
              [(str stuff (html [:pre.u [:span.prompt "user=&gt; "] 
                                 (.replaceAll item "\n" "\n       ")])) false]
              [(str stuff (html [:pre.r item])) true]))
          ["" true]
          (.actions node))]
    (html [:div.spacer goop] )))

#_ (* The main map of @(link Element) subclass to @(lt "#generator-function" generator function).
      )
(def gen-map {
   Text gen-text
   Bold  gen-bold
   Italic gen-italic
   Code gen-code
   SmallCaps gen-smallcaps
   Superscript gen-superscript
   Subscript gen-subscript
   Underline gen-underline
   Deleted gen-del
   Name gen-name
   NameSpace gen-ns
   NameUse gen-use
   Link gen-link
   LinkTo gen-linkto
   Target gen-target
   Form gen-form
   Fun gen-fn
   Preformatted gen-pre
   Image gen-image
   Example gen-example
   
   FlowContainer gen-flow-container
   Paragraph gen-paragraph
   SeeAlso gen-see
   Since gen-since
   Deprecation gen-dep
   Author gen-auth
   Argument gen-monadic
 	 Field gen-monadic
   Option gen-opt
   Return gen-return
   UnorderedList gen-ul 
   OrderedList gen-ol
   ListItem gen-li
  })

(defn- init-context [context artifact]
  (-> context
    (context-name! (artifact-name-of artifact))
    (context-level! 1)
    (context-ns! 
      (if (= (type artifact) Namespace)
        (artifact-name-of artifact)
        (if-let [nspc (namespace-of artifact)] 
          (artifact-name-of nspc) 
          nil)))
    (context-item! (artifact-name-of artifact) 0)
    (context-line! (defined-at artifact))
    (context-file! (defined-in artifact))
    ))

#_ (* Generates the description of an artifact, from comment material or 
      docstring or whatever's availble.
      @arg context The current @(l Context) object.
      @arg artifact The artifact to generate a description for.
      @arg want-blurb? True if the blurb should be generated independently
      from the remainder of the documentation.
      @(returns A tuple of the form @(form [upcontext blurb content]), where\:
          @arg upcontext The updated context object.
          @arg blurb If @(arg want-blurb?) is true, and the artifact has
          CJD documentation, then HTML string representing the blurb\; 
          otherwise, nil.
          @arg content The HTML string representing documentation content
          either following or including the blurb, as selected by
          @(arg want-blurb?). Can be nil if the artifact has no documentation.)
      )
(defn gen-artifact-desc [context artifact want-blurb?]
  (cond
    (has-doc? artifact)
    (let [flow (parse-comment context (doc-form-of artifact))]
      (gen-flow flow context want-blurb?))
    
    (and (satisfies? HasDocString artifact) (has-docstring? artifact))
    [context nil (html [:pre.doc (docstring-of artifact)])]
    
    :else
    [context nil nil]))

#_ (* Generation function for function-like artifacts, such as functions and macros.)
(defn gen-functional [artifact context]
  (let [[upcontext _ content] (gen-artifact-desc context artifact false)]
       #_[flow (parse-comment context (doc-form-of artifact))
        [upcontext _ content] (gen-flow flow context false)]
    (html 
      (map (fn [param-form] 
             (html [:p.decl
                    [:span.expr 
                     (declaration-form (list (context-name upcontext) param-form) upcontext)]]))
           (parameter-sets-of artifact))
      [:div.desc content])))

#_ (* Generation function for simple-minded artifacts, such as vars, where there's no 
      substructure to contend with. This is also useful
      as a generator to get minimal support in place for non-core artifacts.)
(defn gen-desc [artifact context]
  (let [[upcontext blurb content] (gen-artifact-desc context artifact false)] 
    (html [:div.desc content]))
  #_(cond
    (has-doc? artifact)
    (let [flow (parse-comment context (doc-form-of artifact))
        [upcontext _ content] (gen-flow flow context false)]
    (html [:div.desc content]))
    
    (and (satisfies? HasDocString artifact) (has-docstring? artifact))
    (html [:pre.doc (docstring-of artifact)])))

#_ (* Returns a link denoting an artifact or component, with the link text denoting
      the name with or without namespace prepended, based on context.)
(defn gen-contextual-link [artifact context]
  (let [[uri artifact-ns artifact-sym] (link-resolvex context (artifact-name-of artifact) )
        #_ (prn 'gen-contextual-link '-- uri artifact-ns artifact-sym (context-ns context))
        artifact-name (str (artifact-name-of artifact))]
    (cond
      (and uri (= artifact-ns (context-ns context)))
      (html [:a { :href uri } (nonbreak artifact-name)])
      
      (and uri) 
      (html [:a { :href uri } 
             (nonbreak (str artifact-ns (if artifact-sym (str "/" artifact-sym))))])
      
      :else
      (html [:span.nolink (nonbreak artifact-name)]))))

#_ (* Generation function for method prototypes within protocols.)
(defn gen-protocol-method [artifact context]
  (let [neocontext (context-level! (init-context context artifact) 2)
        [upcontext blurb content] (gen-artifact-desc neocontext artifact false)
;        flow (if (has-doc? artifact) (parse-comment neocontext (doc-form-of artifact)))
;        [upcontext blurb content] (if flow (gen-flow flow neocontext false) [neocontext])
        ]
    (html 
      [:div { :id (artifact-name-of artifact) }
       (map
        (fn [signature]
          (html 
            [:p.a1  
             [:span.expr 
              (declaration-form (list (artifact-name-of artifact) signature) upcontext)]]))
        (butlast (signatures-of artifact)))]
      [:p.a1 
       [:span.expr 
        (declaration-form (list (artifact-name-of artifact) 
                                (last (signatures-of artifact))) upcontext)]
       #_(if-not (empty? blurb) (html " &mdash; " blurb))]
      content)))

#_ (* Generation function for protocols.)
(defn gen-protocol [artifact context]   
  (let [[upcontext _ content] (gen-artifact-desc context artifact false)]
    #_ [flow (parse-comment context (doc-form-of artifact))
        [upcontext _ content] (gen-flow flow context false)]
    (html 
      [:div.desc content
       [:div.s1
        [:p.v1 [:span.k1 "Methods"]]
        (map 
          (fn [method]
            (gen-protocol-method method upcontext))
          (methods-of artifact))]]
      )))

#_ (* Generation function for method implementations within artifacts such as records.
      @p Note that we do something slightly squirreley here and tag method implementations
      with a record name "/" method name ID, so as to disambiguate it with respect to
      other implementations, and the protocol method.)
(defn gen-method-impl [method-impl context]
  (let [[upcontext blurb content] (gen-artifact-desc 
                                 (init-context context method-impl) method-impl true)]
       #_[neocontext (init-context context method-impl)
        flow (if (has-doc? method-impl) (parse-comment neocontext (doc-form-of method-impl)))
        [upcontext blurb content] (if flow (gen-flow flow neocontext true) [neocontext])]
    (html 
      [:p.a1 
       [:span.expr 
        #_(- Small dance here to generate appropriate links for methods that 
             are implemented and defined together, ala extensomatic things,
             as opposed to the normal case where the definition
             is in a protocol, and implementation in a record.) 
        { :id (if (identical? (implementer-of method-impl) (parent-of method-impl))
                (artifact-name-of method-impl)
                (str (artifact-name-of (implementer-of method-impl)) 
                     "/" (artifact-name-of method-impl)))}
        (declaration-form (list (artifact-name-of method-impl) 
                                (parameters-of method-impl)) upcontext)]
       (if-not (empty? blurb) (html " &mdash; " blurb))]
      (if content [:div.desc content]))))

;#_ (* Generation function for Protocol Or Interface Or Object artifacts found within
;      records.)
;(defn gen-poioo [poioo context]  
;  (html [:p.proto (gen-contextual-link poioo)]
;        (map #(gen-method-impl % context) 
;             (sort-by artifact-name-of uncased-comparator (method-implementations-of poioo)))))

#_ (* Generation function for records and types.)
(defn gen-record-type [artifact context]   
  (let [pre-upcontext 
        (reduce 
          (fn [pre-up+ poioo]
            (reduce 
              (fn [pre-up++ method-impl]
                (context-item! pre-up++ (artifact-name-of method-impl) 1))
              pre-up+ (method-implementations-of poioo)))
          (init-context context artifact) 
          (poioos-of artifact))
        [upcontext _ content] (gen-artifact-desc context artifact false)
;        flow (parse-comment pre-upcontext (doc-form-of artifact))
;        [upcontext _ content] (gen-flow flow pre-upcontext false)
        ]
    (html
      [:p.decl 
       [:span.expr (declaration-form (list (artifact-name-of artifact) 
                                           (fields-of artifact)) upcontext)]]
      [:div.desc content
       (if-not (empty? (poioos-of artifact))
        (html
          [:div.s1 
           [:p.v1 [:span.k1 "Implemented protocols and interfaces"]]
           [:p 
            (map 
              (fn [poioo] (str (gen-contextual-link poioo context) "&ensp; ")) 
              (poioos-of artifact))]]
          [:div.s1 
           [:p.v1 [:span.k1 "Methods"]]
           (map #(gen-method-impl % context) 
             (sort-by artifact-name-of uncased-comparator
                      (mapcat method-implementations-of (poioos-of artifact))))]))])))

#_ (* Generation function for multimethod declarations.)
(defn gen-multi [artifact context]   ;;; XXX --- needs completion
  (let [[upcontext blurb content] (gen-artifact-desc context artifact false)]
    #_ [flow (parse-comment context (doc-form-of artifact))
        [upcontext _ content] (gen-flow flow context false)]
    (html [:div.desc content]))
  )


(def artifact-map* (ref { }))

(defrecord AI [generate-fn category])

(defn artifact-record [artifact] (get @artifact-map* (type artifact)))
(defn gen-fn-of [artifact] 
  (if-let [ar (artifact-record artifact)] (.generate-fn ar)))
(defn category-of [artifact] 
  (if-let [ar (artifact-record artifact)] (.category ar)))

#_ (* Adds artifact type information to the generator's artifact table.
      @arg artifact-class The class of @(arg Artifact)-derivative object to
      be added.
      @(arg gen-fn A function for generating HTML when the generation process
            encounters an artifact of the type given by @(arg artifact-class). This 
            function is always called from @(link gen-artifact), which produces the
            generic artifact HTML support. @(arg gen-fn) is responsible for producing
            additional detail, and for motivating generation of any component
            substructure the artifact might have.
            @p @(arg gen-fn) has the form @(fun (fn [artifact context])), where
            @arg artifact The artifact to generate for.
            @arg context The context object in effect.
            @returns The generated HTML text.)
      @arg category A keyword that identifies the category that this class of 
      artifacts is to be lumped into for summarization purposes. See @(link mk-category)
      for details.
      )
(defn mk-art [artifact-class gen-fn category]
  (let [rec (AI. gen-fn category)]
    (dosync (alter artifact-map* assoc artifact-class rec))))

(mk-art Function gen-functional :fn-macro)
(mk-art KeyedFunction gen-functional :fn-macro)
(mk-art Macro gen-functional :fn-macro)
(mk-art Var gen-desc :var)
(mk-art OnceVar gen-desc :var)
(mk-art Namespace gen-desc :ns)
(mk-art Record gen-record-type :structure)
(mk-art Type gen-record-type :structure)
(mk-art Protocol gen-protocol :protocol)
(mk-art MultimethodDef gen-multi :multidef)
(mk-art MultimethodMethod gen-multi :multimethod)


(def category-headers* (ref { }))

#_ (* Defines a category of artifacts.
      @p Categories are principally used by @(l gen-namespace) for purposes of
      generating the "summary" tables that follow a the description of a namespace.
      Thus, it's possible to lump functions and macros in to one category of
      function-esque things for summarization purposes.
      @arg sym A symbol or keyword that internally identifies the category.
      @arg title A short string that's used as the title of the category.
      For example, a @(arg title) like "Function/Macro" results in a 
      @(sc ~"Function/Macro Summary") heading.
      )
(defn mk-category [sym title]
  (dosync (alter category-headers* assoc sym title)))

(defn category-header [category] (get @category-headers* category))

(mk-category :var "Variable")
(mk-category :fn-macro "Function/Macro")
(mk-category :structure "Objects")
(mk-category :protocol "Protocol")
(mk-category :multidef "Multimethod")
(mk-category :multimethod "Multimethod Implementation")

#_ (* Generates detailed documentation for an artifact.)
(defn gen-artifact [artifact context]
  (msg context :a "gen-artifact" (.toString artifact))
  (let [gen-fn (gen-fn-of artifact)]
    (if-not gen-fn 
      (throw (CJDException. (str "No generator for artifact type " (type artifact)))))
    (let [neocon (init-context context artifact)
          type-uri (if (= 'def (defined-by artifact)) 
                     def-uri
                     (link-resolve neocon (defined-by artifact) ))]
      (html 
          [:div.artifact { :id (artifact-name-of artifact) }
           [:table.topline
            [:tbody
             [:tr 
              [:td [:span.itemname (artifact-name-of artifact)]]
              [:td {:align "right"}
               #_ [:span.itemtype (descriptive-of artifact)]
               (if type-uri
                 [:a { :href type-uri } (defined-by artifact)]
                 (defined-by artifact))
               " in "
               [:a { :href (str "#top" )} (context-ns neocon)]]]]]
           (gen-fn artifact neocon)]))))

#_ (* Retrieves the "blurb", the first paragraph, of a CJD comment.
      @p By weak convention, the first paragraph of an artifact's description
      is a terse description of what the artifact does to justify its existence,
      and as such, is useful in places like summaries.
      @arg flow The flow from which to extract the blurb.
      @arg context The operative context object.
      @returns The blurb, generally a @(link Paragraph) object, if there is
      one, or nil if not.
      )
(defn get-blurb [flow context]
 (cond 
     (= (type (first flow)) Paragraph)
     (gen-seq (content-of (first flow)) context)
     
     (= (type (first flow)) FlowContainer)
     (get-blurb (content-of (first flow)) context)
     
     :else nil))

(def date-format (java.text.SimpleDateFormat. "dd MMMM yyyy HH:mm zzz"))

#_ (* Generates a "leader", a simple-minded header.
      @p @name is the default header generator, and can be called if desired 
      by other, non-default header generators.
      @arg context The current context object.
      @returns A HTML string.
      )
(defn gen-leader [context]
  (html 
    [:div.leader
     (if (context-index context) 
       [:table
        [:tbody
         [:tr 
          [:td { :width "100%"}]
          [:td {:align "right"}
           [:a { :href (context-index context)} [:span.headlink "Index"]]]]]])]))

#_ (* Generates a "trailer", a simple-minded footer.
      @p @name is the default footer generator, and can be called if desired 
      by other, non-default footer generators.
      @arg context The current context object.
      @returns A HTML string.
      )
(defn gen-trailer [context] 
  (html
    [:div.trailer
     [:span.ending "Generated by " 
      [:a {:href "https://github.com/greenh/CJD"}  "CJD"] 
      " " (context-version context)
      " under Clojure "
      (let [{:keys [major minor incremental qualifier]} *clojure-version*]
        (str major "." minor "." incremental " "))
       " on " (.format date-format (context-gen-time context))]]))

#_ (* Generates a summary for namespace.)
(defn gen-summary [ns-artifact context]
  ; (println "--- gen-summary")
  (let [categories
        (->> (artifacts-of ns-artifact) 
          (filter (context-filter context))
          (map (fn [art] [(category-of art) art]))
          (sort-by first)
          (partition-by first))]
    (html 
      [:table.summary
       [:tbody
        (map 
          (fn [category-items]
            (let [[[cat-type _]] category-items
                  category-header (category-header cat-type)
                  artifacts (sort-by artifact-name-of 
                                     uncased-comparator
                                     (map second category-items))]
              (html 
                [:tr [:td {:colspan 2} [:p.sum-hdr category-header " Summary"]]]
                (map
                  (fn [artifact]
                    (if (has-doc? artifact) 
                      (let [neocon (init-context context artifact)
                            flow (parse-comment neocon (doc-form-of artifact))
                            blurb (get-blurb flow neocon)]
                        (html 
                          [:tr.sum
                           [:td [:a {:href (str "#" (artifact-name-of artifact))}
                                 (nonbreak (artifact-name-of artifact))]]
                           [:td [:div.blurb blurb]]]))))
                  artifacts))))
        categories)]])
    ))

#_ (* Generates a full HTML document for a namespace.)
(defn gen-namespace [ns-artifact context]
  (msg context :n "gen-namespace" (.toString ns-artifact) #_ context )
  (let [ns-name (artifact-name-of ns-artifact)]
    (html5 
      [:head 
       [:meta {:charset "ISO-8859-1"}]
       [:title (if (context-title context) (str (context-title context) " &ndash; ")) ns-name ]
       (map 
         (fn [css] 
           (html [:link {:type "text/css" :rel "stylesheet" :href css}]))
         (context-css context))]
      [:body 
       [:div { :id "top"}]
       (if-let [header (context-header context)] 
         (header context)
         (gen-leader context))
       [:div.ns  (artifact-name-of ns-artifact)]
       (if (has-doc? ns-artifact)
         ; note that we generate the ns description with level = 1, so embedded
         ; form/fun documentation works right
         (gen-desc ns-artifact (context-level! (init-context context ns-artifact) 1)))
       [:div.desc (gen-summary ns-artifact context)]
       (reduce 
         (fn [goop+ artifact]
           (str goop+ (gen-artifact artifact context)))
         "" (sort-by artifact-name-of 
                     uncased-comparator
                     (filter (context-filter context) (artifacts-of ns-artifact))))
       (if-let [footer (context-footer context)] 
         (footer context)
         (gen-trailer context))])))


#_ (* A demented "artifact" to represent an overview comment, for compatibility
      with @(link init-context) and everything else.)
(defconstructo Overview [ArtifactBase HasCJDoc] [])

#_ (* Generates an "overview" page, the ultimate topmost supreme acme of the whole thing...
      or more specifically, the page that's "index.html".
      @arg ns-artifacts A collection of @(link Artifact) objects to be included in the
      overview.
      @arg context The operative context.
      @returns The HTML page.
      )
(defn gen-overview [ns-artifacts context]
  (let [overview-doc (context-overview context)
        { ofile :file oline :line } (meta overview-doc)
        overview-artifact (if overview-doc 
                            (make-Overview "Overview" nil ofile oline overview-doc))
        
        artifacts (mapcat artifacts-of ns-artifacts)
        subartifacts (mapcat named-subartifacts-of 
                             (filter has-named-subartifacts? artifacts))
        artifact-horde
        (->> (concat artifacts subartifacts)
          (filter (context-filter context))
          (sort-by artifact-name-of uncased-comparator))
        
        ]
    (html5
     [:meta {:charset "ISO-8859-1"}]
     [:title (if (context-title context) (str (context-title context) " &ndash; ")) "Index"]
     (map 
       (fn [css] 
         (html [:link {:type "text/css" :rel "stylesheet" :href css}]))
       (context-css context))
     [:body
      [:div#top 
       (if (context-title context)
         (html [:h1 (context-title context)]))
       [:div.desc
        (if overview-artifact
          (let [[upcontext _ content] 
                (gen-artifact-desc (init-context context overview-artifact) 
                                   overview-artifact false)]
             #_[neocontext (init-context context overview-artifact)
                flow (if (has-doc? overview-artifact) 
                       (parse-comment neocontext (doc-form-of overview-artifact)))
                [upcontext _ content] 
                (if flow (gen-flow flow neocontext false) [neocontext])]
            content))
        
        
        (html 
          [:table.summary
           [:tbody
            [:tr [:td {:colspan 2} [:p.sum-hdr "Namespace Summary"]]]
            (map
              (fn [ns-artifact]
                (if (has-doc? ns-artifact) 
                  (let [neocon (init-context context ns-artifact)
                        flow (parse-comment neocon (doc-form-of ns-artifact))
                        blurb (get-blurb flow neocon)]
                    (html 
                      [:tr.sum
                       [:td [:a {:href (str (artifact-name-of ns-artifact) ".html")}
                             (nonbreak (artifact-name-of ns-artifact))]]
                       [:td [:div.blurb blurb]]]))
                  (html 
                      [:tr.sum
                       [:td [:a {:href (str (artifact-name-of ns-artifact) ".html")}
                             (nonbreak (artifact-name-of ns-artifact))]]
                       [:td ]])))
              ns-artifacts)]])
        [:p.sum-hdr "Documented artifacts"]
        [:p 
         (map 
           (fn [artifact]
             (html [:a { :href (str (artifact-name-of (namespace-of artifact)) 
                                    ".html#" 
                                    (artifact-name-of artifact))}
                    (nonbreak (artifact-name-of artifact))] "&ensp; "))
           artifact-horde)
         ]]

       ]
      (if-let [footer (context-footer context)] 
         (footer context)
         (gen-trailer context))])
    ))



