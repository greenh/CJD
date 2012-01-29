#_ ( Copyright (c) Howard Green 2011. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )

#_ (* Parser for CJD comments, wherein a CJD-comment in the form of a sequence
      of Clojure forms is parsed into an abstract syntax tree. The resulting
      AST then serves as the basis for HTML generation.
      )
(ns cjd.parser
  (:use
    [cjd.context]
    [cjd.core-elements]
    [cjd.util.string-utils]
    [extensomatic.extensomatic]
    [hiccup.core]
    )
  (:import 
    [cjd CJDException]
    [org.apache.commons.lang StringEscapeUtils]
    )
  )

(defn- html-encode [string] (StringEscapeUtils/escapeHtml string))

(def nobreak-keyword :nb)
(def break-keyword :br)
(def space-char \ )
(def quote-char \")

(def special-char-keywords 
  #{  })

(defn- last-char-of [string] 
  (if (empty? string) nil (.charAt string (dec (.length string)))))
(defn- cut-last [string] (.substring string 0 (dec (.length string))))
(defn- cut-last-space [string] 
  (if (= space-char (last-char-of string))
    (cut-last string)
    string))

(defn- non-breaking-symbol? [sym] 
  (get #{ } (name sym)))

(defn- right-breaking-symbol? [sym]
  (get #{ ","  "." "..." "?" "!" } (name sym)))

#_(* Works through a list of comment text, turning it into a string.
     @p Processing continues for as long as the input is purely text,
     defined as being devoid of any directives. As soon as @name encounters 
     something non-textual, it quits and returns its text and whatever's left
     over for continued processing. 
     
     @arg form-contents The content of the source comment form to be processed.
     @arg context The current context map.
     @arg initial If true, then this is the initial call to @name within a phrasing
     run. The specific implication of this is that if this is a @(i not) the initial
     call, then the text run should assume that a space is needed for separation
     between the prior phrasing element and subsequent text 
     (which is subject to normal rules for elimination), vs. the initial case,
     where no space is desirable.
     @(returns 
        A tuple of the for @(form [text unprocessed]), where\: 
        @arg text The string containing processed text. This is HTML-ized
        before being returned to the extent of doing the encoding of 
        problematic characters like <, >, and &, so that the returned
        string is ready-for-use HTML text.
        @arg unprocessed The unprocessed tail of @(arg form-contents).))
(defn text-run [context form-contents initial]
   (loop [text (if initial "" " ")
          [item & remains :as remaining] form-contents] 
     #_(println "Item:" item " type:" (type item)  " remains:" remains)
     (cond  
       (nil? item) 
       [(html-encode (cut-last-space text)) nil] ;;;;;;;;;;;;;;;;;;;;;;;; return

       (symbol? item) 
       (cond
         (non-breaking-symbol? item)
         (recur (str (cut-last-space text) (name item)) remains)
         
         (right-breaking-symbol? item) 
         (recur (str (cut-last-space text) (name item) " ") remains)
         
         :else
         (recur (str text (name item) " ") remains))
       
       (keyword? item)
       (cond
         (= item nobreak-keyword) 
         (recur (cut-last-space text) remains)
         
         (= item break-keyword)
         (recur (str text " ") remains)
         #_[(html-encode text) remaining] ;;;;;;;;;;;;;;;;;;;;;;;; return
         
         :else 
         (recur (str text item " ") remains))
       
       (number? item) 
       (recur (str text item " ") remains)
       
       (string? item) 
       (recur (str text quote-char item quote-char space-char) remains)
       
       (char? item) 
       (recur (str (cut-last-space text) item space-char) remains) 
       
       (= (class item) clojure.lang.Cons) 
       (let [[functor target] item]
         (condp = functor
           'quote
           (recur (str (cut-last-space text) \' target space-char) remains)
           
           'clojure.core/deref  ; @<somthing... >
           [(html-encode text) remaining]  ;;;;;;;;;;;;;;;;;;;;;;;; return
           
           'clojure.core/unquote
           (if (string? target )
             (recur (str text target " ") remains)
             (do
               (warn context item "Unquote (\"~\") must occur before a string")
               (recur text remains)))
           
           (do
             (warn context item "Unrecognized cons-functor")
             (recur text remains))
           ))
       
       ;; in the following cases, deal with embedded collections by
       ;; emitting an open bracketing-symbol and sticking the contents 
       ;; of the collection onto the to-be-processed list, followed by
       ;; the closing bracketing symbol. Doesn't make much sense for  
       ;; clojure maps or sets, so we don't encourage their use.
       (list? item)
       (recur (str text "(") (concat item [\)] remains))
       
       (vector? item)
       (recur (str text "[") (concat item [\]] remains))
       
       (map? item)
       (recur (str text "{") (concat item [\}] remains))
       
       (set? item)
       (recur (str text "#{") (concat item [\}] remains))
       
       :else
       (do
         (warn context item "Unrecognized element " )
         (recur text remains))))
   )
    

#_ (* Describes elements for the comment parsing process.
      @p There are several classes of elements\; all share the same
      structure, but are segregated into different maps corresponding
      to different syntactic entities. Not pretty, but it does allow
      tags like "arg" to be used for both phrasing and flow forms.
      @field tags A collection of tag strings used to identify the element.
      @field desc A Very Short description string (why, we're not sure yet).
      @field arity The number of argument forms to acquire.
      @field phrase? True if the element accepts a phrase as a final argument.
      This is only meaningful for phrase-form elements (see @(l mk-phx)).
      @field new-fn A function that creates an object to represent the
      parsed element.
      )
(defconstructo Element [] [tags desc arity new-fn]
  (arity-of [this] arity)
  (new-of [this] new-fn))

(def phrase-map* (ref { }))

#_ (* Makes a phrase element and adds it to the phrase-element map used by the
      CJD comment parser.
      @arg tags A list of tag symbols. These are the symbols that are used with
      "@" to specify the phrase element, e.g. @(c ~"'b") for the @(c ~ "@(b ...)")
      phrase element for bold text.
      @arg desc A string containing a very short description of the phrase element.
      @(arg new-fn A function that creates an object to represent the phrase 
            element and its contents in the parse tree. It has the form 
            @(form (fn [context content])), where
            @arg context The context map currently in force.
            @arg content The subtree of phrasing content to be contained within the
            phrasing element.
            @returns An object representing the phrase element and its content.)
      @returns nil.
      )
(defn mk-ph [tags desc new-fn]
  (let [el (Element. tags desc 0 new-fn)]
    (dosync 
      (doseq [tag tags] (alter phrase-map* assoc tag el)))))

(mk-ph ['b] "Bold" (fn [context content] (make-Bold content) ))
(mk-ph ['i] "Italic" (fn [context content] (make-Italic content) ))
(mk-ph ['c] "Code" (fn [context content] (make-Code content) ))
(mk-ph ['u] "Underline" (fn [context content] (make-Underline content) ))
(mk-ph ['del] "Deleted" (fn [context content] (make-Deleted content) ))
(mk-ph ['sc] "Small caps" (fn [context content] (make-SmallCaps content) ))
(mk-ph ['sup] "Superscript" (fn [context content] (make-Superscript content) ))
(mk-ph ['sub] "Subscript" (fn [context content] (make-Subscript content) ))

(defn phrase-element? [sym] (get @phrase-map* sym))
(defn phrase-element [sym] (get @phrase-map* sym))


(def phrase-form-map* (ref { }))
(defn phrase-form-element? [sym arity] (if (symbol? sym) (get @phrase-form-map* [sym arity])))
(defn phrase-form-element [sym arity] (get @phrase-form-map* [sym arity]))

#_ (* Makes a phrase-form and adds it to the phrase-form map used by the CJD
      comment parser.
      @arg tags A list of tag symbols. These are the symbols that are used with
      "@" to specify the phrase form.
      @arg desc A string containing a very short description of the phrase form.
      @arg arity The number of arguments that should follow the tag. If a tag 
      supports multiple arities, use a separate invocation of @name for each
      arity.
      in effect specifying that that the input form has @(i at least) the number of 
      arguments given by @(arg arity). 
      @(arg new-fn A function that creates an object representing the phrase-form
            and serving as its node in the comment's AST. This function has the form
            @(fun (fn [context other-params...])), where
            @arg context The @(linki cjd.context.Context) object then in force.
            @arg other-params... Other, additional parameters as necessary. The number
            of these parameters must match the value specified for @(arg arity).
            @returns An object representing the phrasing form and its content.
            )
      @returns nil.
      )
(defn mk-phx 
  ([tags desc arity new-fn]
    (let [el (Element. tags desc arity new-fn)]
      (dosync 
        (doseq [tag tags] (alter phrase-form-map* assoc [tag arity] el))))))

(mk-phx ['ns] "Namespace" 0 (fn [context] (make-NameSpace (context-ns context)) ))
(mk-phx ['name] "Name" 0 (fn [context] (make-Name (context-name context)) ))
(mk-phx ['arg 'field 'option 'opt 'key] "Name use" 1 
        (fn [context name] (make-NameUse name ) ))

(mk-phx ['pre] "Preformatted" 1 
        (fn [context content] 
          (make-Preformatted (html-encode (str content)))))
(mk-phx ['form] "Form" 1 (fn [context content] (make-Form content) ))
(mk-phx ['fun] "Function" 1 (fn [context content] (make-Fun content) ))
(mk-phx ['linki 'il] "Link/Import" 1 (fn [context target] (make-Link target nil true) ))
(mk-phx ['target] "Target" 1 (fn [context target] (make-Target target)))
(mk-phx ['image 'im] "Image" 1 (fn [context target] (make-Image target)))

;(mk-phx ['link 'l] "Link" 1 (fn [context target] (make-Link target nil false) ))
;(mk-phx ['link 'l] "Extended link" 2 true (fn [context target text] (make-Link target text false) ))
;(mk-phx ['linkto 'lt] "Link to" 1 (fn [context target] (make-LinkTo target nil) ))
;(mk-phx ['linkto 'lt] "Extended link to" 2 true (fn [context target text] (make-LinkTo target text) ))

(def phrase-form-parsed-map* (ref { }))
(defn phrase-form-parsed-element? [sym] (if (symbol? sym) (get @phrase-form-parsed-map* sym)))
(defn phrase-form-parsed-element [sym] (get @phrase-form-parsed-map* sym))

#_ (* Makes a phrase-form with a custom parse function and adds it to the 
      phrase-form-parsed map used by the CJD comment parser.
      @arg tags A list of tag symbols. These are the symbols that are used with
      "@" to specify the phrase form.
      @arg desc A string containing a very short description of the phrase form.

      @(arg parse-fn A function that parses the supplied content, and if it's 
            acceptable, and creates and returns an object representing the phrase-form
            and serving as its node in the comment's AST. This function has the form
            @(fun (fn [context post-tag])), where
            @arg context The @(linki cjd.context.Context) object then in force.
            @arg post-tag A list containing the content of the element that follows 
            the tag.
            @returns An object representing the phrasing form and its content.
            )
      @returns nil.
      )
(defn mk-phf
  ([tags desc parse-fn]
    (let [el (Element. tags desc 0 parse-fn)]
      (dosync 
        (doseq [tag tags] (alter phrase-form-parsed-map* assoc tag el))))))

(declare phrasing-run)

(mk-phf ['example 'ex] "Example" 
        (fn [context post-tag] 
          (make-Example (map (fn [item] (html-encode (str item))) post-tag))))
(mk-phf ['link 'l] "Link" 
        (fn [context post-tag] 
          (let [[target & post-target] post-tag
                [subphrase post-phrase]  (if post-target (phrasing-run context post-target))]
            (if (not-empty post-phrase)
              (warn context post-phrase "Ignoring unrecognized content"))
            (if target
              (make-Link target subphrase false)
              (warn context post-tag "Missing link target")))))
(mk-phf ['linkto 'lt] "Link to" 
        (fn [context post-tag] 
          (let [[target & post-target] post-tag
                [subphrase post-phrase]  (if post-target (phrasing-run context post-target))]
            (if (not-empty post-phrase)
              (warn context post-phrase "Ignoring unrecognized content"))
            (if target
              (make-LinkTo target subphrase)
              (warn context post-tag "Missing linkto target")))))


#_ (* Parses the phrasing content within a flow into a tree of phrasing elements 
      and forms, and text nodes.
      @p Syntactically, this consists of chunks of "text" interspersed with 
      tagged forms, either open à la "@name" or closed, like "@(b bold-text)"
      @p @name essentially operates by determining what kind of item is next 
      in its list of unprocessed content, and then invoking an appropriate
      processing function to operate on it. Each processing function 
      does its thing for whatever scope is appropriate (possibly recursively
      invoking @name in the process), and returns whatever html it has generated,
      along with any unprocessed comment text. @name adds the generated HTML to
      its string, and then continues processing the remaining input.
      
      @arg form-contents The content of the source comment form to be processed.
      
      @arg context The current context map.
      
      @returns A tree of @(link Phrase), @(link PhraseForm), 
      and @(linki cjd.core_elements.Text) objects
      that collectively describe the phrasing content.
      )
(defn phrasing-run [context form-contents]
  (let 
    [[phrases remaining] 
     (loop [phrases+ []
            [item & remains :as unprocessed] form-contents
            initial-text true]
       ;(prn (get context :ind ) 'ph phrases+)
       ;(prn (get context :ind ) 'un unprocessed)
       
       ;; Start with a little speculative pre-parsing, and then analyze the
       ;; results. 'target' is whatever comes after an '@', and 'tag'
       ;; and 'post-tag' are set if 'target' is a list of stuff (and not, e.g.,
       ;; a symbol)
       (let [[cons-funct t0] (if (= (class item) clojure.lang.Cons) item nil)
             target (if (= cons-funct 'clojure.core/deref) t0 nil)
             [tag & post-tag] (if (seq? target) target nil)]
         (cond 
           (nil? item) [phrases+ nil]  ;; <<<--------------- exit
           
           ;; A phrase element as an open form. In this case, phrase element's
           ;; content is the single following form.
           (phrase-element? target)
           (let [el (phrase-element target)
                 [phrase-content & post-phrase] remains
                 [subphrase leftovers] (phrasing-run context (list phrase-content))]
             (if (not-empty leftovers)
               (warn context leftovers "Ignoring unrecognized content"))
             (recur (conj phrases+ ((new-of el) context subphrase)) 
                    post-phrase false))
           
           ;; A phrase element, e.g. "@(i phrase ... text)", i.e. 
           ;; a closed-form element that has a phrase as its content.
           (phrase-element? tag)
           (let [el (phrase-element tag)
                 [subphrases leftovers] (phrasing-run context post-tag)]
             (if (not-empty leftovers)
               (warn context leftovers "Ignoring unrecognized content"))
             (recur (conj phrases+ ((new-of el) context subphrases)) remains false))

           ;; An open-form niladic form element, e.g. @name.
           (phrase-form-element? target 0)
           (let [el (phrase-form-element target 0)]
             (recur (conj phrases+ ((new-of el) context)) remains false)) 

           ;; closed-form form element, e.g. @(name) or @(arg var). Note that we do
           ;; a match on arity as well as the tag name.
           (phrase-form-element? tag (count post-tag))
           (let [el (phrase-form-element tag (count post-tag))]
             (recur (conj phrases+ (apply (new-of el) context post-tag)) remains false))
           
           (phrase-form-parsed-element? tag)
           (let [el (phrase-form-parsed-element tag)
                 obj ((new-of el) context post-tag)]
             (if obj
               (recur (conj phrases+ obj) remains false)
               (recur phrases+ remains false)))
           
           ;; if 'tag' or 'target' is a symbol and not otherwise accounted for, 
           ;; assume that it's a closed- or open-form flow element, and we're done here.
           #_ (or (symbol? target)(symbol? tag))
           #_ [phrases+ unprocessed] ;; <<<--------------- exit
           
           ;; if 'tag' or 'target' is non-nil, then we've seen something that looks
           ;; like a tag, albeit one we don't regcognize here. Assume that it's a
           ;; flow element, and kick it back to the flow parser for handling.
           (or tag target)
           [phrases+ unprocessed] ;; <<<--------------- exit
           
           :else   ;; treat anything else as "text"
           (let [[text leftovers] (text-run context unprocessed initial-text)] 
             (recur (conj phrases+ (make-Text text)) leftovers false))
           )))]
    [(not-empty phrases) remaining])
  ) ;;; phrasing-run

; (declare parse-comment)

(def flow-map* (ref { }))

#_ (* Defines a flow element, and adds it to the collection of flow elements 
      recognized by the parser.
      @arg tags A vector of symbols that introduce the element, e.g. @(form ['arg]) 
      for arguments. These symbols are all treated as synonyms.
      @arg desc A string containing a @(i very) short description of the element.
      @arg arity The number of forms following the tag that are to be treated as
      arguments. May be zero or any positive integer. Any forms following the arguments are treated as content. @(c ~"@arg"),
      for example, takes the name of the argument as a parameter, and thus has an arity 
      of 1.
      @(arg new-fn 
            A function that the parser invokes to generate an object representing
            the element. The function has a form @(fun [context arg... content]), where
            @arg context The current context object
            @arg arg... Arguments acquired by the parser. The number of these arguments
            is determined by @(arg arity).
            @arg content The content of the flow element.
            @returns An object, nominally a derivative of @(link Flow) extenso, that 
            represents the flow element.)
      )
(defn mk-el [tags desc arity new-fn]
  (let [el (Element. tags desc arity new-fn)]
    (dosync 
      (doseq [tag tags] (alter flow-map* assoc tag el)))))

(mk-el ['return 'returns 'rv] "Returned value" 0 
       (fn [context content] (make-Return content) ))
(mk-el ['arg] "Argument" 1 
       (fn [context name content] (make-Argument name (context-level context) content) ))
(mk-el ['field] "Field" 1 
       (fn [context name content] (make-Field name (context-level context) content) ))
(mk-el ['key 'opt] "Option" 1 
       (fn [context name content] 
         (make-Option name (context-level context) content false nil nil)))
(mk-el ['option] "Option with default" 2 
       (fn [context name default content] 
         (make-Option name (context-level context) content true default nil)))
(mk-el ['popt] "Option with parameter" 2 
       (fn [context name param content] 
         (make-Option name (context-level context) content false nil param)))

(mk-el ['see] "See also" 0 
       (fn [context content] (make-SeeAlso content) ))
(mk-el ['author] "Author" 0 
       (fn [context content] (make-Author content) ))
(mk-el ['deprecated] "Deprecated" 0 
       (fn [context content] (make-Deprecation content) ))
(mk-el ['since] "Since" 0 
       (fn [context content] (make-Since content) ))

(mk-el ['p] "Paragraph" 0 (fn [context content] (make-FlowContainer content)))

(mk-el ['ul] "Unordered list" 0 (fn [context content] (make-UnorderedList content)))
(mk-el ['ol] "Ordered list" 0 (fn [context content] (make-OrderedList content)))
(mk-el ['li] "List item" 0 (fn [context content] (make-ListItem content)))


(defn flow-element [tag] (get @flow-map* tag))

(defn niladic-flow? [tag] (if-let [elt (flow-element tag)] (= (arity-of elt) 0) false))
(defn monadic-flow? [tag] (if-let [elt (flow-element tag)] (= (arity-of elt) 1) false))
(defn dyadic-flow? [tag] (if-let [elt (flow-element tag)] (= (arity-of elt) 2) false))


#_ (* Processes a sequence of CJD comment-content forms into a sequence 
      of flow-forms, which will in turn contain phrasing and text content.
      @arg context The context map.
      @arg contents A sequence of input forms to be parsed.
      @(returns 
         A tuple of the form @(form [flows remaining]), where 
         @arg flows A sequence of flow nodes describing the parsed content
         @arg remaining Any unparsed forms in the input sequence.
         )
      )
(defn flow-run [context contents]
  #_ (println "fr: " context contents)
  (let [[flows remaining]
        (loop [flows+ []
               [item & post-target :as unprocessed] contents]
          (let [[cons-funct t0] (if (or (= (class item) clojure.lang.Cons)
                                        (list? item)) item nil)
                #_ (println "xx: " item (class item) cons-funct t0)
                target (if (= cons-funct 'clojure.core/deref) t0 nil)
                [tag & post-tag] (if (seq? target) target nil)]
            ;(prn "-- i: " item target)
            ;((prn "-- r: " post-target)
            (cond 
              (nil? item) [flows+ nil]
              
              ;; @p ...  
              ;; Note that we special-case @p here: no point in putting
              ;; a paragraph inside a paragraph, as for nominal the niladic case.
              (= target 'p) 
              (let [[para post-content] (phrasing-run context post-target)]
                (recur (conj flows+ (make-Paragraph para)) 
                       post-content))
              
              ;; Niladic open-form, e.g. @returns ...
              (niladic-flow? target)
              (let [el (flow-element target)
                    [para post-content] (phrasing-run context post-target)]
                (recur (conj flows+ ((new-of el) context [(make-Paragraph para)])) 
                       post-content))
              
              ;; Niladic closed form, e.g. @(p ...). We do a tiny optimization
              ;; here and not wrap paragraphs with additional paragraphs if we 
              ;; can avoid it.
              (niladic-flow? tag)
              (let [el (flow-element tag)
                    [flow post-content] (flow-run context post-tag)]
                  (if (not-empty post-content)
                    (warn context post-content "Unrecognized content!"))
                  (if (and (= tag 'p)  
                           (every? #(instance? cjd.core_elements.Paragraph %) flow))
                    (recur (vec (concat flows+ flow))
                           post-target)
                    (recur (conj flows+ ((new-of el) context flow)) 
                           post-target)))
              
              ;; @arg var ...
              (monadic-flow? target)
              (let [el (flow-element target)
                    [arg & post-arg] post-target]
                (let [[para post-content] (phrasing-run context post-arg)]
                  (recur (conj flows+ ((new-of el) context arg [(make-Paragraph para)])) 
                         post-content)))
              
              ;; @(arg var-form ...)
              (and (monadic-flow? tag) (not= (count post-tag) 1))
              (let [el (flow-element tag)
                    [arg1 & post-arg] post-tag]
                (let [[flow post-content] (flow-run context post-arg)]
                  (if (not-empty post-content)
                    (warn context post-content "Unrecognized content!"))
                  (recur (conj flows+ ((new-of el) context arg1 flow)) 
                         post-target)))
              
              ;; @option opt default ...
              (dyadic-flow? target)
              (let [el (flow-element target)
                    [arg1 arg2 & post-arg] post-target]
                (let [[para post-content] (phrasing-run context post-arg)]
                  (recur (conj flows+ ((new-of el) context arg1 arg2 [(make-Paragraph para)]) )
                         post-content)))
              
              ;; @(option opt default ...)
              (and (dyadic-flow? tag) (not= (count post-tag) 1))
              (let [el (flow-element tag)
                    [arg1 arg2 & post-arg] post-tag]
                (let [[flow post-content] (flow-run context post-arg)]
                  (if (not-empty post-content)
                    (warn context post-content "Unrecognized content!"))
                  (recur (conj flows+ ((new-of el) context arg1 arg2 flow)) 
                         post-target)))
              
              #_(- Whatever it is, we don't recognize it here. Most likely,
                   it's the start of a "default" paragraph, e.g. a paragraph
                   unheralded by an @p. But, it could be something that's also
                   unrecognized by phrase-run, in which case it'll return a 
                   nil result... whence we complain. And avoid infinite loops.)
              :else
              (let [[phrases post-content] (phrasing-run context unprocessed)]
                (if (empty? phrases)
                  (do
                    (warn context unprocessed "Unrecognized construct")
                    (recur flows+ nil))
                  (recur (conj flows+ (make-Paragraph phrases)) 
                         post-content))))))]
    #_(println ":fr exit" remaining)
    [flows remaining]))

#_ (* Parser for CJD comment content.
      @arg context The context map.
      @arg contents The CJD comment, notionally as delivered from the 
      CJD reader-hack. It must start with the appropriate symbol, @(c ~"cjd!doc"),
      or it will be rejected.
      @returns A sequence of flow objects representing the AST of the comment.
     )
(defn parse-comment [context contents]
  (let [[docoid & good-stuff] contents]
    (if-not (= docoid 'cjd!doc)
      (throw (CJDException. (str "Not a doc-comment!!"))))
    (let [[flow-nodes remaining] (flow-run context good-stuff)]
      (if-not (empty? remaining)
        (warn context remaining "Unrecognized material in comment" ))
      flow-nodes))
  )





