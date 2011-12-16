#_ ( Copyright (c) Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )

#_ (* Tool for creating a precis, or summarized version, of a CDJ-doc 
      element tree.
      @p This is intended partly as a debug tool --- a precis being a much more 
      tractable representation for visual inspection than a full element dump ---
      and partly as the basis for easily specifying test results.
      )
(ns cjd.precis
  (:use 
    [cjd.core-elements]
    [clojure.pprint]
    )
  (:import 
    [cjd.core_elements 
     Bold Italic Code SmallCaps Superscript Subscript Underline 
     Name NameSpace NameUse Link LinkTo Form Fun 
     Text Paragraph FlowContainer
     SeeAlso Since Deprecation Author
     Argument Field Option Return ]
    [cjd CJDException]
    )
  )

(def *c-to-p (ref {
		Bold  'b
		Italic 'i
		Code 'c
		SmallCaps 'sc
		Superscript 'sup
		Subscript 'sub
		Underline 'u
		Name 'name
		NameSpace 'ns
		NameUse 'nv
		Link 'link
		LinkTo 'linkto
		Form 'form
		Fun 'fn
		Text 'x
 
		Paragraph 'p
		FlowContainer 'fc
		SeeAlso 'see
		Since 'since
		Deprecation 'dep
		Author 'auth
		Argument 'arg
		Field 'field
		Option 'opt
		Return 'returns
  }))

(defn add-precis-class [class sym]
  (dosync (alter *c-to-p assoc class sym)))

#_ (* Creates a precis from a tree of flow-, element-, text-nodes. 
      @p The precis is just a recursive functional form, where the "functor" 
      is a symbol identifying the node type (e.g., ~"'p" for a paragraph node)
      followed by "arguments" that represent the node's children.
      @arg item A node, or sequence of nodes, from which to generate a precis.
      @arg with-text If true, includes text nodes' content\; otherwise, text
      nodes are merely represented, without text.
      @returns The precis form.
      )
(defn make-precis 
  ([item] (make-precis true item))
  ([with-text item]
    (cond
      (nil? item) nil
      
      (sequential? item) (vec (map #(make-precis with-text %) item))
      
      :else
      (do
        (if-not (satisfies? Node item)
          (throw (CJDException. (str "Unrecognized item in make-precis: " item))))
        (let [base (get @*c-to-p (class item))]
          (cond 
            
            (satisfies? HasName item)
            (concat [base (name-of item)] (make-precis with-text (content-of item)))
            
            (instance? Text item)
            (if with-text
              (list 'x (content-of item))
              (list 'x))
            
            :else
            (concat [base] (make-precis with-text (content-of item))))))
      )))

(defn pprint-precis [with-text item]
  (let [prec (make-precis with-text item)
        w (java.io.StringWriter.)]
    (pprint prec w)
    (.toString w)))
