#_ ( Copyright (c) Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Describes a set of objects that effectively represent nodes in an abstract syntax
      tree parsed from a CJD comment. Note that adding to this set will in general 
      also require collateral additions to both the comment parser and the HTML 
      generator before they're useful.
      @see @(link cjd.parser) for the comment parser.
      @see @(link cjd.generate) for HTML generation.
      )
(ns cjd.core-elements
  (:use
    [cjd.util.extensomatic]
    )
  (:import 
    [cjd CJDException]
    )
  )

#_ (* Base extenso for a node in the AST of a parsed CJD comment.
      )
(defextenso Node [] [content]
  (content-of [this] content)
  )

(defconstructo Paragraph [(Node content )] [])

(defconstructo Comment [(Node content)] [])


;-------------------------------------------------------------------------------
;    Flow forms
;-------------------------------------------------------------------------------

#_ (* Extenso incorporated by all flow elements.)
(defextenso Flow [Node] [])


(defconstructo FlowContainer [(Flow content)] [])

(defconstructo UnorderedList [Flow] [])
(defconstructo OrderedList [Flow] [])
(defconstructo ListItem [Flow] [])
(defconstructo Preformatted [Flow] [])


(defextenso Info [Flow] [])
(defconstructo SeeAlso [Info] [])
(defconstructo Since [Info] [])
(defconstructo Deprecation [Info] [])
(defconstructo Author [Info] [])

(defextenso Sectional [] [])

(defextenso HasName [] [name]
  (name-of [this] name))

(defextenso HasLevel [] [level]
  (level-of [this] level))

(defextenso ScopedName [(HasName name) (HasLevel level)] [])

(defconstructo Argument 
  [Sectional (ScopedName name level) (Flow content)] [])

(defconstructo Field 
  [Sectional (ScopedName name level) (Flow content )] [])

(defconstructo Option 
  [Sectional (ScopedName name level) (Flow content )] 
  [has-default default-value]
  (has-default? [this] has-default)
  (default-value-of [this] default-value)
  )

(defconstructo Return [Sectional (Flow content)] [])

;-------------------------------------------------------------------------------
;    Phrasing forms
;-------------------------------------------------------------------------------

#_ (* @name describes a class of nodes representing forms that contain other
      phrasing material, typically text effects like "bold" or "italic".
      )
(defextenso Phrase [Node] [])

(defconstructo Bold [Phrase] [])
(defconstructo Italic [Phrase] [])
(defconstructo Code [Phrase] [])
(defconstructo SmallCaps [Phrase] [])
(defconstructo Superscript [Phrase] [])
(defconstructo Subscript [Phrase] [])
(defconstructo Underline [Phrase] [])
(defconstructo Deleted [Phrase] [])

#_ (* @name describes a class of nodes representing forms that generate 
      phrase content, but
      effectively do their thing based on data derived from context or
      from parameters supplied in the form. 
      @p Note that the number of fields defined for a @name isn't 
      necessarily the same as the number of parameters in the instance
      of the form!
      )
(defextenso PhraseForm [(Node (content nil))] [])

(defconstructo Name [PhraseForm] [name])
(defconstructo NameSpace [PhraseForm] [namespace])
(defconstructo NameUse [PhraseForm] [name])
(defconstructo Link [PhraseForm] [target text condense])
(defconstructo LinkTo [PhraseForm] [target text])
(defconstructo Target [PhraseForm] [target])
(defconstructo Form [PhraseForm] [form])
(defconstructo Fun [PhraseForm] [form])

(defconstructo Text [(Node content )] [])



