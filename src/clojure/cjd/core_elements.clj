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
    [extensomatic.extensomatic]
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

#_ (* Node corresponding to a @(c ~"@p") element.)
(defconstructo FlowContainer [(Flow content)] [])

#_ (* Node corresponding to a @(c ~"@ul") element.)
(defconstructo UnorderedList [Flow] [])
#_ (* Node corresponding to a @(c ~"@ol") element.)
(defconstructo OrderedList [Flow] [])
#_ (* Node corresponding to a @(c ~"@li") element.)
(defconstructo ListItem [Flow] [])

#_ (* Extenso incorporated by all "info-description" node types.)
(defextenso Info [Flow] [])
#_ (* Node corresponding to a @(c ~"@see") element.)
(defconstructo SeeAlso [Info] [])
#_ (* Node corresponding to a @(c ~"@since") element.)
(defconstructo Since [Info] [])
#_ (* Node corresponding to a @(c ~"@deprecated") element.)
(defconstructo Deprecation [Info] [])
#_ (* Node corresponding to a @(c ~"@author") element.)
(defconstructo Author [Info] [])

#_ (* Marks an object as being part of some section.)
(defextenso Sectional [] [])

#_ (* Extenso incorporated by all nodes that "declare" identifiers to be documented.
      @field name A string containing the identifier.)
(defextenso HasName [] [name]
  #_ (* Returns the identifier.)
  (name-of [this] name))

#_ (* Extenso incorporated by nodes having a level.
      @field level The level number, an integer. May be nil!)
(defextenso HasLevel [] [level]
  (level-of [this] level))

#_ (* Extenso representing an identifier declared at a particular level.)
(defextenso ScopedName [(HasName name) (HasLevel level)] [])

#_ (* Node corresponding to a @(c ~"@arg") element.)
(defconstructo Argument 
  [Sectional (ScopedName name level) (Flow content)] [])

#_ (* Node corresponding to a @(c ~"@field") element.)
(defconstructo Field 
  [Sectional (ScopedName name level) (Flow content )] [])

#_ (* Node corresponding to a @(c ~"@opt"), @(c ~"@option"), @(c ~"@popt"), or
      @(c ~"@optn") element.
      @field has-default True if @(field default-value) is specified.
      @field default-value The default value, if there is one.
      @field parameter A string representing an parameter description 
      (as for a command-line argument.)
      @field identifier A identifier corresponding to the option itself, such as 
      the "destination" in ":dest destination".
      )
(defconstructo Option 
  [Sectional (ScopedName name level) (Flow content )] 
  [has-default default-value parameter identifier]
  (has-default? [this] (boolean has-default))
  (default-value-of [this] default-value)
  (has-param? [this] (boolean parameter))
  (parameter-of [this] parameter)
  (has-id? [this] (boolean identifier))
  (identifier-of [this] identifier)
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
(defconstructo Preformatted [PhraseForm] [preform])
(defconstructo Example [PhraseForm] [actions])
(defconstructo Image [PhraseForm] [source])

(defconstructo Text [(Node content )] [])



