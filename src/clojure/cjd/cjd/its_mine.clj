#_ ( Copyright (c) Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Ancillary functions specific to CJD's documentation.
      )
(ns cjd.cjd.its-mine
  (:use 
    [cjd generate context]
    [hiccup core]
    )
  )

#_ (* Simple-minded footer function for adding a simple-minded copyright statement.
      @arg context Current context object.
      @returns Footer HTML string.
      )
(defn gen-footer [context]
  (html
    (gen-trailer context)
    [:div { :style "margin-top: 1mm; margin-bottom: 4mm; text-align: center; width: 100%;"}
     [:span { :style "font-size: 8pt;"}
      "Copyright &copy; 2011 Howard Green. All rights reserved."]]))