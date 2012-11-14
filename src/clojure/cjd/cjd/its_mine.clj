#_ ( Copyright (c) 2011 - 2012 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Miscellaneous functions for customizing the output CJD produces of its own 
      documentation.
      )
(ns cjd.cjd.its-mine
  (:use 
    [cjd generate context custom link-resolver artifact-base]
    [hiccup core]
    )
  )

(def year-format (java.text.SimpleDateFormat. "yyyy"))

#_ (* Simple-minded footer function for adding a simple-minded copyright statement.
      @arg context Current context object.
      @returns Footer HTML string.
      )
(defn gen-footer [context]
  (let [gdate (context-gen-time context)
        gyear ()] 
    (html
    (gen-trailer context)
    [:div { :style "margin-top: 1mm; margin-bottom: 4mm; text-align: center; width: 100%;"}
     [:span { :style "font-size: 8pt;"}
      "Copyright &copy; "
      (.format year-format (context-gen-time context))
      " Howard Green. All rights reserved."]])))

(add-source-resolver 
  (fn [artifact]
    (let [path (defined-in artifact)
          line (defined-at artifact)
          #_ (pr 'path path 'line line)] 
      (if (and path line 
               (or (re-matches #"src/clojure/cjd.*" path) 
                   (re-matches #"src/clojure/leiningen\.cjd.*" path))) 
        (str "https://github.com/greenh/CJD/blob/master/" path "#L" line)))))