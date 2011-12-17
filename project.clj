#_ ( Copyright (c) Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* CJD is a technology for specifying documentation within Clojure program files, 
      and subsequently extracting that documentation and processing it into such
      forms as an HTML document tree. 
      @p CJD is an open-source project. Source materials and documentation are
      located at its @(linkto "https://github.com/greenh/CJD" home).
      )
(defproject cjd "0.0.1-SNAPSHOT"
  :description "CJD documentation processor"
  :dependencies [
     [org.clojure/clojure "1.2.1"]
     [hiccup/hiccup "0.3.7"]
     [commons-lang "2.6"]
     ]
  :source-path "src/clojure"
  :resources-path "src/resources"
  :java-source-path "src/java"
  
  :library-path "lib"
  :compile-path "classes"
  :target-dir "bin"
;  :jar-name "cjd-doc.jar"
;  :uberjar-name "cjd.jar"
  :main cjd.main
  :javac-options {:destdir "classes/"}
  )
