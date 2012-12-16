#_ ( Copyright (c) 2011 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Support for CJD in Leiningen ~"2.x".
      )
(ns leiningen.cjd.lein2
  (:use
    ; [cjd.exome]
    [cjd.version]
    )
  (:require 
    [leiningen.core.classpath :as classpath]
    [leiningen.core.main :as main]
    [leiningen.core.eval :as lein-eval] 
    )
  (:import 
    [java.io File]
    )
  )

#_ (* Front-end back-end function for leiningen ~"2.*". Interface as defined by 
      @(l leiningen.cjd/cjd).
      
      @p @(u Much) nicer than version-1 lein... just call @(il cjd-generator) without further ado. 
      )
(defn cjd-2 [project]
  (try
    (let [ { opts :cjd-opts } project
          s1 (or (:cjd-source project) (:source-paths project) "src")
          sources (if (coll? s1) s1 [s1])
          dest (or (:cjd-dest project) "doc")
          prj  (update-in project [:dependencies] conj ['cjd *cjd-version*])
          ]
      (lein-eval/eval-in-project prj 
         `(cjd.exome/cjd-generator ~sources ~dest ~opts) `(require 'cjd.exome)))
    (catch Throwable t (.printStackTrace t))))

