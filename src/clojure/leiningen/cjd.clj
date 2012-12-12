#_ ( Copyright (c) 2011 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Leiningen plugin for CJD.
      @p Most of what goes on here is a desperate trial-and-error attempt to 
      select the appropriate version of Leiningen. The actual work is done
      in one or the other of 
      the @(l leiningen.cjd.lein1) or @(l leiningen.cjd.lein2) namespaces.
      )
(ns leiningen.cjd)

(try
  (require 'leiningen.classpath)
  (load "/leiningen/cjd/lein1")
  (def lein-version 1)
  (def cjd-fn  'leiningen.cjd.lein1/cjd-1)
  (catch Exception e
    (try
      (require 'leiningen.core.classpath)
      (load "/leiningen/cjd/lein2")
      (def lein-version 2)
      (def cjd-fn 'leiningen.cjd.lein2/cjd-2)
      (catch Exception e
        (throw (Exception. "Could not load CJD Leiningen support!" e))))))

#_ (* CJD's front end for leiningen.
      
      @p @name selects source locations, in order of preference, from\:
      @(ul @li The :cjd-source-path option in the :cjd-opts option map. Note that this 
           can be either a single string, or a set of strings. CJD will search 
           any directories for .clj files.
           @li The :source-path option in the project map.
           @li "src" in the current directory.) 

      @p @name selects the destination directory, in order of preference, from\:
      @(ul @li The :doc-path value in the :cjd-doc option map\;
           @li The :doc-path value in the @(arg project) map\;
           @li "doc" in the current directory.) 

      @p @name extracts all other options from a map associated the :cjd-opts key 
      in the @(arg project) map. Options are as defined by @(l cjd.exome/cjd-generator).

      @arg project The leiningen project map, as derived from the project.clj file.
      @returns 0 , if document generation appears to have succeeded, and 1 otherwise.
      )
(defn cjd 
  "  Extracts CJD comments from Clojure sources and generates a HTML documentation tree.
   
	Specify CJD options by placing them in a map associated with the :cjd-doc key in the
	in the main project map. 
	
	CJD selects source locations, in order of preference, from:
	-- The :cjd-source option in the project map. Note that this can be
	   either a single string, or a collection of strings. CJD will search any 
     directories for .clj files.
	-- The :source-paths option in the project map.
	-- \"src\" in the current directory. 
	
	CJD selects the destination directory, in order of preference, from:
	-- The :cjd-dest value in the project map;
	-- \"doc\" in the current directory. 
	
	CJD takes all other options from a map associated the :cjd-opts key. Options and
	values are as described at: 
	    http://greenh.github.com/CJD/doc/dark/cjd.exome.html#cjd-generator.
"
  [project]
  ; (println "Using Leiningen" lein-version) 
  (let [form `(~cjd-fn '~project)
        ; _ (prn 'form--> form)
        ] 
    (eval form)))


