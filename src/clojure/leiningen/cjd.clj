#_ ( Copyright (c) 2011 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Leiningen plugin for CJD.
      )
(ns leiningen.cjd
  "Generates a HTML documentation tree from CJD source comments."
  #_(:use
    [cjd.exome]
    )
  (:require 
    [leiningen.classpath :as classpath]
    )
  (:import 
    [java.io File]
   ; [cjd CJDException]
    )
  )

(defn- get-classpath-string [project]
  (let [cp (classpath/get-classpath project)]
    (apply str (interpose File/pathSeparatorChar (map str (filter #(not (nil? %)) cp))))))


#_ (* Conditionally does a @(c conj).
      @arg col A collection to conditionally be added to.
      @arg tf Expression to be tested.
      @arg vals zero or more values to be added to @(arg col).
      @returns If @(arg tf) is truth, @(arg col) with @(arg vals) :br
      @(l clojure.core/conj) 'ed to
      it\; if not, @(arg col) .
      )
(defn cconj [col tf & vals] 
  (if tf (apply conj col vals) col))


#_ (* Converts any relative file names in a list to absolute file names
      relative to a specified target directory.
      @arg target-dir A string containing the target directory name.
      @arg file-name-or-names A file name string, or a collection of file name strings.
      @returns Absolute file name string or strings. This is a collection or
      a single string, based on the form of @(arg file-name-or-names).
      )
(defn absfile [target-dir file-name-or-names]
  (let [td (File. target-dir)]
    (letfn [(absf [file-name]
              (let [f (File. file-name)]
                (if (.isAbsolute f)
                  file-name
                  (let [ff (File. td file-name)]
                    (.getPath ff)))))] 
      (if (coll? file-name-or-names)
        (map absf file-name-or-names)
        (absf file-name-or-names)))))

#_ (* Creates a thread that copies data emanating from an input stream 
      (as from a process's output pipe) to an output stream.
      @arg from-stream An input stream.
      @arg to-stream The destination output stream.
      @returns )
(defn repeater [from-stream to-stream] 
  (proxy [Thread] []
    (run [] 
      (let [buf (byte-array 1024)]
        (try 
          (loop [chrs (.read from-stream buf)]
            (if (> chrs 0)
              (do
                (.write to-stream buf 0 chrs)
                (.flush to-stream)
                (recur (.read from-stream buf)))))
          (catch Exception e (.printStackTrace e)))))))

#_ (* "Massages" identifiers for things like the :requires option.
      @arg An identifier, quoted or not, or a list of identifiers, quoted or not.
      @returns A collection of unquoted idenfitiers.
      )
(defn massage [stuff]
  (cond
    (and (list? stuff) (= (first stuff) 'quote)) [(second stuff)]
    (coll? stuff) (map (fn [item] (if (and (list? item) (= (first item) 'quote))
                                    (second item) item)) 
                       stuff)
    :else [stuff]))

#_ (* Front end for leiningen.
      
      @name selects source locations, in order of preference, from\:
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
	-- The :cjd-source-path option in the project map. Note that this can be
	   either a single string, or a collection of strings. CJD will search any 
     directories for .clj files.
	-- The :source-path option in the project map.
	-- \"src\" in the current directory. 
	
	CJD selects the destination directory, in order of preference, from:
	-- The :cjd-dest-path value in the project map;
	-- \"doc\" in the current directory. 
	
	CJD takes all other options from a map associated the :cjd-opts key. Options and
	values are as described at: 
	    http://greenh.github.com/CJD/doc/dark/cjd.exome.html#cjd-generator.
"
  [project]
  (try
    (let [ { opts :cjd-opts target-dir :target-dir } project
          s1 (or (:cjd-source project) (:source-path project) "src")
          sources (if (coll? s1) s1 [s1])
          dest (or (:cjd-dest project) "doc")
          ]
      (let [sep (File/separator)
            jbin (File. (str (System/getProperty "java.home") sep "bin"))
            ;_ (println "java -- " (.getPath jbin))
            jexe (File. jbin "java.exe")
            jj (File. jbin "java")
            java (cond
                   (.canExecute jexe) jexe
                   (.canExecute jj) jj
                   :else (throw (Exception. "Can't find java executable")))
            { :keys [exclude requires css title overview throw-on-warn 
                     nogen v theme header footer index noindex showopts
                     all docstrings] } opts
            cp (get-classpath-string project)
            ; _ (println cp)
            args
            (-> [(.getPath java) "-cp" cp "cjd.main"]
              (cconj exclude "--exclude" 
                     (if (coll? exclude) (apply str (interpose ";" exclude)) exclude))
              (cconj requires "--requires" 
                     (apply str (interpose ";" (massage requires))))
              (cconj css "--css" 
                     (if (coll? css) (apply str (interpose ";" css)) css))
              (cconj title "--title" (str title))
              (cconj overview "--overview" overview)
              (cconj throw-on-warn "--throw")
              (cconj nogen "--nogen")
              (cconj v "--v" (apply str (map name v)))
              (cconj theme "--theme" (name theme))
              (cconj header "--header" (str header))
              (cconj footer "--footer" (str footer))
              (cconj index "--index" (str index))
              (cconj noindex "--noindex")
              (cconj showopts "--showopts")
              (cconj all "--all")
              (cconj docstrings "--docstrings")
              (conj dest)
              (concat sources)
              )
            _ (if showopts
                (println "Running:\n" 
                         (apply str (interpose " " args))))
            proc-builder (ProcessBuilder. (into-array String args))
            proc (.start proc-builder)
            ]
        (.start (repeater (.getInputStream proc) System/out))
        (.start (repeater (.getErrorStream proc) System/err))
        (.waitFor proc)
        (println "Done, status " (.exitValue proc))
        ))
    (catch Throwable t (.printStackTrace t))))
