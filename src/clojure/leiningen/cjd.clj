#_ (* Leiningen plugin for CJD.
      )
(ns leiningen.cjd
  "Generates a HTML documentation tree."
  (:use
    [cjd exome]
    )
  (:import 
    [java.io File])
  )

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

#_ (* Front end for leiningen.
      
      @name selects source locations, in order of preference, from\:
      @(ul @li The :source-paths option in the :cjd-doc option map. Note that this 
           can be either a single string, or a set of strings. CJD will search 
           any directories for .clj files.
           @li The :source-path option in the project map.
           @li "src" in the current directory.) 

      @p @name selects the destination directory, in order of preference, from\:
      @(ul @li The :doc-path value in the :cjd-doc option map\;
           @li The :doc-path value in the @(arg project) map\;
           @li "doc" in the current directory.) 

      @p @name extracts all other options from a map associated the :cjd-doc key 
      in the @(arg project) map. Options are as defined by @(l cjd-generate).

      @arg project The leiningen project map, as derived from the project.clj file.
      @returns 0, if document generation appears to have succeeded, and 1 otherwise.
      )
(defn cjd 
"Extracts CJD comments from Clojure sources and generates a HTML documentation tree.
   
Specify CJD options by placing them in a map associated with the :cjd-doc key in the
in the main project map. 

CJD selects source locations, in order of preference, from:
-- The :source-paths option in the :cjd-doc option map. Note that this can be
   either a single string, or a set of strings. CJD will search any directories
   for .clj files.
-- The :source-path option in the project map
-- \"src\" in the current directory. 

CJD selects the destination directory, in order of preference, from:
-- The :doc-path value in the :cjd-doc option map;
-- The :doc-path value in the project map;
-- \"doc\" in the current directory. 

CJD takes all other options from a map associated the :cjd-doc key.
"
  [project]
  (let [ { opts :cjd-doc target-dir :target-dir } project
        s1 (or (:source-paths opts) (:source-path project) "src")
        s2 (if (coll? s1) s1 [s1])
        sources (absfile target-dir s2)
        dest (absfile target-dir (or (:cjd-path opts) (:cjd-path project) "doc"))
        overview* (if-let [ov (:overview opts)] (absfile target-dir ov))
        opts* (if (and opts overview*) (assoc opts :overview overview*)  opts)]
    (prn 'project project)
    (prn 'source sources)
    (prn 'dest dest)
    (prn 'opts opts*)
    (cjd-generator sources dest opts*)
    ))
