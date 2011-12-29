#_ ( Copyright (c) Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )

#_ (* Top-level driver routines for the CJD documentation generation process.
      )
(ns cjd.exome
  (:import 
    [clojure.lang LineNumberingPushbackReader NotTheLispReader]
    [java.io File FileReader StringReader]
    [cjd CJDException]
    )
  (:use
    [cjd.util.defnk]
    [cjd.context]
    [cjd.generate]
    [cjd.artifact-base]
    [cjd.core-artifacts]
    [cjd.resolver]
    [cjd.link-resolver]
    [cjd.resource]
    )
  (:gen-class)
  )

#_ (* Reads the next form from the specified input reader. 
      @(p This is essentially identical in behavior to @(link clojure.core/read),
          except that it uses the CJD reader.)
      @arg reader A @(link java.io.Reader Reader) object, which must also implement
      the @(link java.io.PushbackReader PushbackReader) interface.
      @arg eof-error? @(c true), if end of file is to be considered as an error. If true 
      and a read is attempted past EOF, an exception is thrown. Defaults value is false.
      @arg eof-value A value to be returned if end-of-file is encountered. Only meaningful
      if @(arg eof-error?) is @(c false).
      @arg recursive? True, if ...
      @return The next form from the reader.
     )
(defn cjd-read
  ([reader]
   (read reader false nil))
  ([reader eof-error? eof-value]
   (read reader eof-error? eof-value false))
  ([reader eof-error? eof-value recursive?]
   (NotTheLispReader/read reader (boolean eof-error?) eof-value recursive?)))

(defn- basename [filename] 
  (let [lastf (.lastIndexOf filename "/")
        lastb (.lastIndexOf filename "\\")
        last (cond (> lastf lastb) lastf
                   (> lastb lastf) lastb
                   :else -1)]
    (.substring filename (inc last))))

#_ (* Repetitively reads a Clojure file using @(l cjd-read), and returns a 
      sequence of the forms contained therein.
      @arg file-or-name Identifies the file to extract forms from. Must be
      either a string containing the file name, or @(link java.io.File File)
      object.
      @returns A sequence of forms read from the file.
      )
(defn forms-from-file [file-or-name]
  (let [file (if (instance? File file-or-name) file-or-name (File. file-or-name))
        fname (basename (.getPath file))]
    (if (.canRead file)
      (with-open [reader (LineNumberingPushbackReader. (FileReader. file ))]
        (loop [form (cjd-read reader false nil false )
               forms []]
          (if form
            (recur
              (cjd-read reader false nil false )
              (conj forms (vary-meta form assoc :file fname)))
            (do
              (.close reader)
              forms)))))))


#_ (* Repetitively reads from a string using @(l cjd-read), and returns a 
      sequence of the forms contained therein. This is intended primarily 
      as a debugging tool.
      @arg string The string to read forms from.
      @returns A sequence of forms read from the string.
      )
(defn forms-from-string [string]
  (with-open [reader (LineNumberingPushbackReader. (StringReader. string))]
    (loop [form (cjd-read reader false nil false )
           forms []]
      (if form
        (recur
          (cjd-read reader false nil false )
          (conj forms form))
        (do
          (.close reader)
          forms)))))

#_ (* Accepts a sequence of Clojure forms from some source, and motivates a 
      parsing exercise on them, to result in a rough abstract syntax tree
      that serves as the basis for further processing.
      @arg context The current context object.
      @arg forms A sequence of forms as generated, e.g. by @(link forms-from-file).
      @(returns 
         A tuple of the form @(form [ns-obj artifacts]), where
         @arg ns-obj The namespace artifact from the source, if there 
         was one.
         @arg artifacts A sequence of artifact objects from the source.)
      )
(defn endoculate [context forms]
  (binding [artifact-msg (fn [& things] (msg context :e (apply str things)))]
    (let [[ns-artifact _ artifacts] 
        (reduce 
          (fn [[ns-artifact+ last-doc artifacts+] form] 
            (if (cjd-doc? form)
              [ns-artifact+ form artifacts+]
              (let [artifact (cjd-artifact form ns-artifact+ last-doc)]
                (if artifact 
                  (if (namespace-artifact? artifact)
                    [artifact nil artifacts+]
                    [ns-artifact+ nil (conj artifacts+ artifact)])
                  [ns-artifact+ nil artifacts+]))))
          [nil nil []]
          forms)]
    (if ns-artifact   
      (set-artifacts ns-artifact artifacts))
    [ns-artifact artifacts])))

#_ (* Determines if a file should be excluded from processing.
      @arg file A @(linki java.io.File) object representing the
      file to be checked. Note that as a mindless convenience, 
      the file's path name separator characters are all translated into
      slashes "/".
      @(arg exclusions A collection of objects which are either 
            @(ul 
               @li A string representing the prefix of file names to be excluded.
               @li A regex pattern.)
            If @(arg file)'s path name starts with any of the strings, or matches any
            of the patterns, it is excluded.)
      @returns Truth if the file should be excluded.
      )
(defn excluded-file [^File file exclusions]
  (let [fname 
        (if (= (java.io.File/separator) "\\")
          (.replaceAll (.getPath file) "\\\\" "/")
          (.getPath file))]
    (some (fn [exclusion] 
       #_(prn "ex:" fname exclusion)
       (if (= (type exclusion) java.util.regex.Pattern)
              (re-matches exclusion fname)
              (.startsWith fname exclusion))) 
          exclusions))) 


#_ (* Starting from a collection of file objects, checks to see if each file 
      is directly of interest (e.g., is readable, ends with ".clj", isn't excluded), 
      or if it's a directory, in which case its contents are recursively analyzed.
      
      @arg file-set The set of @(link java.io.File File) objects that 
      have been accumulated to date. @name adds to this set.
      @arg files A collection of @(link java.io.File File) objects to be examined 
      for inclusion.
      @arg exclusions A set of exclusion criteria; see @(l excluded-file) for details. 
      
      @returns @(arg file-set), augmented with any files discovered locally.
      )
(defn find-files [file-set files exclusions]
  (reduce 
    (fn [file-set+ ^File file] 
      (cond
        (.isDirectory file) (find-files file-set+ (.listFiles file) exclusions)
    
        (and (.canRead file) 
             (.endsWith (.getPath file) ".clj")
             (not (excluded-file file exclusions))) (conj file-set+ file)
    
        :else file-set+))
    file-set files ))


#_ (* Main CJD document processing driver.
      
      @arg sources A file or directory name (string), or collection of files or 
      directory names, that are searched or examined for candidate clojure 
      (".clj") files to be included in the documentation generation process.
      
      @arg out-dir The name (string) of the top-level directory where generated
      documentation files are to be placed.
      
      @(arg options A map describing options for @(name).
        @(opt :exclude A specification, or collection of specifications,
              for files to be excluded from those implied by 
              @(arg sources). This is a collection of objects which are either 
              @(ul 
                 @li A string representing a prefix of file names to be excluded.
                 @li A regex pattern.)
              @p If a path name starts with any of the strings, or matches any
              of the patterns, it is excluded.
              As a convenience, path separator characters are all translated to
              "/" before making the comparison. @(i Sic semper fenestrae!))
      
        @opt :requires The name (symbol or string) of a namespace, 
        or a collection of namespace names,
        to be added to the base state of affairs during comment processing.
        This supports extensions for such purposes as added artifacts or alternative
        generation methods. Namespaces can be specified as symbols or strings.
        
        @opt :css  Name, or a collection of names, of the CSS 
        document or documents to use. These documents replace the standard CSS documents.
        
        @opt :title  A title for the body of documentation. This will appear in 
        the document title of all generated pages.
        
        @(opt :overview  The name of a ~".clj" file containing a CJD-comment
        that notionally describes the overall body of work being documented by
        the @name run. The overview text is taken from the first CJD-doc comment 
        in the file. 
        @p As a suggestion, note that a leiningen "project.clj" file works excellently for this
        purpose!)
        
        @option :throw-on-warn false If true, warnings throw exceptions instead of just
        printing a warning message.
        
        @option :nogen false Intended as a development tool. When true, suppresses the
        HTML generation phase, and causes @name to
        return the the results of the pre-HTML-generation phase of the operation.
        
        @(option :v #{ :n :g } A set of keys describing the desired level of 
                 output. Keys can be any of\:
                 @key :f produces a message when starting the processing of a file.
                 @key :e produces a message when an artifact has been parsed.
                 @key :n produces a message when HTML generation starts for a 
                 new namespace.
                 @key :a produces a message when generation starts for a 
                 new artifact.  
                 ) 
        @(opt :theme If non-nil, selects a style theme, as defined by a 
                 set of CSS files. Standard options include\:
                 @key :light Generates light-field (white background) pages (the default) .
                 @(key :dark Generates dark-field (black background) pages.)
                 @p Note that @(option :theme) is not meaningful if the @(arg :css) 
                 option is specified.) 
        @(opt :footer Either a function, or a symbol that resolves to a function, that 
              generates a footer used on each generated web page. 
              The function has the form @(fun (fn [context])), where
              @arg context The current context object.
              @returns A string containing HTML to be inserted as the footer.)
        @(opt :header Either a function, or a symbol that resolves to a function, 
              that generates a header used on each generated web page. 
              The function has the form @(fun (fn [context])), where
              @arg context The current context object.
              @returns A string containing HTML to be inserted as the header.)
        ) 
      
      @(returns nil, unless the @(option :nogen) option is set, in which case it returns
        results of the early phases of operation.
        @p In the latter case, the result has the form 
        @(form [ns-artifacts base-context]), where
        @arg ns-artifacts The collection of all @(linki cjd.core_artifacts.Namespace) 
        artifacts gleaned from the source files.
        @arg base-context The context object as it stands at the start of the 
        generation process.)
      )
(defn cjd-generator [sources out-dir options] 
  (let [{ :keys [exclude requires css title overview throw-on-warn 
                 nogen v theme header footer] 
           :or { :css "cjd.css"}} options
        outdir (File. out-dir)
        exclusions (if (coll? exclude) exclude [exclude])
        file-set (find-files (sorted-set) 
                             (map #(File. %) (if (coll? sources) sources [sources]))
                             exclusions)
        overview-file (if overview (File. overview))
        pre-context (-> (make-Context)
                      (context-verbiage! v)
                      (context-theme! theme))]
    (if (empty? file-set)
      (throw (CJDException. "No files found")))
    (if-not (and (.exists outdir) (.isDirectory outdir))
      (.mkdirs outdir))
    (if (and overview-file (not (.canRead overview-file)))
      (throw (CJDException. "Overview file not found")))
    (when-not css 
      (add-css "cjd.css")
      (copy-resource 
        (fn [context]
          (if (= (context-theme context) :dark)
            ["cjd.css" "/cjd/resources/cjd-r.css"]
            ["cjd.css" "/cjd/resources/cjd-f.css"]))))
    ;; Doing a reset here collides with the fact that extensions (or stuff loaded by
    ;; extensions) aren't reloaded  after an initial run of cjd-generator. 
    ;; This is a a non-issue for one-off runs, from command-line or lein, 
    ;; but can produce a notable absence 
    ;; of link resolvers if one does multiple runs from the REPL.
;   (reset-resolver-fns)
    (if requires
      (doseq [req (if (coll? requires) requires [requires])]
        (require :reload (symbol req))))
    (init-artifacts)
    (reset-resolver)
    
    #_(println "File-set:" file-set)
    (let [overview-doc 
          (if overview-file
            (let [c (first (forms-from-file overview-file))]
              (if (and c (cjd-doc? c)) c nil)))
          [ns-artifacts name-map]
          (reduce 
            (fn [[ns-artifacts+ name-map+] file]
              (let [filename (.getPath file)
                    _ (msg pre-context :f (str "processing " filename))
                    [ns-artifact artifacts] (endoculate pre-context (forms-from-file filename))
                    ns-name (if ns-artifact (artifact-name-of ns-artifact) nil)]
                (if ns-artifact 
                  (let [name-map* 
                        (reduce 
                          (fn [name-map++ artifact]
                            (if (has-doc? artifact)
                              (assoc name-map++ (artifact-name-of artifact) ns-name)
                              name-map++)) 
                          name-map+ (artifacts-of ns-artifact))]
                    [(conj ns-artifacts+ ns-artifact) name-map*])
                  (do
                    (warn pre-context (str "Warning: No namespace found --- " filename))
                    [ns-artifacts+ name-map+]))))
            [[] (sorted-map)] 
            file-set)
          nss (set (map artifact-name-of ns-artifacts))
          base-context 
          (-> pre-context
            (context-css! 
              (if css 
                (if (coll? css) css [css])
                @css-docs*))
            (context-namespaces! nss)
            (context-title! title)
            (context-header! 
              (if (symbol? header)
                (try
                  (let [xns (namespace header)] 
                    (if-not (empty? xns)
                      (require (symbol xns))))
                  (var-get (resolve header))
                  (catch Exception e 
                    (throw (CJDException. "Unable to find header function" e))))
                header))
            (context-footer! 
              (if (symbol? footer)
                (try
                  (let [fns (namespace footer)] 
                    (if-not (empty? fns)
                      (require (symbol fns))))
                  (var-get (resolve footer))
                  (catch Exception e 
                    (throw (CJDException. "Unable to find footer function" e))))
                footer))
            (context-overview! overview-doc)
            (context-throw-on-warn! throw-on-warn))]
      
      (if nogen
        [ns-artifacts base-context]
        (do 
          (let [overview (gen-overview ns-artifacts base-context)
                destfile (File. outdir "index.html")]
            (spit (.getPath destfile) overview))
          (doseq [ns-artifact ns-artifacts]
            (let [ns-name (artifact-name-of ns-artifact)
                  destfile (File. outdir (str ns-name ".html"))
                  destgoop (gen-namespace ns-artifact base-context)]
              (spit (.getPath destfile) destgoop)))
          (doseq [res-fn @resource-fns*]
            (let [[doc-name resource-name] (res-fn base-context)]
              (if (and doc-name resource-name)
                (try
                  (let [destfile (File. outdir doc-name)
                        content (slurp (.getResourceAsStream Object resource-name))]
                    (msg base-context :x
                         (str "Copying " resource-name " as " (.getPath destfile)))
                    (spit (.getPath destfile) content))
                  (catch Throwable e 
                      (warn base-context 
                          (str "Exception copying resource " resource-name " to " doc-name
                               ":\n   " (.getMessage e))))))))
          nil)))))

#_ (* @(link defnk) :nb -based alternate front-end for @(link cjd-generator), intended for easy REPL invocation.
      @p Arguments and results are as described in @(link cjd-generator), except that
      options can be specified as keyword-value argument pairs.
      )
(defnk cjd-gen [sources out-dir :exclude nil :requires nil 
                :css nil :title nil 
                :overview nil :throw-on-warn false :nogen false
                :v #{ :f :n } :theme :light :header nil :footer nil] 
  (cjd-generator sources out-dir 
                 { :exclude exclude :requires requires
                  :css css :title title
                  :overview overview :throw-on-warn throw-on-warn 
                  :nogen nogen :v v :theme theme :header header :footer footer}))


