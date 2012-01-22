#_ ( Copyright (c) 2011 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Main function for standalone operation.
      )
(ns cjd.main
  (:use
    [cjd.exome]
    [cjd.version]
    )
  (:import
    [cjd CJDException])
  (:gen-class))


(defn cjd-help []
  (println "usage: java -cp <classpath> cjd.main [option...] dest-dir source...
where:
  dest-dir 
      The pathname of the output directory. If it doesn't exist, 
      it will be created.
  source... 
      One or more file or directory pathnames containing 
      Clojure files to be included in the CJD documentation run. 
      If a pathname designates a directory, that directory will be 
      searched for files with names ending in \".clj\".
 
   option... is a list of options, which can be a combination of:
   --css <css-file>[;<css-file>...] 
      A semicolon-separated list of alternative CSS file names to 
      be used.
   --help
      Prints this very message.
   --index
      Specifies the name of an overview/index document relative to
      the directory specified by dest-dir. Defaults to \"index.html\".
   --nogen 
      Inhibits HTML generation; only parses input.
   --noindex
      Inhibits index generation.
   --overview <overview> 
      The name of a .clj file, the first CJD comment of which will 
      be used as the summary statement for the generated 
      documentation in the overview/index document. 
   --requires <ns>[;<ns>...] 
      A semicolon-separated list of namespaces containing extensions
      to the base CJD functionality.
   --showopts 
     Debug tool that prints a list of the options as extracted from 
     command-line arguments.
   --theme <theme>
      Specifies <theme> as the styling theme for generated output.
      Current standard themes include: 
          light   Light backround, black text (the default) 
          dark    Black background, white text 
   --throw
      Causes any warning to generate an exception
   --title <title>  
      Use <title> as the title for the documentation.
   --v <vopts>
      Sets output verbosity. <vopts> is a string of single-letter 
      selectors. \"n\"
   --version
      Prints the version string for CJD and exits.
"))


#_ (* Main method for cjd.
      )
(defn -main [& args]
  (prn 'cjd.args args)
  (let [[opts remaining] 
        (loop [[arg & remains+ :as remaining+] args
               opts+ { }]  ; <-- default options go here
          (let [[param & remains*] remains+
                [_ opt] (if arg (re-matches #"--(.*)" arg))
                [_ xopt attached] (if arg (re-matches #"-(\p{Alpha})(.*)" arg))]
            (cond 
              opt  ; multicharacter option, ala --title
              (condp = opt
                "title" 
                (if param
                  (recur remains* (assoc opts+ :title param))
                  (throw (CJDException. "Missing parameter for --title")))
                
                "overview"
                (if param
                  (recur remains* (assoc opts+ :overview param))
                  (throw (CJDException. "Missing parameter for --overview")))
                
                 "index"
                (if param
                  (recur remains* (assoc opts+ :index param))
                  (throw (CJDException. "Missing parameter for --index")))
                
                "css"
                (if param
                  (let [csss (vec (.split param ";"))]
                    (recur remains* (assoc opts+ :css csss)))
                  (throw (CJDException. "Missing parameter for --css")))
              
                "requires"
                (if param
                  (let [reqs (vec (.split param ";"))]
                    (recur remains* (assoc opts+ :require reqs)))
                  (throw (CJDException. "Missing parameter for --require")))
              
                "exclude"
                (if param
                  (let [reqs (vec (.split param ";"))]
                    (recur remains* (assoc opts+ :exclude reqs)))
                  (throw (CJDException. "Missing parameter for --exclude")))
                
                "nogen" (recur remains+ (assoc opts+ :nogen true))
                
                "noindex" (recur remains+ (assoc opts+ :no-index true))
                
                "help" 
                (do
                  (cjd-help)
                  (System/exit 0))
                
                "showopts" (recur remains+ (assoc opts+ :showopts true))
                
                "theme"
                (if param
                  (recur remains* (assoc opts+ :theme (keyword param)))
                  (throw (CJDException. "Missing parameter for --theme")))
                
                "throw" (recur remains+ (assoc opts+ :throw-on-warn true))
                
                "version"
                (do 
                  (println *cjd-version*)
                  (System/exit 0))
                
                "v"
                (let [vopts param #_(if attached attached param)
                      remains** remains* #_(if attached remaining+ remains*)]
                  (if (empty? vopts)
                    (throw (CJDException. "Missing parameter for -v"))
                    (recur remains** 
                           (assoc opts+ :v (set (map (fn [ch] (keyword (str ch))) vopts))))))
                 
                ; :else
                (throw (CJDException. (str "Unrecognized option: " opt))))
              
              #_xopt  ; single-character option, ala -v
              #_(condp = xopt
                "v"
                (let [vopts (if attached attached param)
                      remains** (if attached remaining+ remains*)]
                  (if (empty? vopts)
                    (throw (CJDException. "Missing parameter for -v"))
                    (recur remains** 
                           (assoc opts+ :v (set (map (fn [ch] (keyword (str ch))) vopts))))))
                
                ; :else
                (throw (CJDException. (str "Unrecognized option: " opt))))
              
              :else 
            [opts+ remaining+])))  ; <<--- EXIT
        
        [out-dir & files] remaining]
    #_(if-not out-dir 
      (throw (CJDException. "Missing output directory / input files")))
    #_(if (empty? files) 
      (throw (CJDException. "Missing input files")))
    (if (and out-dir  (not-empty files))
      (cjd-generator (vec files) out-dir opts)
      (cjd-help))))

