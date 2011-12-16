(ns cjd.main
  (:use
    [cjd.exome]
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
   -css <css-file>[;<css-file>...] 
      A semicolon-separated list of alternative CSS file names to 
      be used.
   -help
      Prints this very message.
   -nogen 
      Inhibits HTML generation; only parses input.
   -overview <overview> 
      The name of a .clj file, the first CJD comment of which will 
      be used as the summary statement for the generated 
      documentation. 
   -require <ns>[;<ns>...] 
      A semicolon-separated list of namespaces containing extensions
      to the base CJD functionality.
   -theme <theme>
      Specifies <theme> as the styling theme for generated output.
      Current standard themes include: 
          light   Light backround, black text (the default) 
          dark    Black background, white text 
   -throw
      Causes any warning to generate an exception
   -title <title>  
      Use <title> as the title for the documentation.
   -v <vopts>
      Sets output verbosity. <vopts> is a string of single-letter 
      selectors. \"n\"
"))


#_ (* Main method for cjd.
      )
(defn -main [& args]
  (let [[opts remaining] 
        (loop [[arg & remains+ :as remaining+] args
               opts+ { }]  ; <-- default options go here
          (let [[param & remains*] remains+
                [_ opt] (if arg (re-matches #"-(.*)" arg))
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
                
                "css"
                (if param
                  (let [csss (vec (.split param ";"))]
                    (recur remains* (assoc opts+ :css csss)))
                  (throw (CJDException. "Missing parameter for --css")))
              
                "require"
                (if param
                  (let [reqs (vec (.split param ";"))]
                    (recur remains* (assoc opts+ :require reqs)))
                  (throw (CJDException. "Missing parameter for --require")))
              
                ; "exclude"
                
                "throw" (recur remains+ (assoc opts+ :throw-on-warn true))
                
                "nogen" (recur remains+ (assoc opts+ :nogen true))
                
                "help" []
                
                "theme"
                (if param
                  (recur remains* (assoc opts+ :theme (symbol param)))
                  (throw (CJDException. "Missing parameter for --theme")))
                
                "v"
                (let [vopts (if attached attached param)
                      remains** (if attached remaining+ remains*)]
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

