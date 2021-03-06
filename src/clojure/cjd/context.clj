#_ ( Copyright (c) 2011 - 2012 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Defines a "context" object, a record containing 
      miscellaneous information about the state of affairs currently in effect
      during CJD documentation generation processing. 
      @p The object's contents are situationally updated as it propagates 
      throughout the CDJ comment-handling process.  
      )
(ns cjd.context
  (:use 
    [cjd.version]
    [cjd.util.string-utils]
    [extensomatic.extensomatic]
    )
  (:import 
    [cjd CJDException]
    )
  )

#_ (* A minor print method to allow @(c ~"(clojure.core/deref whatever)") 
      to be printed out as @(c ~"@(whatever)") or @(c ~"@whatever"), as 
      appropriate. 
      )
(defmethod print-method clojure.lang.Cons [form ^java.io.Writer w]
  (let [[funct & stuff] form]
    (if (= funct 'clojure.core/deref)
      (do
        (.write w "@")
        (if (= (count stuff) 1)
          (let [[item] stuff]
            (print-method item w))
          (print-method stuff w)))
      (print-method (apply list form) w))))

#_ (* Defines a small set of output functions, notionally for use wherever there's a
      @(l Context) object within CJD.)
(defprotocol Messaging
  (msg 
    [context opt who stuff]
    [context opt stuff])
  (warn 
    [context form stuff]
    [context stuff])
  (error 
    [context stuff]
    [context form stuff])
)

#_ (* The context object. 
      @p A @name object serves to aggregate all of the contextual information
      in effect at any time within a running instance of CJD. The specific
      content varies wildly\; some fields are fixed throughout an execution,
      while others are set based on whatever happens to be going on at a
      specific point in program execution.
      
      @p By convention all fields are initialized to null values, then 
      initialized by @(link cjd.exome/cjd-gen cjd-gen) (or elsewhere) 
      using the supplied methods, as needed.
      @p As is typical, we prefer methods to ad-hoc use of keywords for
      field access, and hence the record structure. A few more bugs
      wrung out at compile time...
      )
(defconstructo Context [] 
  [(file nil) (line 0) (name nil) (namespace nil) 
   (nss #{}) (munged-ns-map nil) (level 0) (items {}) (css nil)
   (title nil) (overview nil) (throw-on-warn nil) 
   (gen-time (java.util.Date.)) (version *cjd-version*) 
   (verbiage #{}) (theme :light) (header nil) (footer nil) (index-name nil)
   (all-public nil) (use-docstrings nil) (artifact-filter nil)
   (extracted false)]
  
  (context-file [context] file)
  (context-file! [context file] (assoc context :file file))
  (context-line [context] line)
  (context-line! [context line] (assoc context :line line))
  (context-location [context] (str (context-file context) ":" (context-line context)))
  
  (context-name [context] name)
  (context-name! [context name] (assoc context :name name))
  
  #_ (* Returns the @(linki cjd.core_artifacts.Namespace) artifact of the namespace in which
        a context artifact is defined.)
  (context-ns [context] namespace)
  (context-ns! [context ns] (assoc context :namespace ns))
  
  (context-namespaces [context] nss)
  (context-namespaces! [context nss] (assoc context :nss nss))
  
;  (context-munged-ns-map [context] munged-ns-map)
;  (context-munged-ns-map! [context nss] (assoc context :munged-ns-map nss))
  
  (context-level [context] level)
  (context-level! [context level] (assoc context :level level))
  
  (context-items [context] items)
  (context-item! [context item level] 
    (assoc context :items (assoc (context-items context) item level)))
  (context-item-level [context item] (get (context-items context) item))
  
  (context-css [context] css)
  (context-css! [context css] (assoc context :css css))
  
  (context-title [context] title)
  (context-title! [context title] (assoc context :title title))
  
  (context-overview [context] overview)
  (context-overview! [context overview] (assoc context :overview overview))
  
  (context-throw-on-warn [context] throw-on-warn)
  (context-throw-on-warn! [context t-o-w] (assoc context :throw-on-warn t-o-w))
  
  #_ (* Returns a string indicating what version of CJD is running.)
  (context-version [this] version)
  #_ (* Returns a @(il java.util.Date) containing the time CJD was run.)
  (context-gen-time [this] gen-time)
  
  (context-verbiage [context] verbiage)
  (context-verbiage! [context v] (assoc context :verbiage v))
  
  (context-theme [context] theme)
  (context-theme! [context t] (assoc context :theme t))
  
  #_ (* Returns a string containing the output directory-relative name
        of the index file (nominally "index.html"), or nil if index
        generation is suppressed.)
  (context-index [context] index-name)
  (context-index! [context index-name] (assoc context :index-name index-name))
  
  #_ (* Returns the header generation function, or nil if none was specified. 
        @p See @(il cjd.exome/cjd-generator) for details.)
  (context-header [context] header)
  (context-header! [context h] (assoc context :header h))
  
  #_ (* Returns the footer generation function, or nil if none was specified. 
        @p See @(il cjd.exome/cjd-generator) for details.)
  (context-footer [context] footer)
  (context-footer! [context f] (assoc context :footer f))
  
  (context-all-public [context] all-public)
  (context-all-public! [context t] (assoc context :all-public t))
  
  (context-docstrings [context] use-docstrings)
  (context-docstrings! [context t] (assoc context :use-docstrings t))
  
  #_ (* Returns an artifact filter function, used to select artifacts
        for inclusion in the output.
        @(returns A function that tests whether an artifact should be included
                  in the output, of the form @(fun [context artifact]), where
                  @arg context The current context object.
                  @arg artifact The artifact to test.
                  @returns true, if the artifact should be included.
                  @p Or, returns nil if no function has been specified.)
        ) 
  (context-filter [context] artifact-filter)
  (context-filter! [context t] (assoc context :artifact-filter t))
  
  (context-extracted [context] extracted)
  (context-extracted! [context ext] (assoc context :extracted ext)) 
  
  Messaging
  (msg [context opt stuff]
    (if (or (nil? opt) (get verbiage opt))
      (println (str "cjd: " (apply str stuff)))))

  (msg [context opt who stuff]
    (if (or (nil? opt) (get verbiage opt))
      (println (str "cjd: " who " --- " (apply str stuff)))))
  
  (warn [context stuff] 
    (if (context-throw-on-warn context)
      (error context stuff)
      (binding [*out* *err*]
        (println (str "cjd: " (apply str stuff))))))
  
  (warn [context form stuff] 
    (if (context-throw-on-warn context)
      (error context form stuff)
      (binding [*out* *err*]
        (println (str "cjd: " (apply str stuff) 
                      ": " (if form (enquote (maxstr 40 (if (seq? form) (apply str form) form))))
                      (if-let [name (context-name context)] (str " in " (context-name context)))
                      " near " (context-location context))))))
  
  (error [context stuff] 
    (throw  (CJDException. (str (apply str stuff) ))))

  (error [context form stuff] 
    (throw  (CJDException. 
              (str (apply str stuff) 
                   ": " (if form (enquote (maxstr 40 (if (seq? form) (apply str form) form))))
                   (if-let [name (context-name context)] 
                     (str " in " (context-name context)))
                   " near " (context-location context)))))

  )



