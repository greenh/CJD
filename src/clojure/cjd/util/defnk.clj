#_ (* Alternative @(c defnk) definition discovered 
      @(lt https://groups.google.com/forum/#!topic/clojure/tln8Jx4TOJc here).
      @p It works...)
(ns cjd.util.defnk)

(defn expand-keyword-call [name pos kw-defaults args] 
  (if (< (count args) (count pos)) 
    (throw (Exception. (str "Too few arguments to " name)))) 
  `(~(symbol (str name "-**")) 
     ~@(take (count pos) args) 
     ~@(loop [;; current keyword args left to process 
              kwargs (drop (count pos) args) 
              ;; current keyword values 
              kwvals (apply hash-map kw-defaults)] 
         (cond (= (count kwargs) 0) 
               (map #(kwvals %) (take-nth 2 kw-defaults)) 
               
               (not (contains? kwvals (first kwargs))) 
               (throw (Exception. (str "Unknown keyword argument to " 
                                       name ": " (first kwargs)))) 
               
               (= (count kwargs) 1) 
               (throw (Exception. (str "No value for keyword argument" 
                                       (first kwargs) " in call to " name))) 
               
               true 
               (recur (drop 2 kwargs) 
                      (assoc kwvals (first kwargs) (second kwargs))))))) 

#_ (* Defines a function with keyword-based parameter identification.
      @p This should be completely compatible with the standard @(c defnk) definition,
      wherever it ended up. )
(defmacro defnk [name args & body] 
  (let [ [pos kwvals] (split-with symbol? args)] 
    `(do 
       (defn ~(symbol (str name "-**")) 
         ~(vec (concat pos (map #(symbol (.getName %)) (take-nth 2 kwvals)))) 
         ~@body) 
       (defmacro 
         ~name [& args#] 
         (expand-keyword-call '~name '~pos '~kwvals args#)) 
       (.setMeta #'~name (assoc (meta #'~name) :arglists '(~args) ))))) 