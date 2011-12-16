(ns cjd.util.defnk)

; from https://groups.google.com/forum/#!topic/clojure/tln8Jx4TOJc

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