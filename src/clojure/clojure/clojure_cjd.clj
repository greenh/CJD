#_ (* Some supporting stuff for use when running CJD on the Clojure source.
      @p This is more for demonstration purposes than to actually be useful.)

(ns clojure.clojure-cjd
  (:use 
    [cjd custom artifact-base link-resolver]
    )
  )

#_ (* Editor for Clojure source docstrings. 
      @p All this does is to strip off the first two blanks at the start of 
      any line that has them (a harmless Clojure docstring convention), 
      the better to get lines to align.
      @arg docstr The docstring to edit.
      @returns The edited docstring.
      )
(defn docstring-editor [docstr] 
  (.replaceAll docstr "\\n  (.*)" "\n$1"))

(use-docstring-editor docstring-editor)

#_ (* Source code location resolver for Clojure source.
      @p Yes, we know\: It probably won't work because we're aiming at the 
      master branch of the source, which almost certaintly 
      doesn't correspond with the source we're generating the documentation
      from... but until we get smarter, it's better that naught.
      @arg artifact The artifact to source-resolve
      @returns The URI for the artifact's source.
       )
(defn source-resolver [artifact]
    (let [line (defined-at artifact)
          [_ srcpath] (re-matches #".*(src/clj/clojure/.*\.clj)" (defined-in artifact))] 
      (if (and srcpath line) 
        (str "https://github.com/clojure/clojure/blob/master/" srcpath "#L" line))))

(add-source-resolver source-resolver)