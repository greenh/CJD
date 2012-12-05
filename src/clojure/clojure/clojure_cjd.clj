(ns clojure.clojure-cjd
  (:use 
    [cjd custom]
    )
  )

(use-docstring-editor 
  (fn [docstr] (.replaceAll docstr "\\n  (.*)" "\n$1")))