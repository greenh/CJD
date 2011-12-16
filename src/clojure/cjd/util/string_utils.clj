#_ ( Copyright (c) 2011 Howard Green. All rights reserved.
                
     The use and distribution terms for this software are covered by the
     Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
     which can be found in the file epl-v10.html at the root of this distribution.
     By using this software in any fashion, you are agreeing to be bound by
     the terms of this license.
     
     You must not remove this notice, or any other, from this software.
     )
#_ (* Provides a set of occasionally useful string-related function.
      )
(ns cjd.util.string-utils)

#_ (* Constructs a string (by applying @(link clojure.core/str str)) 
      from its arguments, and then wraps the string with quotes.
      @arg stuff Values to be added to the string.
      @returns The composite string surrounded by quotes.)
(defn enquote [& stuff] (str "\"" (apply str stuff) "\""))

#_ (* Accepts a sequence of expressions, converts them to strings if necessary,
      and concatenates them (using @(link clojure.core/str str)). If the
      resulting string is longer than @(arg max-len), @(name) truncates the string
      at @(arg max-len) characters, and appends " ..." at the end. 
      @arg max-len The maximum nember of characters in the string.
      @arg things Expressions that are converted to strings as needed and concatenated.
      @returns The (possibly truncated) concatenated string.)
(defn maxstr [max-len & things]
  (let [s (apply str things)
        len (count s)]
    (if (> len max-len)
      (str (.substring s 0 max-len) " ...")
      s)))

#_ (* Invokes the @(c addAll()) method on a mutable @(c java.util.Collection) 
      destination to add all of the elements of a source collection---and 
      then returns the destination collection.)
(defn add-all [dest-coll source-coll]
  (.addAll dest-coll source-coll)
  dest-coll)

(defn re-split [re string] (.split string re))

#_ (* Splits a string into a sequence of sub-strings based matches
      reported by a regular expression. 
      @p The RE must have a particular form,
      viz. it must have a capture group that _ends_ at the point where the 
      split will be made. The capture group can capture any subset of the
      string up to the final character, as the entire substring following
      the prior split point (or the beginning of the string) will be included
      in output substring. 
      @p Note that unlike re-seq, @name does not discard any of the input
      string's characters.
      @arg re The regular expression to use for splitting.
      @arg string The string to split.
      @returns A vector of split strings.
     )
(defn re-split-at [re string]
  (let [m (re-matcher re string)
        l (.length string)]
    (loop [next-start-index 0
           svec []]
      (if (and (< next-start-index l) (.find m))
        (let [end-index (.end m 1)]
          (recur 
            end-index
            (conj svec (.substring string next-start-index end-index) )))
        (if (< next-start-index l)
          (conj svec (.substring string next-start-index l))
          svec)))))

(defn re-replace-all [ re ^String reps ^String target]
  (.replaceAll (re-matcher re target) reps))

(defn re-replace-first [re ^String reps ^String target]
  (.replaceFirst (re-matcher re target) reps))

(defn re-starts-with [re ^String string]
  (.lookingAt re string))

(defn re-repeat [re rep string]
  (loop [victim string]
    (if (re-find re victim)
      (recur (re-replace-first re rep victim))
      victim)))

(defn- match-seq [matcher]
  (lazy-seq
    (when-let [match (re-find matcher)] 
      (cons (drop 1 match) (match-seq matcher)))))

(defn re-region-seq [content start-re end-re extract-re]
    (let [begin (re-matcher start-re content)]
      (if (.find begin)
        (let [end (re-matcher end-re content)]
          (if (.find end(.start begin))
            (let [mm (re-matcher extract-re content),
                  region (.region mm (.start begin) (.start end))]
              (match-seq region))
            nil))
        nil)))
