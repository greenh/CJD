#_ (* Comprehensive test namespace.
      @p Here's where we try out at least one of everything, so that we can
      conclude that at least one of everything actually works.
      )
(ns cjdtest.comprehensive)

#_ (* Test of margin-setting capablities.
      @(p This is a second paragraph, and it should be more or less in line
      with the prior paragraph, and more or less in line with the following 
      list, as well.
      @(ul 
         @li This is the first element of a @(c ~"@ul") list.
         @(li This is a list within the list.
              @(ul
                 @li An inner item.
                 @li And another!
                 @li And another! There are millions of them!!
                 ))
         @(li And this is the last item.
              @(ul @li ...which also has an inner item.))
         ))
      @p And here's another paragraph, followed by an ~"@ol" list.
      @(ol 
         @li This is the first element of an ~"@ul" list.
         @(li This is a list within the list.
              @(ol
                 @li An inner item.
                 @li And another!
                 @li And another! There are millions of them!!
                 ))
         @(li And this is the last item.
              @(ul @li ...which also has an inner item.))
         )

      @arg doodle This is a simple argument with the simple name @(arg doodle) .
      @(arg warble Here we have an argument with a list.
            @(ul
               @li this is the first point we want to make about @name .
               @li But, more can be said, too.
               @li Or less.)
            )
      @p And an intermezzo paragraph.
      @(arg chirp A tuple, we say with barely concealed astonishment, that by some
            miracle happens to have the form @(form [xx-yy yy-xx]), where\:
            @(arg xx-yy 
                  
                  This is an introductory paragraph for the introduction of the possibly
                  (but not likely) learned discussion of @(arg xx-yy).
                  @(ul
                     @(li Some points we want to make about @(arg xx-yy) might want to be 
                          in the form of a list, albeit one of not terribly large 
                          length\:
                          @(ol
                             @li An inner item. This item runs on unmercifully for far
                             longer than anyone would want to contemplate. Doth we dare 
                             contemplate the templates? Or nay!!!
                             @li And another!
                             @li And another! There are a few of them...
                             )
                          )
                     @(li And another, too.)
                     @li Or not.
                     )  
                  )
            @arg yy-xx Like @(arg xx-yy), but different.
            )
      @(returns Some old value, nothing important.
                @(ul
                     @(li Some more points we want to make about @name\:
                          @(ul
                             @li An inner item.
                             @li And another!
                             @li And another! There are a few of them...)
                          )
                     @(li And another, too.)
                     @li Or not.))
      @p And a parting paragraph.
      @since The dawn of time.
      )
(defn margins-test [doodle warble chirp])

#_ (* Here we test preformatted stuff.
      @(p @(pre "
(let [vopts (if attached attached param)
      remains** (if attached remaining+ remains*)]
  (if (empty? vopts)
    (throw (CJDException. \"Missing parameter for -v\"))
    (recur remains** 
       (assoc opts+ :v (set (map (fn [ch] (keyword (str ch))) vopts))))))
"))
      @p And here is an example of an example, exemplified, as it were, in exemplary
      form.
      @(example
"(cjd-gen \"test/cjdtest/comprehensive.clj\" \"doc/dark\"  :theme :dark
            :noindex true 
            :title \"CJD Test\"  
            :footer 'cjd.cjd.its-mine/gen-footer 
            )" 
"cjd: processing test\\cjdtest\\comprehensive.clj
cjd: gen-namespace --- #<Namespace cjdtest.comprehensive>
nil" 
"(cjd-gen \"test/cjdtest/comprehensive.clj\" \"doc/dark\"  :theme :dark
            :noindex true 
            :title \"CJD Test\"  
            :footer 'cjd.cjd.its-mine/gen-footer 
            )"
"cjd: processing test\\cjdtest\\comprehensive.clj
cjd: gen-namespace --- #<Namespace cjdtest.comprehensive>
#<CompilerException java.lang.IllegalArgumentException: Wrong number of args (2) passed to: generate$gen-flow (NO_SOURCE_FILE:0)>" 
"(cjd-gen \"test/cjdtest/comprehensive.clj\" \"doc/dark\"  :theme :dark
            :noindex true 
            :title \"CJD Test\"  
            :footer 'cjd.cjd.its-mine/gen-footer 
            )"
"cjd: processing test\\cjdtest\\comprehensive.clj
cjd: gen-namespace --- #<Namespace cjdtest.comprehensive>
#<CompilerException java.lang.IllegalArgumentException: Wrong number of args (2) passed to: generate$gen-flow (NO_SOURCE_FILE:0)>
nil"
"")

      )
(defn preform-test [])


(defn docstring-fn 
  "This is a certifiable docstring.

Isn't it pretty?
    Oops, I mean, hideous?
        Especially since we're formatting it in an especially
        unpleasant way.
    But, that's the nature of things.
Even if they're somewhat unnatural.

But that's life...
         ........... or lack thereof.
"
  [x y z])

(defn arg-test [aaa [xxx yyy] 
                { kkk :k lll :l :as eee :or { :k 3 }} 
                { :keys [ppp qqq rrr] :as fff }
                { :strs [ttt uuu vvv]}
                { :syms [gg hh ii] }])


#_ (* Generates and returns a single value by applying a function 
      to the elements of a collection.
      
      @name internally maintains an value that functions as an "accumulator". The 
      accumulator is initialized to the first element of @(arg coll) in the two-argument
      form of reduce, or to @(arg val) in the three-element form. Then, @name
      calls the function @(arg f) once for each subsequent element of @(arg coll),
      passing to it the current accumulator value and the next element of @(arg coll), 
      and each time updating the accumulator with the value @(arg f) returns. When @(arg f)
      has been called with all elements in @(arg coll), @name returns the final value
      of the accumulator.
      
      There are two forms of @name. The first, two-argument form, initially calls 
      @(arg f) with the first two elements of @(coll). For the second and any
      subsequent elments of @(arg coll) @name calls @(arg f) with the value 
      returned by the previous call to @(arg f) and the next element in @(arg coll).
      @name then returns the value returned by the call to @(arg f) with the final
      element of @(arg coll). 
      
      If @(arg coll) is empty, @(arg f) is called with no arguments, and must therefore 
      have a zero-argument form. If @(arg coll) has exactly one element, @name returns it 
      without calling @(arg f).
      
      In the second, three-argument form, @name initially calls @(arg f) with
      @(arg val) and the first element of @(arg coll). For each successive element
      in @(arg coll), @name calls @(arg f) with the value returned by the previous call
      to @(arg f) and the next element of @(arg coll). @name returns the value returned
      by the final call to @(arg f).
      
      In this form, if @(arg coll) is empty, @name returns @(arg val) without calling 
      @(arg f).
      
      @(arg f A function of the form @(fun [acc element]), where
            @(arg acc The "accumulator" value, which is either @(arg val) or 
                  the first element of @(arg coll) on the first call to @(arg f), or 
                  the result of the prior call to @(arg f) on subsequent calls.)
            @(arg element The next unprocessed element of @(arg coll).)
            @returns The next "accumulator" value. )
      @arg val An initial value for the "accumulator".
      @arg coll A collection of elements.
      @returns The value of the "accumulator" after the final call to @(arg f). 
)
(defn irreduce 
  ([f col] )
  ([f val col]))



