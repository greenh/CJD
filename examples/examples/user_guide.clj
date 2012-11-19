
#_ (* This is a collection of examples of CJD-based documentation. Most of the 
      contents here have starring roles in the CJD user guide.
      @p The examples here are purely for explanatory purposes, and attempts
      to actually run them may prove hazardous to your health.
      )
(ns examples.user-guide)


#_ (* Returns the number of items in a collection.
      
      @p @name works regardless of whether @(arg coll) is a set,
      vector, list, map, or string.
      
      @arg coll The collection to count.
      @returns The number of elements in @(arg coll). Returns 0 if @(arg coll) is nil. 
      @since Clojure ~"1.0", or maybe even before.
      )
(defn count [coll])

#_ (* Generates and returns a single value by applying a function 
      to the elements of a collection.
      
      @p @name internally maintains a value that serves as an "accumulator", 
      which is initialized to @(arg val). Then, @name calls the function 
      @(arg f) once for each element of @(arg coll), passing to it the 
      current accumulator value and the next element of @(arg coll). 
      @name then updates the accumulator with the value returned by @(arg f). 
      When @(arg f) has been called with all elements  
      in @(arg coll), @name returns the final value of the accumulator.
      
      @(arg f A function of the form @(fun [acc element]), where
            @(arg acc The accumulator value, which is @(arg val) on the first call 
                  to @(arg f), or the result of the prior call to @(arg f) 
                  on subsequent calls.)
            @(arg element The next unprocessed element of @(arg coll).)
            @returns The updated accumulator value. )
      @arg val An initial value for the accumulator.
      @arg coll A collection of elements.
      @returns The value returned by the final call to @(arg f). If @(arg coll)
      is empty, returns @(arg val). 
      )      
     
(defn reduce [f val coll])


#_ (* Generates and returns a single value by applying a function 
      to the elements of a collection.
      
      @p @name internally maintains a value that serves as an "accumulator". The 
      accumulator's initial value is the first element of @(arg coll) in the two-argument
      form of reduce, or @(arg val) in the three-element form. Then, @name
      calls the function @(arg f) once for each subsequent element of @(arg coll),
      passing to it the current accumulator value and the next element of @(arg coll), 
      and each time updating the accumulator with the value @(arg f) returns. When @(arg f)
      has been called with all elements in @(arg coll), @name returns the final value
      of the accumulator.
      
      @p In the two-argument form, if @(arg coll) is empty, @name calls @(arg f) with no 
      arguments, and @(arg f) must therefore have a zero-argument form. If @(arg coll) 
      has exactly one element, @name returns it without calling @(arg f). Similarly,
      in the three-argument form, if @(arg coll) is empty, @name returns @(arg val) 
      without calling @(arg f).
      
      @(arg f A function of the form @(fun [acc element]), where
            @(arg acc The "accumulator" value, which is either @(arg val) or 
                  the first element of @(arg coll) on the first call to @(arg f) as 
                  described above, or 
                  the result of the prior call to @(arg f) on subsequent calls.)
            @(arg element The next unprocessed element of @(arg coll).)
            @returns The next "accumulator" value. )
      @arg val An initial value for the "accumulator".
      @arg coll A collection of elements.
      @returns The value of the "accumulator" after the final call to @(arg f) . 
)
(defn reduce1 
  ([f col] )
  ([f val col]))

#_ (* Generates and returns a single value by applying a function 
      to the elements of a collection.
      
      @p @name internally maintains a value that serves as an "accumulator". The 
      accumulator's initial value is the first element of @(arg coll) in the two-argument
      form of reduce, or @(arg val) in the three-element form. Then, @name
      calls the function @(arg f) once for each subsequent element of @(arg coll),
      passing to it the current accumulator value and the next element of @(arg coll), 
      and each time updating the accumulator with the value @(arg f) returns. When @(arg f)
      has been called with all elements in @(arg coll), @name returns the final value
      of the accumulator.
      
      @p In the two-argument form\: 
      @(ul
        @(li if @(arg coll) is empty, @name calls @(arg f) with no 
             arguments, and @(arg f) must therefore have a zero-argument form.) 
        @( li If @(arg coll) has exactly one element, @name returns it without 
              calling @(arg f).))
      
      @p In the three-argument form\: 
      @(ul 
         @(li if @(arg coll) is empty, @name returns @(arg val) without calling @(arg f).))
      
      @(arg f A function of the form @(fun [acc element]), where
            @(arg acc The "accumulator" value, which is either @(arg val) or 
                  the first element of @(arg coll) on the first call to @(arg f) as 
                  described above, or 
                  the result of the prior call to @(arg f) on subsequent calls.)
            @(arg gloopy-doop Something bogus.) 
            @(arg element The next unprocessed element of @(arg coll).)
            @returns The next "accumulator" value. )
      @arg val An initial value for the "accumulator".
      @arg coll A collection of elements.
      @returns The value of the "accumulator" after the final call to @(arg f) . 
)
(defn reduce2 
  ([f col] )
  ([f val col]))

#_ (* Generates and returns a single value by applying a function 
      to the elements of a collection.
      
      @p @name internally maintains a value that serves as an "accumulator". The 
      accumulator's initial value is the first element of @(arg coll) in the two-argument
      form of reduce, or @(arg val) in the three-element form. Then, @name
      calls the function @(arg f) once for each subsequent element of @(arg coll),
      passing to it the current accumulator value and the next element of @(arg coll), 
      and each time updating the accumulator with the value @(arg f) returns. When @(arg f)
      has been called with all elements in @(arg coll), @name returns the final value
      of the accumulator.
      
      @p In the two-argument form\: 
      @(ul
        @(li if @(arg coll) is empty, @name calls @(arg f) with no 
             arguments, and @(arg f) must therefore have a zero-argument form.) 
        @( li If @(arg coll) has exactly one element, @name returns it without 
              calling @(arg f).))
      
      @p In the three-argument form\: 
      @(ul 
         @(li if @(arg coll) is empty, @name returns @(arg val) without calling @(arg f).))
      @p Examples
      @(example  
         "(reduce + [1 2 3 4 5])" "15"
         "(reduce + [])" "0"
         "(reduce + 1 [])" "1"
         "(reduce + 1 [2 3])" "6"
         "(reduce conj [1 2 3] [4 5 6]" "[1 2 3 4 5 6]"
"(reduce
  (fn [primes number]
      (if (some zero? (map (partial mod number) primes))
          primes
          conj primes number)))
       [2]
       (take 100 (iterate inc 3)))"
        "[2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97]"
       ) 
      
      @(arg f A function of the form @(fun [acc element]), where
            @(arg acc The "accumulator" value, which is either @(arg val) or 
                  the first element of @(arg coll) on the first call to @(arg f) as 
                  described above, or 
                  the result of the prior call to @(arg f) on subsequent calls.)
            @(arg element The next unprocessed element of @(arg coll).)
            @returns The next "accumulator" value. )
      @arg val An initial value for the "accumulator".
      @arg coll A collection of elements.
      @returns The value of the "accumulator" after the final call to @(arg f) . 
      
      
      
      )
(defn reduce3
  ([f col] )
  ([f val col]))

#_(* Computes the min and max values of a collection.
	 
	 @arg coll A collection of numbers.
	 @(returns A tuple of the form @(form [minval maxval]), where\:
		@arg minval The minimum value of @(arg coll).
		@arg maxval The maximim value of @(arg coll).)
	)
(defn minmax [coll] [(apply min coll) (apply max coll)])

#_ (* Returns the average of the values in a min / max tuple.
      
      @(arg min-max A tuple of the form @(form [minval maxval]), where
            @arg minval A number representing a minimum value.
            @arg maxval A number representing a maximim value.)
      @returns The average of @(arg minval) and @(arg maxval).
      )
(defn average [min-max] 
  (let [[minval maxval] min-max]
    (/ (+ minval maxval) 2)))


#_ (* Generates a curve through a set of points.
      
      @(arg curvespec A structure of the form @(form [points weight color]), where
            @(arg points A sequence of tuples of the form @(form [x y z]), where
                  @arg x The x-coordinate.
                  @arg y The y-coordinate.
                  @arg z The z-coordinate.)
            @arg weight The weight of the curve.
            @(arg color Describes the color to use, which can be any of 
                  the following\:
                  @(ul
                     @li  A string containing the name of the color for the curve.
                     @(li A tuple of the form @(form [r g b]), where
                          @arg r The red value.
                          @arg g THe green value.
                          @arg b The blue value.)
                     @(li A tuple of the form @(form [h s l]), with
                          @arg h The hue value.
                          @arg s The saturation value.
                          @arg l The luminance value.))))
      @(returns A sequence of projected points of the form @(form [x y]), where
                @arg x The projected x-coordinate.
                @arg y The projected y-coordinate.)
      )
(defn generate-curve [curvespec] )

#_ (* Examples of documentation for structured data.
      
      @(p We can talk about a function of the form @(fun [e q]) where
          @arg e Describes some pecularity.
          @arg q Is something else.
          @returns Nothing of import.
          @p and then nothing happens.) 
      
      @(arg p1 The first parameter, which should have
            the form @(form [[a b] c & ds]), where
            @arg a The first element of the first element of @(arg p1).
            @arg b The second element of the first element of @(arg p1).
            @arg c The second element of @(arg p1).
            @arg ds Any remaining elements of @(arg p1).)
      @(arg p2 Another parameter, which should be a map. Options of 
            interest include\:
            @opt :xx The value for @(opt :xx).
            @option :yy "abc" The value for @(opt :yy), which has a 
            default value of "abc" .)
      @(arg test-fn A function of the form @(fun [xx]), where
            @(arg xx A tuple of the form @(form [t1 t2]), where\:
                  @arg t1 The first element.
                  @arg t2 The second element.)
            @returns True, if @(arg xx) is suitably wonderful.)
      @(returns If the approved by @(arg test-fn), a collection tuples of 
                the form @(form [aa bb]), where\:
            @arg aa The first element of the tuple.
            @arg bb The second element of the tuple.
            @p Otherwise, returns nil.)
      )
(defn structured-fn [p1 p2 test-fn]
  (let [[[a b] c] p1
        {x :xx y :yy} p2]
    (if (test-fn c c) [[a b] [x y]]))) 

#_ (* Illustration of documenting destructuring
      
      @(arg p1 The first argument.
            @(arg p1-1 The first element element of @(arg p1).
                  @arg a The first element of @(arg p1-1).
                  @arg b The second element of @(arg p1-1).)
            @arg c The second element of @(arg p1).
            @arg ds Any remaining elements of @(arg p1),
            )
      @arg e First element of the unnamed second parameter.
      @arg f Second element of the unnamed second parameter.
      @(arg p2 The map of options or whatever.
            @nopt :xx x Value of the @(key :xx) key in @(arg p2). 
            @nopt :yy y Value of the @(key :yy) key in @(arg p2). 
            )
      @(returns A collection tuple of the form @(form [aa bb cc]), where\:
            @arg aa The first element of the tuple.
            @arg bb The second element of the tuple.
            @arg cc The third element of the tuple.)
      )
(defn destructed-fn [[[a b :as p1-1] c & ds :as p1] [e f] {x :xx y :yy :as p2}]
  [[a b] [[x y]]]) 

#_ (* Illustrates really really recursive data documentation.
      
      @(arg a A sequence of the form @(form [b bb]), where
        @(arg b A tuple of the form @(form [c cc]), where
          @(arg c A collection of form @(form [d dd]), where
            @(arg d A collection of form @(form [e ee]), where
              @(arg e A collection of form @(form [f ff]), where
                @(arg f A collection of form @(form [g gg]), where
                  @(arg g A collection of form @(form [h hh]), where    
                        @arg h Some old value.
                        @arg hh A value somewhat more or less than @(arg dd).)
                  @arg gg A random value.)
                @arg ff @(i e) :nb @(sup –  @(i i) :nb π) .)
              @arg ee A random value.)
            @arg dd Another random value.)
          @arg cc An integer, which must be set to -1\.)
        @arg bb Any old value.)
      @arg aa Occasionally equal to π.
      @returns 0\, unless the moon is full, in which case it returns the sum of 
      @(arg aa), @(arg bb), @(arg cc), @(arg dd), @(arg ee),
      @(arg ff), @(arg gg), and @(arg hh).
      )
(defn recursive-nonsense [a aa]) 

#_ (* Replaces words in source files with alternative words.
      
      @p @name processes each file given by the @(opt :source) option of 
      @(arg opts). For each word in a file, @name checks to see if 
      there's a substitute word in @(arg from-to). If it finds a substitute 
      word, it replaces that word with its substitute. As it completes 
      each file, @name writes the substituted file
      to the location specified by @(opt :dest) option of @(arg opts).
      
      @(arg from-to A map where each key is a string representing a word 
            to look for, and its corresponding value is a string containing 
            the substitute word. )
      @(arg opts A map containing options, which may include any combination 
            of\:
            @opt :source A collection of strings containing path names of
                  source directories or files.
            @opt :dest A string containing the pathname of the destination.
            @(opt :v Sets the disposition of different kinds output. Value 
                  is a map mapping output types to levels.
                  @p Output levels are defined as follows\:
	                  @opt :off Turns output off.
	                  @opt :on Sends output to stdout.
	                  @opt :err Sends output to stderr.
                  @p Output types are any of\:
	                  @option :error @(opt :err) Error messages.
	                  @option :details :off Detail messages.
	                  @option :debug :off Debug messages.
	                  @option :misc :on Miscellaneous stuff.
                  )
            )
      @returns An integer representing the number of substitutions made.
      )
(defn substitute-stuff [from-to opts] ) 

#_ (* Protocol describing a @(i misfiles), a class of 
      objects for doing stochastically successful I / O.)
(defprotocol MisFile
  #_ (* Opens the misfile.
        @returns Zero, if the misfile might have been opened.)
  (misopen [this])
  #_ (* Misreads a @(l MisFile).
        @arg dest Destination buffer for misread data.
        @arg byte The number of bytes to misread.
        @arg start 
        @returns The number of bytes actually misread.)
  (misread [this dest bytes] [this start dest bytes])
  #_ (* Miswrites a @(l MisFile).
        @arg dest Source buffer for data to miswrite.
        @arg byte The number of bytes to miswrite.
        @arg start When specified, specifies the location to miswrite to. 
        @returns The number of bytes actually miswritten.)
  (miswrite [this source bytes] [this start dest bytes])
  #_ (* Stochastically closes the @(l MisFile), and possibly misplaces it, too.)
  (misclose [this])
  ) 

#_ (* Misimplementation of the @(l MisFile) protocol. 
      @field file The underlying @(il java.io.File) object.
      @field path A string, the path name of the @(l MisFiler) object.
      )
(defrecord MisFiler [path file]
  MisFile
  (misread [this dest bytes])
  (misread [this start dest bytes])
  #_ (* Miswrites a @(l MisFile).
        @arg dest Source buffer for data to miswrite.
        @arg byte The number of bytes to miswrite.
        @arg start When specified, specifies the location to miswrite to. 
        @returns The number of bytes actually miswritten.)
  (miswrite [this source bytes])
  (miswrite [this start dest bytes])
  #_ (* Stochastically closes the @(l MisFile), and possibly misplaces it, too.)
  (misclose [this])
)
(declare abc-xyz)

#_(* This is documentation for function @name . It lazily
	 relies on @(link abc-xyz) to do its work for it.
  )
(defn xyz-abc [] (abc-xyz))

#_(* Does something remarkable with @(l xyz-abc).
	 )
(defn abc-xyz [] (xyz-abc))
































