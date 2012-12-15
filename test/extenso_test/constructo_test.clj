#_ (* A namespace filled with various varying variations with which to vex CJD's
      extenso-constructo artifact support.)
(ns extenso-test.constructo-test
  (:use
    [extensomatic.extensomatic]
    )
  )

#_ (* A protocol for no seasons.)
(defprotocol Zotz
  (zotz-a [this])
  (zotz-b [this something])
  )
#_ (* A protocol for the sake of having a protocol.)
(defprotocol Zapz
  (zapz-a [this])
  (zapz-b [this something])
  )


(#_ (* This is extenso @name .)
defextenso Extxx1 [] [aa bb cc] 
  (aa-of [this] aa)
  (bb-cc [this] (str bb (if (= bb cc) " is " " is not ") cc))
  )

#_ (* This is extenso @name .)
(defextenso Extxx2 []  [qqq rrr]
  #_ (* Returns the @(arg qqq).
        @returns The @(arg qqq), in case you didn't get it the first time.)
  (qqq-of [this] qqq)
  #_ (* Generates the @(arg rrr) of the @(arg qqq).)
  (rrr-qqq [this ss] (str qqq rrr ss))
  Zotz
  #_ (* Does something.)
  (zotz-a [this] (.toUpperCase (str qqq)))
  #_ (* Does something else\; no one is quite sure what.)
  (zotz-b [this something] (str something qqq rrr))
  )

#_ (* This is extenso @name .)
(defextenso Dated [] []
  (date [this] (.format (java.text.SimpleDateFormat.) (java.util.Date.))))

#_ (* This is constructo @name , in the land of the home, and the... whatever.
      @field poo Don't ask.
      )
(defconstructo PooPoo [Extxx1 Dated] [poo] 
  #_ (* Method @name .)
  (poo-of [this] poo)
  #_ (* Method @name .)
  (poo-vec [this] [aa bb cc poo])
  java.lang.Object
  #_ (* Method @name .)
 (toString [this] (str "#<PooPoo " poo " with " aa ">"))
  )

#_ (* @name is a contructo of mass destruction. And it doesn't have any fields.
      )
(defconstructo PooPoo2 [Extxx1 Dated] [] 
  java.lang.Object
  (toString [this] (str "#<PooPoo2 " (date this) " with " aa ">"))
  )
#_ (* A constructo for testing stuff.
      @field eeeg An initialized field.
      @field poo3 An uninitialized field.
      @field yyyo A field initialized so as to just say "No.".)
(defconstructo PooPoo3 
  [(Extxx2 (qqq 128.5) (rrr -11117)) Dated] 
  [(eeeg nil) poo3 (yyyo "No.")]
  #_ (* @(link PooPoo3)'s very own method.)
  (poo3-of [this] (.toUpperCase (str poo3 yyyo eeeg)))
  #_ (* Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod 
        tempor incididunt ut labore et dolore magna aliqua. 
        @p Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
        aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in 
        voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
        @p Excepteur sint occaecat cupidatat non proident, sunt in culpa qui 
        officia deserunt mollit anim id est laborum. 
        @(arg this Definitely not @(arg that).)
        @(arg that Definitely not @(arg this).) 
        @(arg others Neither @(arg this) nor @(arg that).)
        @(returns None of the above, but condider that\:
                  @p Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod 
                  tempor incididunt ut labore. ) 
        @since The dawn of time.
        @p Misc. annotation\: Ut enim ad minim veniam, quis nostrud exercitation 
        ullamco laboris, or maybe not. 
        )
  (trial-balloon [this that & others])
  Zapz 
  #_ (* An implementation of @name .)
  (zapz-a [this] 3)
  #_ (* An implementation of @name .)
  (zapz-b [this something] (+ 3 something))
  java.lang.Object
  (toString [this] (str "#<PooPoo2 " (date this) " with " poo3 ">"))
  )

#_ (* An extenso with a bunch of stuff.
      )
(defextenso Flexibot [(Extxx2 (qqq "qqq") rrr)] [qq (nada (ref [])) zz (u2u "No.") ]
  (nada-of [this] @nada)
  (nada-nada [this nothing] (dosync (alter nada conj nothing ))))

#_ (* A constructo that makes use of @(link Flexibot), although there's no
      particular reason why.)
(defconstructo Doodle [Extxx1 Flexibot] [(yorick (ref 0))]
  (alas [this] (str @yorick " " @nada)))