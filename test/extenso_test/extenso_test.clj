
#_ (* This namespace has been afflicted with the unfortunate duty of 
      being a test victim.)
(ns extenso-test.extenso-test
  (:use
    [extensomatic.extensomatic]
    )
  )

#_ (* This is @(name).)
(defprotocol Zot
  (zot-a [this])
  (zot-b [this something])
  )

#_ (* This is @(name).)
(defprotocol Zap
  (zap-a [this])
  (zap-b [this something])
  )


#_ (* This is @(name).)
(defextenso Ext1 [] [aa bb cc] 
  (aa-of [this] aa)
  (bb-cc [this] (str bb (if (= bb cc) " is " " is not ") cc))
  )

#_ (* This is @(name).)
(defextenso Ext2 []  [qqq rrr]
  #_ (* Generates noise, but very quietly.
        @returns The sound of silence.)
  (qqq-of [this] qqq)
  #_ (* @name does something funny to whatever it touches. It's not intentional,
        but it is stochastically klutzy.
        @arg ss The sound of air escaping from your punctured dreams.
        @returns A string of something unpredictable.)
  (rrr-qqq [this sss] (str qqq rrr sss))
  Zot
  (zot-a [this] (.toUpperCase (str qqq)))
  (zot-b [this something] (str something qqq rrr))
  )

#_ (* This is @(name).)
(defextenso Ext3 [] []
  (wurble [this] "wurble"))

(defextenso Aaaaaaa [Ext1 Ext2] [daphne chloe avocado])

#_ (* This is extenso @(name). It directly incorporates  @(link Ext1),
      and @(link Ext2).
      @field alfa Short for "alfalfa", as in "alfalfa field".
      @field beta A kind of ray, sort of like a manta, but beta instead.)
(defextenso Bbb [(Ext1 aa bb cc) (Ext2 qqq rrr)] [alfa beta]
  #_ (* On occasion, does something.
        @arg that Not this.
        @arg something-else Neither that nor this.
        @returns Some of the above.)
  (piddle [this that something-else] [aa bb qqq rrr alfa something-else])
  Zap 
  #_ (* An implementation of @(link zap-a).)
  (zap-a [this] 42)
  #_ (* @name is an implementation of @(link zap-b), but unwilling to admit it.)
  (zap-b [this something] 43)
  )

#_ (* This is extenso @(name). It directly incorporates @(link Ext3), @(link Ext1),
      and @(link Ext2).)
(defextenso Ccc 
  [Ext3 (Ext1 aa (bb 3) cc) (Ext2 (qqq nil) (rrr "rrr"))] 
  [alfa (beta 19) (gamma 3) ])

#_(* This is extenso @(name). It directly incorporates @(link Ccc).
     @(field ttt A haven for hexapods. 
             @p Actually, we really want to see if @(field ttt) works.))
(defextenso Ddd [Ccc] [ttt] 
  #_ (* @name is a friend of @(l Ddd).
        @arg this Not @(i that!)
        @returns Something to do with @(field ttt). Or is it @(arg ttt)?
        )
  (ttt-of [this] ttt))



