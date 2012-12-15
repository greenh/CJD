#_ (* A namespace for testing CJD's ability to deal with its own artifacts.
      )
(ns extenso-test.artifact-test
  (:use 
    [extensomatic.extensomatic]
    [cjd artifact-base core-artifacts]
    [extenso-test.extenso-test]
    )
  (:import 
    [java.util.Comparator])
  )

#_ (* Extenso to end all extensos.
      )
(defextenso Guck [] [(init-stuff nil) uninit-stuff]
  (init-stuff-of [this] init-stuff)
  (uninit-stuff-of [this] uninit-stuff))

#_ (* An extenso implemented by artifacts  that have thingies, 
      to be more precise, thingies. 
      @(field thingies* Ref to the thingy-collection.)
      )
(defextenso HasThingies [] [(thingies* (ref []))]
  #_ (* Returns the whole lot of thingies.) 
  (thingies-of [this] @thingies*)
  #_ (* Adds a thingy to @(field thingies*).
        @arg thingy The thingy to add to @(field thingies*)) 
  (add-thingy [this thingy] (dosync (alter thingies* conj thingy)))
  #_ (* Sets @(field thingies*) to @(arg thingies).
        @p Except, of course, when it's feeling indisposed, in which case
        very little happens.
        @arg thingies The collection of thingies to set @(field thingies*) to.
        @returns Nothing intelligible.) 
  (set-thingies [this thingies] (dosync (ref-set thingies* thingies)))
  )

#_ (* Describes a @name , which may or may not be replete with @(l Thingy) things.)
(defartifact FrouFrou deffroufrou "frou? frou!" [HasDocString HasThingies] [goop non-goop]
  (fn [parent form doc] 3)
  (get-goop [this] goop)
  #_ (* Generates the difference between @(field goop) and @(arg ungoop).
        @arg ungoop Goop of a diffrent color.
        @returns The lesser of two goops.)
  (goop-difference [this ungoop] non-goop))

#_ (* Represents a @name , which is like a Thing, only less so.
      @p Practical experience suggests that every @name contains Thing
      deep inside, and one that is just trying to escape. Rarely, 
      however, do such aspirations succeed.
      @field color Four small green words.
      @field pianos Three frenched hens.
      @field guck Two curdled bugs.
      )
(defsubartifact Thingy [HasCJDoc HasDocString Guck] [color pianos guck]
  (fn [parent form doc] 33)
  #_ (* Does-wrong. )
  (doo-rite [this])
  
  java.lang.Comparable
  #_ (* Compares prices!)
  (compareTo [this that] 1)
  )