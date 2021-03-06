# CJD

CJD is a technology for documenting Clojure programs through the use of structured 
comments embedded in Clojure source code. As such, it's a supplement to or replacement
for the docstring mechanism. 

CJD was inspired by Javadoc, to which it bears a superficial similarity. 
Like Javadoc,
CJD-comments support a simple form of markup that not only adds formatting detail
but provides a level of metadata that describes what's being documented. 
This allows CJD's processing facility
to extract the documentation content from collections of Clojure namespaces and convert 
it into trees of consistently-formatted HTML documents. 

A few salient CJD features: 

* It structures comments and markup in terms of well-defined Clojure forms, allowing 
structure-sensitive editors to be used to good advantage.

* It incorporates a recursively-defined documentation structure that allows 
Clojure's recursively-defined data structures and functions 
to be documented to whatever depth is needful.

* It provides support for documenting most core Clojure artifacts (vars, functions,
macros, protocols, records, etc.), and provides facilities for extending CJD to
allow user-defined artifacts to be compatibly documented.  

* It doesn't insist on completeness --- what does and doesn't get documented,
and to what extent, is entirely at the developer's discretion.

So, what does a CJD comment look like? Here's a taste, in the form a 
randomly selected function from the CJD source:

	#_ (* Processes a sequence of CJD comment-content forms into a sequence 
	      of flow-forms, which will in turn contain phrasing and text content.
	      
	      @arg context The context map.
	      @arg contents A sequence of input forms to be parsed.
	      @(returns A tuple of the form @(form [flows remaining]), where\: 
	        @arg flows A sequence of flow nodes describing the parsed content
	        @arg remaining Any unparsed forms in the input sequence.)
	      )
	(defn flow-run [context contents] . . . )

And if you'd like to see the corresponding generated output, you have your choice of
[dark background](http://greenh.github.com/CJD/doc/dark/cjd.parser.html#flow-run) and
[light background](http://greenh.github.com/CJD/doc/light/cjd.parser.html#flow-run)
renderings.

The CJD source is itself extensively commented using CJD, and serves as a pretty 
good example of CJD-based documentation can look like. You can view the full CJD 
documentation tree 
from the top in both [dark background](http://greenh.github.com/CJD/doc/dark/index.html) 
and [light background](http://greenh.github.com/CJD/doc/light/index.html) forms.

For a description of how to obtain, use, and customize CJD, see the
[user guide](http://greenh.github.com/CJD/doc/User.html). Or, for a 
not-terribly-reverent introduction to CJD and how it got there, 
check out [the FAQ](http://greenh.github.com/CJD/doc/FAQ.html).

## Documentation
Several documents are in the making. These are mostly complete but nonetheless
works in progress, so proceed with caution! 

* A [user guide](http://greenh.github.com/CJD/doc/User.html), which attempts to explain 
how to obtain, use, customize, and extend CJD. (This is mostly complete, to a first 
approximation, except for the section on extensions.)

* A [quick reference](http://greenh.github.com/CJD/doc/QuickRef.html) to CJD markup
elements. 

* A [FAQ](http://greenh.github.com/CJD/doc/FAQ.html), which discusses 
what CJD is and does, and how it managed to get there.

* CJD's program documentation, as generated by CJD, in your choice of 
[dark background](http://greenh.github.com/CJD/doc/dark/index.html) or 
[light background](http://greenh.github.com/CJD/doc/light/index.html).

## Status

It works! 

Well, after a fashion --- there are a few caveats. Some known problems, limitations, and gotchas:

* It's currently just for Clojure running on a JVM -- no Clojure/CLR or 
ClojureScript support at the moment.

* The current implementation's operation involves loading Clojure files to assist 
its name resolution processes. Works like a charm, but there's a down side: 

    + Namespaces being processed must be (transitively) loadable...
 
    + ...so CJD's classpath needs to include everything that your project depends on...
 
    + ...which introduces the possibility of conflicts with CJD's dependencies. 
[Hiccup](https://github.com/weavejester/hiccup) represents the principle 
potential incompatibility; CJD uses Hiccup 1.x, which isn't 
interface-compatible with Hiccup 0.x. 

<!-- xxx -->

* It's currently working with Clojure 1.4.0 and 1.3.0 (but not with 1.2.1). 

    + CJD does contain release-specific code, so the choice of Clojure version for running CJD 
is not entirely arbitrary.

    + The current release is (mostly) not compiled code, so loading the code is no ball of 
fire.

<!-- xxx -->

* CJD incorporates a Leiningen plugin that adaptively does the right stuff for 
both versions 1 and 2 of Leiningen. Both versions currently seem to work pretty well!

* Error reporting is less than fully wonderful:

    + Syntax error messages tend to get you to the offending comment, not the specific line.
 
    + Due to the peculiarities of its parsing behavior, CJD often reports the same 
syntax error twice.
 
    + Certain low-level (i.e., reader) errors cause exceptions that don't report 
the location of the problem.

<!-- xxx -->

* It doesn't cover everything one can do with Clojure (it comes pretty close).

    + CJD clearly accommodates most common syntactic variations (as a point of reference,
it successfully makes it through the Clojure source code), 
but there's been no effort expended to ensure that
coverage is fully comprehensive. This has occasionally given rise to issues in the reader variant
CJD uses for comment parsing.

    + There's no way to accommodate anything that's dynamically or conditionally
compiled: CJD is limited to just _reading_ source files; it doesn't _execute_ them. 
 
And the build process is all over the floor, testing leaves everything to be 
desired, it doesn't result in world peace, and it hasn't freed mankind from hunger, 
disease, or bad guys. Or ants. (That's Leiningen's job.) 
But aside from all these minor details, everything is wonderful! 
 
## License

Copyright (c) 2011-2012 Howard Green. All rights reserved.
            
The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the file epl-v10.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by
the terms of this license.
 
You must not remove this notice, or any other, from this software.