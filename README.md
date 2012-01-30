# CJD

Warning! This project is still under construction---so if things break, are missing in
action, or viciously attack you, well, you've been warned!!

CJD is a system for documenting Clojure programs through the use of structured 
comments embedded in Clojure source code. As such, it's a supplement to or replacement
for the docstring mechanism. 

CJD was inspired by Javadoc, to which it bears superficial similarity. Like Javadoc,
CJD-comments support a simple form of markup. This allows CJD's processing facility
to extract the documentation content from collections of Clojure namespaces and convert 
it into trees of consistently-formatted HTML documents. 

For a not-terribly-reverent introduction to CJD, 
[try the FAQ](http://greenh.github.com/CJD/doc/faq.htm).

If you'd like to see what it looks like in the real world, the CJD source is 
itself extensively commented using CJD. For a representative sample, try
[the generated output](http://greenh.github.com/CJD/doc/dark/cjd.exome.html) and
[the source code](https://github.com/greenh/CJD/blob/master/src/clojure/cjd/exome.clj) 
from which it was generated. (The entire CJD HTML tree is online in both 
[dark-background](http://greenh.github.com/CJD/doc/dark/index.html) and
[light-background](http://greenh.github.com/CJD/doc/light/index.html) renderings. 
Be aware, however, that this tree is mostly oriented towards "internal" documentation
of CJD itself---so if it looks obscure, well, it is!)

## Documentation
Several documents are in the making:

* A [user guide](http://greenh.github.com/CJD/doc/User.html), which attempts to explain 
CJD in depth.

* A [FAQ](http://greenh.github.com/CJD/doc/FAQ.html), which discusses a number of issues
about what CJD is and does, and how it managed to get there.

* A [guide to extending CJD](http://greenh.github.com/CJD/doc/Extension.html). Clojure 
extensible, so it follows that a documentation technology for Clojure needs to be 
extensible, too, right?


## License

Copyright (c) 2011-2012 Howard Green. All rights reserved.
            
The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the file epl-v10.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by
the terms of this license.
 
You must not remove this notice, or any other, from this software.