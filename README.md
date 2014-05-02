AAUDOC
======
Cross-platform implementation of the (in)famous compile-doc for collaborative LaTeX document compilation.

NOTE: this is a development repository and not intended for use by external parties.    

TODO
----
* Addition of user defined header Macros, so Jobs could be bundled.
* Pretty printing of output from commands and parsing of it for errors.
```
src/Main.hs:34:13: Warning: Defined but not used: `jobOutput'
```
* Refactor the program into multiple smaller modules with a distinct purpose.
* Go through the program and simplify the code where possible, refactoring at the same time.
* Investigate how to set the path appropriately to correct "bibtex: (openout_any = p)" errors.
