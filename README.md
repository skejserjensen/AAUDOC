AAUDOC
======
AAUDOC is a cross-platform LaTeX builder, that automates the process of linking tex document, running the various compilers over the main document, and removal of temporary documents created by the compilation process.

Also AAUDOC does not force your LaTeX projects to follow any particular structure, instead it is configured by adding a small header to the main tex document, this header specifies where other tex documents should be linked from and what compilers needs to be run to produce the final document.

Installation
------------
AAUDOC is written in Haskell and uses the Cabal build tool. The simplest way to ensure all dependencies are installed is to download the [Haskell Platform](http://www.haskell.org/platform/) either from its homepage, or using your favourite package manager.

The package can then be compiled and/or installed by running one of two commands, depending on where the compiled program should be placed.
```
The program can be either compiled and left in a folder dist in the root of hte repository.
cabal build

Or compiled and installed into cabals bin directory, which should be in systemm path.
cabal install
```

Configuration
-------------
AAUDOC are configured using some simple annotations in the header of main tex document in a LaTeX project, the one that is being compiled to create the final document. The AAUDOC configuration header must be placed at the top of the main tex document and have no normal LaTeX comments trailing it. Currently three annotations are supported, each corresponding more or less to one of the programs features.

Link searches a directory recursively for documents with either the .tex or .bib suffix and create a index, with each file included with the appropriate expression and the entire list of documents enclosed in \begin{document} .. \end{document}, making the index easy to included directly in the main tex document. The first argument defines what folder should be search for .tex and .bib files, while the second argument is the path where the index .tex file should be written.
```
%link directory-with-tex-documet output-path-index.tex
```

Command takes a program in the systems path and runs it on the main tex document, for example lualatex, bibtex, etc. Arguments can be entered as a space separated list after the program name itself.  
```
%command program [arguments]
```

Clean removes all the temporary files created doing the compilation process of the file containing the annotation. If some of the files needs to left alone can a list of specific suffixes, again separated by spaces, AAUDOC should delete be specified.
```
%clean [suffixes]
```

Macros
------
Although no particular structure are enforced by AAUDOC, does many LaTeX projects follow a similar setup, so an additional annotation "macro" are supported. This annotation does not add additional functionality, but is simply replaced by a set of annotation on runtime, allowing for less configuration for LaTeX projects following a particular structure.

The first macro makes AAUDOC operate like the [(in)famous compile-doc](https://github.com/dhil/compile-doc), "document-name" is a place holder for the name of the main tex document being compiled.
```
%macro compile-doc
------
%link Documents/"document-name"/ Documents/"document-name"/index.tex
%command lualatex
%command bibtex
%command lualatex
%command lualatex
%clean
```

The next two macros currently implemented are very similar, and performs linking of the documents in a folder recursively, followed by compilation with either bibtex or makeindex, followed by removal of temporary files.
```
%macro compile-with-bib
------
%link Documents/ Documents/index.tex
%command lualatex
%command bibtex
%command lualatex
%command lualatex
%clean
```
```
%macro compile-with-index
------
%link Documents/ Documents/index.tex
%command lualatex
%command makeindex
%command lualatex
%command lualatex
%clean
```

The last macro is just compilation linking, compilation, and cleaning. Two compilations are performed to support the use of reference between the documents.
````
%macro compile
------
%link Documents/ Documents/index.tex
%command lualatex
%command lualatex
%clean
```

TODO
----
* Make errors in the header of TeX files non terminating, so one file can fail while the rest is OK.
* Go through the program and simplify the code where possible, re-factoring at the same time.
* Investigate how to set the path appropriately to correct "bibtex: (openout_any = p)" errors.
 
License
-------
The program is licensed under version 3 of the GPL, and a copy of the license is bundled with the program.

