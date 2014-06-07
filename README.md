AAUDOC
======
AAUDOC is a cross-platform LaTeX builder, that automates the process of linking tex documents, running the various compilers over the main document, and removing the temporary documents created by the compilation process.

Also AAUDOC does not force your LaTeX projects to follow any particular structure, instead it is configured by adding a small header to the main tex document, specifying where other tex documents should be linked from and which compilers needs to be run to produce the final document.

Installation
------------
AAUDOC is written in Haskell and uses the Cabal build tool. The simplest way to ensure that all dependencies are installed is to download the [Haskell Platform](http://www.haskell.org/platform/) either from its homepage, or using your favourite package manager.

The package can then be compiled and/or installed by running one of two commands, depending on where the compiled program should be placed:
```
The program can be either compiled and left in a folder dist in the root of the repository.
cabal build

Or compiled and installed into cabals bin directory, which should be in systemm path.
cabal install
```

Configuration
-------------
AAUDOC is configured using some simple annotations in the header of main tex document in a LaTeX project, the one that is being compiled to create the final document. The AAUDOC configuration header must be placed at the top of the main tex document and have no normal LaTeX comments trailing it. Currently, three annotations are supported, each corresponding more or less to one of the programs features.

*Link* searches a directory recursively for documents with either the .tex or .bib suffix and creates an index, with each file included with the appropriate expression. The first argument defines what folder should be searched for .tex and .bib files, while the second argument is the path where the index .tex file should be written.
```
%link directory-with-tex-documet output-path-index.tex
```

*Link-doc* performs nearly identical to *link*, with the small difference that the index is automatically encapsulated in "\begin{document} .. \end{document}". This makes the index easy to include directly in the main .tex document, but at the same time prohibits any non preamble LaTeX commands outside the folder the index is created from.
```
%link-doc directory-with-tex-documet output-path-index.tex
```

*Command* takes a program in the system's path and runs it on the main tex document, for example lualatex, bibtex, etc. Arguments can be entered as a space-separated list after the program name itself.
```
%command program [arguments]
```

*Clean* removes all the temporary files created doing the compilation process of the file containing the annotation. If some of the files need to left alone, a list of specific suffixes, again separated by spaces, that should be deleted by AAUDOC can be specified.
```
%clean [suffixes]
```

Macros
------
Although no particular structure is enforced by AAUDOC, many LaTeX projects follow a similar setup, so an additional annotation "macro" is supported. This annotation does not add additional functionality, but is simply replaced by a set of annotations on runtime, allowing for less configuration for LaTeX projects following a particular structure.

The first macro makes AAUDOC operate like the [(in)famous compile-doc](https://github.com/dhil/compile-doc), "document-name" is a placeholder for the name of the main tex document being compiled.
```
%macro compile-doc
------
%link-doc Documents/"document-name"/ Documents/"document-name"/index.tex
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

The last macro is just compilation linking, compilation, and cleaning. Two compilations are performed to support the use of references between the documents.
```
%macro compile
------
%link Documents/ Documents/index.tex
%command lualatex
%command lualatex
%clean
```

License
-------
The program is licensed under version 3 of the GPL, and a copy of the license is bundled with the program.

