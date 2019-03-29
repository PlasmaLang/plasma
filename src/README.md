# Plasma tools directory

The code in this directory builds the plasmac and pzasm programs.  Plasmac
is the plasma compiler and pzasm will assemble a .pz file from a .pzt
(plasma bytecode text) file.

The bytecode assembler has three stages, parsing the source to an `asm`
structure, assembling this to a `pz` structure, and writing out the `pz`
structure.  Some files/modules are:

* [pzasm.m](pzasm.m) - The plasma bytecode assembler entry point
* [pzt\_parse.m](pzt\_parse.m) - The pzt parser
* asm - These modules contain structures and code used to represent code
        during assembly by pzasm

The compiler parses the code to an `ast` structure, transforms that to the
`core` structure, performs semantic analysis and compilation on the `core`
structure before generating code as a `pz` structure and writing out the
`pz` structure.  This version of the compiler does not perform any
optimisations, most optimisations would be done within the `core` phase.

* [plasmac.m](plasmac.m) - The plasma compiler entry point
* [parse.m](parse.m) - The plasma parser
* [ast.m](ast.m) - The plasma abstract syntax tree
* [pre.m](pre.m) - The pre-core representation
* [pre.from\_ast.m](pre.from\_ast.m) - The translation between the AST and
                                       pre-core representations
* [pre.to\_core.m](pre.to\_core.m) - The translation between the pre-core
                                     and core representations
* [core.m](core.m and sub-modules) - These modules contain the core
                                     structure and code that performs
                                     semantic analysis.
* [core\_to\_pz.m](core\_to\_pz.m and sub-modules) -
  Code to transform `core` to `pz`

Some files/modules shared between several tools are:

* [lex.m](lex.m) -
  This library is part of the Mercury extras distribution and
  provides code to build a lexical analyser
* [parsing.m](parsing.m) - Code to build table based LL(2) parsers
* [pz.m](pz.m and sub-modules) - Code to represent and write out PZ format
  bytecode

