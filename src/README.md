# Plasma tools directory

The code in this directory builds the plasmac and pzasm programs.  Plasmac
is the plasma compiler and pzasm will assemble a .pz file from a .pzt
(plasma bytecode text) file.

The bytecode assembler has three stages, parsing the source to an `asm`
structure, assembling this to a `pz` structure, and writing out the `pz`
structure.  Some files/modules are:

* pzasm.m - The plasma bytecode assembler entry point
* pzt_parse - The pzt parser
* asm - These modules contain structures and code used to represent code
        during assembly by pzasm

The compiler parses the code to an `ast` structure, transforms that to the
`core` structure, performs semantic analysis and compilation on the `core`
structure before generating code as a `pz` structure and writing out the
`pz` structure.  This version of the compiler does not perform any
optimisations, most optimisations would be done within the `core` phase.

* plasmac.m - The plasma compiler entry point
* parse - The plasma parser
* ast.m - The plasma abstract syntax tree
* ast_to_core.m - Code to transform `ast` to `core`
* core - These modules contain the core structure and code that performs
         semantic analysis.
* core_to_pz.m - Code to transform `core` to `pz`

Some files/modules shared between several tools are:

* lex - This liibrary is part of the Mercury extras distribution and provides
        code to build a lexical analyser
* parsing - Code to build table based LL(2) parsers
* pz - Code to represent and write out PZ format bytecode

