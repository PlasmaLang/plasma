# Plasma Runtime System

Plasma uses a byte code interpreter.  One basic interpreter and runtime
system is currently under development but this could change in the future,
including the addition of native code generation.

Some, but not all, of the files here are:

* [pz\_interp.h](pz\_interp.h) - The header file for the core of the
                                 interpreter
* [pz\_generic.c](pz\_generic.c) - The architecture independent (and only)
                                   implementation of the interpreter
* pz\_generic\_\*.[ch] - Other parts of the generic interpreter.  Only files
                         in this group may include other headers in this
                         group, there must be no coupling with the rest of
                         the system other than trhough pz_interp.h
* [pz\_main.c](pz\_main.c) - The entry point for pzrun
* [pz\_instructions.h](pz\_instructions.h) and
  [pz\_instructions.c](pz\_instructions.c)
  Instruction data for the bytecode format
* [pz.h](pz.h)/[pz.c](pz.c),
  [pz\_code.h](pz\_code.h)/[pz\_code.c](pz\_code.c) and
  [pz\_data.h](pz\_data.h)/[pz\_data.c](pz\_data.c) -
  Structures used by pzrun
* [pz\_format.h](pz\_format.h) - Constants for the PZ bytecode format
* [pz\_read.h](pz\_read.h)/[pz\_read.c](pz\_read.c) - 
  Code for reading the PZ bytecode format

