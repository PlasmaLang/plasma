# Plasma Runtime System

Plasma uses a byte code interpreter.  One basic interpreter and runtime
system is currently under development but this could change in the future,
including the addition of native code generation.

Some, but not all, of the files here are:

* pz\_interp.h - The header file for the core of the interpreter
* pz\_generic.c - The architecture independent (and only) implementation
                 of the interpreter
* pz\_main.c - The entry point for pzrun
* pz\_instructions.[hc] - Instruction data for the bytecode format
* pz.[hc], pz\_code.[hc], pz\_data.[hc] - Structures used by pzrun
* pz\_format.h - Constants for the PZ bytecode format
* pz\_read.[hc] - Code for reading the PZ bytecode format

