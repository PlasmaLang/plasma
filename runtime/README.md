# Plasma Runtime System

Plasma uses a byte code interpreter.  One basic interpreter and runtime
system is currently under development but this could change in the future,
including the addition of native code generation.

Some, but not all, of the files here are:

* pz_run.h - The header file for the core of the interpreter
* pz_run_generic.c - The architecture independent (and only) implementation
                     of the interpreter
* pz_main.c - The entry point for pzrun
* pz_instructions.[hc] - Instruction data for the bytecode format
* pz.[hc], pz_code.[hc], pz_data.[hc] - Structures used by pz_run
* pz_format.h - Constants for the PZ bytecode format
* pz_read.[hc] - Code for reading the PZ bytecode format

