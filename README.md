# Plasma Language
## *a new programming language*

For a general overview, please visit
[http://plasmalang.org/](http://plasmalang.org/)

This is in very early development.

### Dependencies

You will need:

* A C compiler (C99 on a POSIX.2-1992 environment), I've been testing with GCC.
  Clang should also work.
* [Mercury](https://www.mercurylang.org/). 14.01  I've also been using various
  ROTD builds.
* Asciidoc

### Usage

Use ```make``` in the root directory to build the project.

You should get:

* src/plasmac - The plasma compiler, compiles plasma (```.p```) files to
  plasma bytecode (```.pz```)
* runtime/pzrun - The runtime system, esecutes plasma bytecode (```.pz```)
  files.
* src/pzasm - The plasma bytecode assembler.  This compiles textual bytecode
  (```.pzt```) to bytecode (```.pz```).  It is useful for testing the
  runtime.

There is an example plasma program in ```examples/p/``` and there are some
example bytecode programs in ```examples/pzt/```.  Running ```make test```
will execute these programs to test that things are working.

