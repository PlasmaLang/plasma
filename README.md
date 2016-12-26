# Plasma Language
## *a new programming language*

For a general overview, please visit
[http://plasmalang.org/](http://plasmalang.org/)

This is in very early development.

### Dependencies

You will need:

* A C compiler (C99 on a POSIX.1-2008 environment), I've been testing with
  GCC.  Clang should also work.
* [Mercury](https://www.mercurylang.org/).  I attempt to maintain
  compatibility with 14.01.1 until installation issues on OS X are resolved
  in the newer builds.
* Asciidoc

### Usage

Use ```make``` in the root directory to build the project.

You should get:

* src/plasmac - The plasma compiler, compiles plasma (```.p```) files to
  plasma bytecode (```.pz```)
* runtime/pzrun - The runtime system, executes plasma bytecode (```.pz```)
  files.
* src/pzasm - The plasma bytecode assembler.  This compiles textual bytecode
  (```.pzt```) to bytecode (```.pz```).  It is useful for testing the
  runtime.

There are example plasma programs in ```examples/```.  Running ```make
test``` will execute these programs as part of the test suite to ensure that
things are working correctly.

### Layout

* docs/ - Documentation
* examples/ - Example Plasma programs
* runtime/ - Runtime system (C code)
* src/ - The compiler and other tools
* tests/ - The test suite (in addition to some of the files in examples/)

### Contributing

Please see [CONTRIBUTING.md](CONTRIBUTING.md)

