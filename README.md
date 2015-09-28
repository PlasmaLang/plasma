# Plasma Language
## *a new programming language*

For a general overview, please visit [http://plasmalang.org/](http://plasmalang.org/)

This is in very early development.

### Dependencies

You will need:

* A C compiler (C11 on a POSIX.2-1992 environment), I've been testing with GCC.  Clang should also work.
* [Mercury](https://www.mercurylang.org/). (The latest stable release does not work, I've been using [rotd-2015-07-01](http://dl.mercurylang.org/rotd/mercury-srcdist-rotd-2015-07-01.tar.gz).)

### Usage

Use ```make``` in the root directory to build the project.

You should get src/pzasm and runtime/pzrun.
These are part of the bytecode interpreter, pzasm will assemble a bytecode
file ```.pz``` from a textual bytecode file ```.pzt```.  pzrun will run a
bytecode program.  There are some example bytecode programs in examples/pzt/
they can be tested using ```make test```.

